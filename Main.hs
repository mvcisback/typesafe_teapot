module Main where

import Graphics.GPipe
import Graphics.GPipe.Texture.Load
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Monoid
import Data.IORef
import Control.Applicative
import Control.Monad
import Obj
import System.IO    
import System.Exit    
import Graphics.UI.GLUT
    (Window,
    mainLoop,
    postRedisplay,
    idleCallback,
    getArgsAndInitialize,
    ($=))

type TriangleStream3 = PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
type TriangleStream4 = PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))

main :: IO ()
main = do
  getArgsAndInitialize
  tex <- loadTexture RGB8 "myPicture.jpg"
  angleRef <- newIORef 0.0
  cubeObj <- getContents
  cube <- (getCube . objToGPU) cubeObj
  let cube' = toGPUStream TriangleList cube
  newWindow "Spinning box" (100:.100:.()) (800:.600:.()) 
       (renderFrame tex angleRef cube')
       initWindow
  mainLoop
      where getCube (Left cube) = return cube
            getCube (Right s) = print s >> exitFailure

renderFrame :: Texture2D RGBFormat -> IORef Float -> TriangleStream3 -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame tex angleRef obj size = readIORef angleRef >>= nextFrame
    where nextFrame angle = writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
                            >> return (objFrameBuffer tex angle size obj)
initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win))

transformedObj :: Float -> Vec2 Int -> TriangleStream3 -> TriangleStream4
transformedObj angle size = fmap (transform angle size)

rasterizedObj angle size = rasterizeFront . transformedObj angle size

litObj tex angle size obj = enlight tex <$> rasterizedObj angle size obj

objFrameBuffer tex angle size obj = paintSolid (litObj tex angle size obj) emptyFrameBuffer

enlight tex (norm, uv) = RGB (c * Vec.vec (norm `dot` toGPU (0:.0:.1:.())))
    where RGB c = sample (Sampler Linear Wrap) tex uv

transform angle (width:.height:.()) (pos, norm, uv) = (transformedPos, (transformedNorm, uv))
    where
        modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
        viewMat = translation (-(0:.0:.5:.()))
        projMat = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewProjMat = projMat `multmm` viewMat
        transformedPos = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
        transformedNorm = toGPU (Vec.map (Vec.take n3) $ Vec.take n3 modelMat) `multmv` norm

paintSolid = paintColor NoBlending (RGB $ Vec.vec True)
emptyFrameBuffer = newFrameBufferColor (RGB 0)
