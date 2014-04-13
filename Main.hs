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

main = do
  getArgsAndInitialize
  tex <- loadTexture RGB8 "texs/myPicture.jpg" :: IO (Texture2D RGBFormat)
  env <- loadTexture RGB8 "envs/myEnv.jpg" :: IO (Texture2D RGBFormat)
  angleRef <- newIORef 0.0
  obj <- getContents >>= (getObj . objToGPU)
  newWindow "Spinning box" (100:.100:.()) (800:.600:.()) 
       (renderFrame tex env angleRef obj)
       initWindow
  mainLoop
      where getObj (Left obj) = return $ toGPUStream TriangleList obj
            getObj (Right s) = print s >> exitFailure

renderFrame tex env angleRef obj size = readIORef angleRef >>= nextFrame
    where nextFrame angle = writeIORef angleRef ((angle + 0.05) `mod'` (2*pi))
                            >> return (objFrameBuffer tex env angle size obj)
initWindow win = idleCallback $= Just (postRedisplay (Just win))

transformedObj :: Float -> Vec2 Int -> TriangleStream3 -> TriangleStream4
transformedObj angle size = fmap $ transform angle size

rasterizedObj angle size = rasterizeFront . transformedObj angle size

litObj tex env angle size obj = enlight tex env <$> rasterizedObj angle size obj

objFrameBuffer tex env angle size obj = paintSolid (litObj tex env angle size obj) emptyFrameBuffer

enlight tex env (norm, uv) = RGB $ c * Vec.vec (ambient + specular + diffuse) + c2*Vec.vec (specular + diffuse/10)
    where RGB c = sample (Sampler Linear Mirror) tex uv
          RGB c2 = sample (Sampler Linear Clamp) env (x:.y:.())
                   where
                     (x:.y:._:.()) = (toGPU 0.5)*(-norm + (Vec.vec 1))
          light = toGPU 0:.0:.1:.()
          view = toGPU 0:.0:.1:.()
          diffuse = norm `dot` light
          ambient = toGPU 0.1
          specular = (view `dot` r) ** n
              where r = (Vec.vec (2* (norm `dot` light))) * norm - light
                    n = 10

transform angle (width:.height:.()) (pos, norm, uv) = (transformedPos, (transformedNorm, uv))
    where
        modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
        viewMat = translation (-(0:.0:.5:.()))
        projMat = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewProjMat = projMat `multmm` viewMat
        transformedPos = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
        transformedNorm = toGPU (Vec.map (Vec.take n3) $ Vec.take n3 modelMat) `multmv` norm

paintSolid = paintColorRastDepth Lequal True NoBlending (RGB $ Vec.vec True)
emptyFrameBuffer = newFrameBufferColorDepth (RGB 0) 32
