module Main where

import Graphics.GPipe
import Graphics.GPipe.Texture.Load
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Monoid
import Data.IORef
import Control.Applicative
import Obj
import Graphics.UI.GLUT
    (Window,
    mainLoop,
    postRedisplay,
    idleCallback,
    getArgsAndInitialize,
    ($=))

main :: IO ()
main = do
  getArgsAndInitialize
  tex <- loadTexture RGB8 "myPicture.jpg"
  angleRef <- newIORef 0.0
  newWindow "Spinning box" (100:.100:.()) (800:.600:.()) (renderFrame tex angleRef) initWindow
  mainLoop       

renderFrame :: Texture2D RGBFormat -> IORef Float -> Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame tex angleRef size = do
    angle <- readIORef angleRef
    writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
    return $ objFrameBuffer tex angle size cube

initWindow :: Window -> IO ()
initWindow win = idleCallback $= Just (postRedisplay (Just win))

type TriangleStream3 = PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
type TriangleStream4 = PrimitiveStream Triangle (Vec4 (Vertex Float), (Vec3 (Vertex Float), Vec2 (Vertex Float)))

transformedObj :: Float -> Vec2 Int -> TriangleStream3 -> TriangleStream4
transformedObj angle size = fmap (transform angle size)

rasterizedObj :: Float -> Vec2 Int -> TriangleStream3 -> FragmentStream (Vec3 (Fragment Float), Vec2 (Fragment Float))
rasterizedObj angle size = rasterizeFront . (transformedObj angle size)

litObj :: Texture2D RGBFormat -> Float -> Vec2 Int -> TriangleStream3 -> FragmentStream (Color RGBFormat (Fragment Float))
litObj tex angle size obj = enlight tex <$> rasterizedObj angle size obj

objFrameBuffer :: Texture2D RGBFormat -> Float -> Vec2 Int -> TriangleStream3 -> FrameBuffer RGBFormat () ()
objFrameBuffer tex angle size obj = paintSolid (litObj tex angle size obj) emptyFrameBuffer

enlight tex (norm, uv) = RGB (c * Vec.vec (norm `dot` toGPU (0:.0:.1:.())))
    where RGB c = sample (Sampler Linear Wrap) tex uv

transform angle (width:.height:.()) (pos, norm, uv) = (transformedPos, (transformedNorm, uv))
    where
        modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
        viewMat = translation (-(0:.0:.2:.())) 
        projMat = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewProjMat = projMat `multmm` viewMat
        transformedPos = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
        transformedNorm = toGPU (Vec.map (Vec.take n3) $ Vec.take n3 modelMat) `multmv` norm

paintSolid = paintColor NoBlending (RGB $ Vec.vec True)
emptyFrameBuffer = newFrameBufferColor (RGB 0)


uvCoords = [0:.0:.(), 0:.1:.(), 1:.0:.(), 1:.1:.()]
sidePosX = toGPUStream TriangleStrip $ zip3 [1:.0:.0:.(), 1:.1:.0:.(), 1:.0:.1:.(), 1:.1:.1:.()] (repeat (1:.0:.0:.()))    uvCoords
sideNegX = toGPUStream TriangleStrip $ zip3 [0:.0:.1:.(), 0:.1:.1:.(), 0:.0:.0:.(), 0:.1:.0:.()] (repeat ((-1):.0:.0:.())) uvCoords
sidePosY = toGPUStream TriangleStrip $ zip3 [0:.1:.1:.(), 1:.1:.1:.(), 0:.1:.0:.(), 1:.1:.0:.()] (repeat (0:.1:.0:.()))    uvCoords
sideNegY = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 1:.0:.0:.(), 0:.0:.1:.(), 1:.0:.1:.()] (repeat (0:.(-1):.0:.())) uvCoords
sidePosZ = toGPUStream TriangleStrip $ zip3 [1:.0:.1:.(), 1:.1:.1:.(), 0:.0:.1:.(), 0:.1:.1:.()] (repeat (0:.0:.1:.()))    uvCoords
sideNegZ = toGPUStream TriangleStrip $ zip3 [0:.0:.0:.(), 0:.1:.0:.(), 1:.0:.0:.(), 1:.1:.0:.()] (repeat (0:.0:.(-1):.())) uvCoords

cube = mconcat [sidePosX, sideNegX, sidePosY, sideNegY, sidePosZ, sideNegZ]
