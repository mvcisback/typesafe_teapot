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
    (mainLoop,
    postRedisplay,
    idleCallback,
    getArgsAndInitialize,
    ($=))

main = do
  getArgsAndInitialize
  tex <- loadTexture RGB8 "texs/myPicture.jpg" :: IO (Texture2D RGBFormat)
  env <- loadTexture RGB8 "envs/myEnv.jpg" :: IO (Texture2D RGBFormat)
  angleRef <- newIORef 0.0
  obj <- getContents >>= (getObj . objToGPU)
  newWindow "Spinning box" (100:.100:.()) (800:.600:.()) 
       (renderFrame (tex,env) angleRef obj)
       initWindow
  mainLoop
      where getObj (Left obj) = return $ toGPUStream TriangleList obj
            getObj (Right s) = print s >> exitFailure

renderFrame texs angleRef obj size = readIORef angleRef >>= nextFrame
    where nextFrame angle = writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
                            >> return (objFrameBuffer texs angle size obj)
initWindow win = idleCallback $= Just (postRedisplay (Just win))

type TriangleStream = PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
type TriangleStream' = PrimitiveStream Triangle (Vec4 (Vertex Float),(Vec3 (Vertex Float),Vec2 (Vertex Float)))
transformedObj :: Float -> Vec2 Int -> TriangleStream -> TriangleStream'
transformedObj angle size = fmap $ transform angle size

rasterizedObj angle size = rasterizeFront . transformedObj angle size
litObj texs angle size obj = enlight texs <$> rasterizedObj angle size obj
objFrameBuffer texs angle size obj = paintSolid (litObj texs angle size obj) emptyFrameBuffer

-- This light source is at oo
light = toGPU $ normalize (1:.1:.1:.())
view = toGPU 0:.0:.1:.()

phong norm = specular + ambient + diffuse
    where diffuse = norm `dot` light
          ambient = toGPU 0.1
          specular = ifB (proj >* 0) ((abs $ view `dot` r) ** n) 0
              where r = Vec.vec (2* proj) * norm - light
                    proj = norm `dot` light
                    n = 40

seeliger norm = ifB (s ==* 0 &&* t ==* 0) 0 (s / (s + t))
    where s = maxB (norm `dot` light) 0
          t = maxB (norm `dot` view) 0

enlight (tex, env) (norm,uv) = RGB $ color * Vec.vec (phong norm)
    where color = texColor + envColor * Vec.vec 0.5
          RGB texColor = sample (Sampler Linear Mirror) tex uv
          RGB envColor = sample (Sampler Linear Clamp) env (x:.y:.())
              where (x:.y:._:.()) = toGPU 0.5*(Vec.vec 1 - norm)

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
