module Render (render) where

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

render texs envs bumps lightingFlag = do
  getArgsAndInitialize
  tex <- loadTexture RGB8 texs :: IO (Texture2D RGBFormat)
  env <- loadTexture RGB8 envs :: IO (Texture2D RGBFormat)
  bumps <- loadTexture RGB8 bumps :: IO (Texture2D RGBFormat)
  angleRef <- newIORef 0.0
  obj <- getContents >>= (getObj . objToGPU)
  newWindow "Spinning box" (100:.100:.()) (800:.600:.())
       (renderFrame (tex,env,bumps) lighting angleRef obj)
       initWindow
  mainLoop
      where getObj (Left obj) = return $ toGPUStream TriangleList obj
            getObj (Right s) = print s >> exitFailure
            lighting = if lightingFlag then seeliger else phong

renderFrame texs lighting angleRef obj size = readIORef angleRef >>= nextFrame
    where nextFrame angle = writeIORef angleRef ((angle + 0.005) `mod'` (2*pi))
                            >> return (objFrameBuffer (enlight lighting) texs angle size obj)
initWindow win = idleCallback $= Just (postRedisplay (Just win))

type TriangleStream = PrimitiveStream Triangle (Vec3 (Vertex Float), Vec3 (Vertex Float), Vec2 (Vertex Float))
type TriangleStream' = PrimitiveStream Triangle (Vec4 (Vertex Float),(Vec3 (Vertex Float),Vec2 (Vertex Float)))
transformedObj :: Float -> Vec2 Int -> TriangleStream -> TriangleStream'
transformedObj angle size = fmap $ transform angle size

rasterizedObj angle size = rasterizeFront . transformedObj angle size
litObj enlight texs angle size obj = enlight texs <$> rasterizedObj angle size obj
objFrameBuffer enlight texs angle size obj = paintSolid (litObj enlight texs angle size obj) emptyFrameBuffer

-- This light source is at oo
light = toGPU $ normalize (1:.1:.1:.())
view = toGPU 0:.0:.1:.()

phong norm (texColor,envColor) = RGB $ (texColor+envColor)*Vec.vec (specular + ambient + diffuse)
    where diffuse = maxB (norm `dot` light) 0
          ambient = toGPU 0.1
          specular = ifB (proj >* 0) (abs $ view `dot` r ** n) 0
              where r = Vec.vec (2* proj) * norm - light
                    proj = norm `dot` light
                    n = 40

seeliger norm (color,_) = RGB $ color * Vec.vec (ifB (s ==* 0 &&* t ==* 0) 0 (s / (s + t)))
    where s = maxB (norm `dot` light) 0
          t = maxB (norm `dot` view) 0

bumpedNormal bumps norm@(nx:.ny:.nz:.()) uv@(u:.v:.()) =
    normalize $ norm - (dBdu * norm) `cross` dSdv + (dBdv * norm) `cross` dSdu
    where
          bumpColor = sample (Sampler Linear Wrap) bumps
          diffS dFda = dFda nx :. dFda ny :. dFda nz :.()
          diffB u2 v2 =  (b2 - b1) * Vec.vec (1/h)
              where RGB b1 = bumpColor $ uv
                    RGB b2 = bumpColor $ u2:.v2:.()
          dBdu = diffB (u+h) v
          dBdv = diffB u (v+h)
          dSdv = diffS dFdx
          dSdu = diffS dFdy
          h = 0.01

paintSolid = paintColorRastDepth Lequal True NoBlending (RGB $ Vec.vec True)
emptyFrameBuffer = newFrameBufferColorDepth (RGB 0) 32

enlight lighting (tex,env,bumps) (norm,uv) = lighting norm' (texColor,envColor)
    where color = texColor + envColor * Vec.vec 0.5
          RGB texColor = sample (Sampler Linear Mirror) tex uv
          RGB envColor = sample (Sampler Linear Clamp) env (x:.y:.())
              where (x:.y:._:.()) = toGPU 0.5*(Vec.vec 1 - norm')
          norm' = bumpedNormal bumps norm uv

transform angle (width:.height:.()) (pos, norm, uv) = (transformedPos, (transformedNorm, uv))
    where
        modelMat = rotationVec (normalize (1:.0.5:.0.3:.())) angle `multmm` translation (-0.5)
        viewMat = translation (-(0:.0:.5:.()))
        projMat = perspective 1 100 (pi/3) (fromIntegral width / fromIntegral height)
        viewProjMat = projMat `multmm` viewMat
        transformedPos = toGPU (viewProjMat `multmm` modelMat) `multmv` (homPoint pos :: Vec4 (Vertex Float))
        transformedNorm = toGPU (Vec.map (Vec.take n3) $ Vec.take n3 modelMat) `multmv` norm
