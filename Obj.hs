module Obj where

import Parser
import Graphics.GPipe
import qualified Graphics.GPipe as G
import Data.Vec.LinAlg
import Data.List

toTriangles verts = concat . (map getCoords)
    where getCoords (Face f1 f2 f3) = [getVert f1, getVert f2, getVert f3]
          getVert f = let (Vertex v1 v2 v3) =  verts !! f in v1:.v2:.v3:.()

computeNormal :: (Vec3 Float, Vec3 Float, Vec3 Float) -> Vec3 Float
computeNormal (v1, v2, v3) = normalize $ v3-v2 `cross` v2-v1

computeNormals = map computeNormal

objToGPU:: String -> Either [CPU (Vec3 (G.Vertex Float), Vec3 (G.Vertex Float), Vec2 (G.Vertex Float))] String
objToGPU s = case parse s of 
              Ok (vs, ts, ns, fs) -> Left $ zip3 vs' ns' ts'
                  where normal2Vec (Normal a b c) = a:.b:.c:.()
                        texture2Vec (Texture a b) = a:.b:.()
                        vs' = toTriangles vs fs
                        ns' = map (normal2Vec) ns
                        ts' = map (texture2Vec) ts
              Failed s -> Right s
