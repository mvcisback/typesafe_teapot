module Obj where

import Parser
import Graphics.GPipe
import qualified Graphics.GPipe as G
import Data.Vec.LinAlg
import Data.List

toTriangles verts = map getCoords
    where getCoords (Face f1 f2 f3) = (getVert f1, getVert f2, getVert f3)
          getVert f = let (Vertex v1 v2 v3) =  verts !! f in v1:.v2:.v3:.()

computeNormal :: (Vec3 Float, Vec3 Float, Vec3 Float) -> Vec3 Float
computeNormal (v1, v2, v3) = normalize $ (v2-v1) `cross` (v3-v2)

computeNormals = concat . (map (replicate 3 . computeNormal))

flatten = foldr (\(a1,a2,a3) as -> a1:a2:a3:as) []

objToGPU:: String -> Either [CPU (Vec3 (G.Vertex Float), Vec3 (G.Vertex Float), Vec2 (G.Vertex Float))] String
objToGPU s = case parse s of 
              Ok (vs, ts, ns, fs) -> Left $ zip3 tris ns' ts'
                  where normal2Vec (Normal a b c) = a:.b:.c:.()
                        texture2Vec (Texture a b) = a:.b:.()
                        fs' = toTriangles vs fs
                        tris = flatten fs'
                        ns' = computeNormals fs'
                        ts' = repeat (0:.0:.())
              Failed s -> Right s
