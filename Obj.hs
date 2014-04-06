module Obj where

import Parser
import Graphics.GPipe
import Data.Vec.LinAlg
import Data.List

processObj = foldr processExp ([], [], [])
    where processExp v@(Vertex _ _ _) (vs, fs, os) = (vs++[v], fs, os)
          processExp f@(Face _ _ _) (vs, fs, os) = (vs, fs++[f], os)
          processExp o (vs, fs, os) = (vs, fs, os++[o])

toTriangles (verts, faces, _) = map getCoords faces
    where getCoords (Face f1 f2 f3) = (getVert f1, getVert f2, getVert f3)
          getVert f = let (Vertex v1 v2 v3) =  verts !! f in v1:.v2:.v3:.()

computeNormal :: (Vec3 Float, Vec3 Float, Vec3 Float) -> Vec3 Float
computeNormal (v1, v2, v3) = normalize $ v3-v2 `cross` v2-v1

computeNormals = map computeNormal

fromObj s = case parse s of 
              Ok eqs -> Left (toTriangles . processObj $ eqs)
              Failed s -> Right s
              
