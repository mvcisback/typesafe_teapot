module Obj where

import Parser
import Graphics.GPipe
import Data.Vec.LinAlg

processObj = (span isVertex) . onlyFaceVertex
             where onlyFaceVertex = filter (\x -> isFace x || isVertex x)

isFace (Face _ _ _) = True
isFace _ = False

isVertex (Vertex _ _ _) = True
isVertex _ = False

toTriangles (verts, faces) = map getCoords faces
    where getCoords (Face f1 f2 f3) = (getVert f1, getVert f2, getVert f3)
          getVert f = let (Vertex v1 v2 v3) =  verts !! f in v1:.v2:.v3:.()

computeNormal (v1, v2, v3) = normalize $ (v3-v2) `cross` (v2-v1)
computeNormals [(v1, v2, v3)] = map computeNormal
