module Obj(objToGPU) where

import Parser
import Graphics.GPipe
import qualified Graphics.GPipe as G
import Data.Vec.LinAlg
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

getCoords verts (Face f1 f2 f3) = (getVert' f1, getVert' f2, getVert' f3)
    where getVert' = getVert verts
getVert verts f = let (Vertex v1 v2 v3) =  verts !! f in v1:.v2:.v3:.()

computeNormal :: (Vec3 Float, Vec3 Float, Vec3 Float) -> Vec3 Float
computeNormal (v1, v2, v3) = normalize $ (v2-v1) `cross` (v3-v2)

computeNormals = concatMap (replicate 3 . computeNormal)
faceToNormal verts = computeNormal . (getCoords verts)
facesToNormal verts faces = weight * total
    where total = sum $ map (faceToNormal verts) faces
          weight = 1 / (fromIntegral $ length faces)


mapNormals verts faces = M.map (facesToNormal verts) (mapFaces faces)
mapFaces = foldr mapFace M.empty
mapFace f@(Face f1 f2 f3) m = Data.List.foldr (insertFace f) m [f1,f2,f3]
insertFace f f1 m = M.insert f1 new m
    where new = case M.lookup f1 m of
                  Just old -> f:old
                  Nothing -> [f]

mapCoords vs fs = map toCoord indexes
    where indexes = concatMap (\(Face f1 f2 f3) -> [f1,f2,f3]) fs
          toCoord i = (getVert vs i,getNormal i,0:.0:.())
          getNormal i = fromMaybe (0:.0:.0:.()) $ M.lookup i normals
          normals = mapNormals vs fs

flatten = foldr (\(a1,a2,a3) as -> a1:a2:a3:as) []
objToGPU:: String -> Either [CPU (Vec3 (G.Vertex Float), Vec3 (G.Vertex Float), Vec2 (G.Vertex Float))] String
objToGPU s = case parse s of 
              Ok (vs, [], [], fs) -> Left $ mapCoords vs fs
              Ok _ -> undefined
              Failed s -> Right s
