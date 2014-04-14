module Obj(objToGPU) where

import Parser
import Graphics.GPipe
import qualified Graphics.GPipe as G
import Data.Vec.LinAlg
import qualified Data.Vec as V
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

getCoords verts (f1, f2, f3) = (getVert' f1, getVert' f2, getVert' f3)
    where getVert' (faceI,_,_)= verts !! faceI

computeNormal :: (Vec3 Float, Vec3 Float, Vec3 Float) -> Vec3 Float
computeNormal (v1, v2, v3) = normalize $ (v2-v1) `cross` (v3-v2)

computeNormals = concatMap (replicate 3 . computeNormal)
faceToNormal verts = computeNormal . getCoords verts
facesToNormal verts faces = weight * total
    where total = sum $ map (faceToNormal verts) faces
          weight = 1 / fromIntegral (length faces)

mapNormals verts faces = M.map (facesToNormal verts) (mapFaces faces)
mapFaces = foldr mapFace M.empty
mapFace f@(f1,f2,f3) m = Data.List.foldr (insertFace f) m [f1,f2,f3]
insertFace f (f1,_,_) m = M.insert f1 new m
    where new = case M.lookup f1 m of
                  Just old -> f:old
                  Nothing -> [f]

type Tri = (Coord,Coord,Coord)
facesToTriangles :: [Face] -> [Tri]
facesToTriangles = concatMap faceToTriangles
faceToTriangles :: Face -> [Tri]
faceToTriangles (c1,c2,c3,lis) = faceToTriangles' c1 c2 (c3:lis)
    where faceToTriangles' a1 a2 [] = []
          faceToTriangles' a1 a2 (a3:t) = (a1,a2,a3) : faceToTriangles' a1 a3 t

mapCoords (vs, ts, ns, tris) = map toCoord indexes
    where indexes = concatMap (\(f1,f2,f3) -> [f1,f2,f3]) tris
          toCoord (vertI,texI,normI) = (vs !! vertI,getNormal vertI normI,getTexture vertI texI)
          getNormal _ (Just i) = ns !! i
          getNormal i _ = fromMaybe (0:.0:.0:.()) $ M.lookup i normals
          getTexture _ (Just i) = ts !! i
          getTexture i _ = u:.v:.()
              where (x:.y:.z:.()) = vs !! i
                    u = atan2 y x 
                    v = z
          normals = mapNormals vs tris

type GVertex = Vec3 (G.Vertex Float)
type GFace = Vec3 (G.Vertex Float)
type GTexture = Vec2 (G.Vertex Float)
objToGPU:: String -> Either [CPU (GVertex, GFace, GTexture)] String
objToGPU s = case parse s of
               Ok (vs,ts,ns,fs) -> Left $ mapCoords (vs,ts,ns,tris) 
                   where tris = facesToTriangles fs
               Failed s -> Right s
