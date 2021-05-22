module Stl 
 ( putSTL
 , Triangle (..)
 , Vec3 (..)
 , writeTriangles
 ) where
import qualified Data.Binary.Builder as B

import Data.Binary.Builder (Builder)

import qualified Data.Binary.Put as P
import Data.Binary.Put (Put)
import Data.Binary (encodeFile)
import Control.Monad (replicateM_, forM_)
import GHC.Float (double2Float)


data Vec3 = Vec3 Double Double Double
data Triangle = Tri Vec3 Vec3 Vec3

minus :: Vec3 -> Vec3 -> Vec3
minus (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x1 - y1) (x2 - y2) (x3 - y3)

plus :: Vec3 -> Vec3 -> Vec3
plus (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x1 + y1) (x2 + y2) (x3 + y3)

normal :: Triangle -> Vec3
normal (Tri a b c) = cross (b `minus` a) (c `minus` a)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) = Vec3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

putHeader :: Put
putHeader = replicateM_ 80 (P.putWord8 0)

putSTL :: [Triangle] -> Put
putSTL triangles = do
  putHeader
  let numTriangles = fromIntegral (length triangles)
  P.putWord32le numTriangles
  forM_ triangles putTriangle 
  
putTriangle :: Triangle -> Put
putTriangle t@(Tri p q r) = do
 putVector (normal t)
 putVector p
 putVector q
 putVector r
 P.putWord16le 0

putVector :: Vec3 -> Put
putVector (Vec3 x y z) = do
  P.putFloatle (double2Float x)
  P.putFloatle (double2Float y)
  P.putFloatle (double2Float z)

writeTriangles :: [Triangle] -> FilePath -> IO ()
writeTriangles triangles path = encodeFile path (P.runPut (putSTL triangles))
