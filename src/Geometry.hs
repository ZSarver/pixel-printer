module Geometry
  (
    Geometry(..)
  ) where

data Geometry = Transform Int Int Int Geometry | Cube Int Int Int | Empty

makeCube :: Int -> Int -> Int -> Geometry
makeCube width depth height = Cube width depth height

transform :: Int -> Int -> Int -> Geometry -> Geometry
transform x y z g = Transform x y z g
