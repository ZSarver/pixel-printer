module Scad
  ( scadify
  ) where

import Geometry (Geometry(..))

scadifyGeometry :: Geometry -> String
scadifyGeometry (Transform x y z g) = "translate([" <> show x
                              <> "," <> show y
                              <> "," <> show z
                              <> "]) \n" <> scadifyGeometry g <> "\n"
scadifyGeometry (Cube width depth height) = "cube([" <> show width
                                    <> "," <> show depth
                                    <> "," <> show height
                                    <> "]);"
scadifyGeometry Empty = ""

scadify :: [Geometry] -> String
scadify image = "union() {\n" <> scadify' <> "};"
  where scadify' = (foldr (<>) "") . (map scadifyGeometry) $ image
