{- |
Module      : Scad
Description : Transforms geometry data into openSCAD commands
Copyright   : (c) Zachary Sarver, 2021
License     : GPL-3
Maintainer  : Zachary.Sarver@gmail.com

Transforms geometry data into a string of openSCAD commands.
-}
module Scad
  ( scadify
  , scadifyGeometry
  ) where

import Geometry (Geometry(..))

-- | One to one mapping of geometry data to SCAD code. E.g. Cube w d h maps to
-- "cube([w,d,h]);"
scadifyGeometry :: Geometry -> String
scadifyGeometry (Translate x y z g) = "translate([" <> show x
                              <> "," <> show y
                              <> "," <> show z
                              <> "]) \n" <> scadifyGeometry g <> "\n"
scadifyGeometry (Cube width depth height) = "cube([" <> show width
                                    <> "," <> show depth
                                    <> "," <> show height
                                    <> "]);"
scadifyGeometry Empty = ""

-- | Transforms a list of Geometry using scadify, wrapping all the scadified
-- Geometry in a union() block
scadify :: [Geometry] -> String
scadify image = "union() {\n" <> scadify' <> "};"
  where scadify' = foldr ((<>) . scadifyGeometry) "" image
