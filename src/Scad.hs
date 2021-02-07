module Scad
  ( scadify
  ) where

import Geometry (Geometry(..))

scadify :: Geometry -> String
scadify (Transform x y z g) = "transform([" <> show x
                              <> "," <> show y
                              <> "," <> show z
                              <> "]) \n" <> scadify g <> "\n"
scadify (Cube width depth height) = "cube([" <> show width
                                    <> "," <> show depth
                                    <> "," <> show height
                                    <> "]);"
