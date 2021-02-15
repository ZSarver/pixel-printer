{- |
Module      : Geometry
Description : Represents primitive geometry
Copyright   : (c) Zachary Sarver, 2021
License     : GPL-3
Maintainer  : Zachary.Sarver@gmail.com

Represents primitive 3d geometry. The intent of this module is to use it as an
intermediary data structure which can be translated to different output formats.
-}
module Geometry
  (
    Geometry(..)
  ) where

-- | Represents primitive 3d geometry from which to construct 3d prints
data Geometry =
  -- | Represents geometry that is translated by x y z
  Translate Int Int Int Geometry
  -- | Represents a cube with width, depth, and height
  | Cube Int Int Int
  -- | Empty geometry
  | Empty
