{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable 
-- Portability :  GHC with TypeFamilies and more
--
-- Common interface to Wumpus.Core.
--
-- This module re-exports types and functions from:
--
-- * "Wumpus.Core.AffineTrans"
--
-- * "Wumpus.Core.BoundingBox"
--
-- * "Wumpus.Core.Colour"
--
-- * "Wumpus.Core.FontSize"
--
-- * "Wumpus.Core.Geometry"
--
-- * "Wumpus.Core.GraphicsState"
--
-- * "Wumpus.Core.OutputPostScript"
--
-- * "Wumpus.Core.OutputSVG"
--
-- * "Wumpus.Core.Picture"
--
-- * "Wumpus.Core.PictureLanguage"
--
-- * "Wumpus.Core.TextEncoding"
--
--
-- Named colours ( black, white etc.) are hidden from 
-- "Wumpus.Core.Colour" to avoid collisions with modules that
-- define colour sets (e.g. all the SVG colours). 
--
-- Some data types are exported "Wumpus.Core.PictureInternal" but
-- are made opaque. 
-- 
--------------------------------------------------------------------------------


module Wumpus.Core
  (
    module Wumpus.Core.AffineTrans 
  , module Wumpus.Core.BoundingBox
  , module Wumpus.Core.Colour
  , module Wumpus.Core.FontSize
  , module Wumpus.Core.Geometry
  , module Wumpus.Core.GraphicsState
  , module Wumpus.Core.OutputPostScript
  , module Wumpus.Core.OutputSVG
  , module Wumpus.Core.Picture
  , module Wumpus.Core.PictureLanguage
  , module Wumpus.Core.TextEncoding

  -- Export from Picture Internal
  , Picture
  , DPicture
  , Primitive
  , DPrimitive
  , Path
  , DPath
  , PathSegment
  , DPathSegment
  , Label
  , DLabel

  , PathProps                   -- Better hidden?
  , LabelProps                  --      "
  , EllipseProps                --      "
  , DrawPath                    --      "
  , DrawEllipse                 --      "

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour hiding ( black, white, red, green, blue )
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OutputPostScript
import Wumpus.Core.OutputSVG
import Wumpus.Core.Picture
import Wumpus.Core.PictureInternal
import Wumpus.Core.PictureLanguage
import Wumpus.Core.TextEncoding



