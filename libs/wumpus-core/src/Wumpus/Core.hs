{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Common interface to Wumpus.Core...
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
  , DrawProp                    --      "
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

