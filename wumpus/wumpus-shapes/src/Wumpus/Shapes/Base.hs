{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Common core for shapes (anchors...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Base
  ( 

  -- * Shape label
    ShapeLabel(..)
  , AddLabel(..)
  , basicLabel
  , updateText
  , drawShapeLabel

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)


  ) where

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Core.Colour ( black )

import Data.AffineSpace                 -- package: vector-space

data ShapeLabel = ShapeLabel
      { shapelabel_text         :: String
      , shapelabel_font_props   :: FontAttr
      , shapelabel_font_colour  :: DRGB
      }

basicLabel :: String -> ShapeLabel
basicLabel text = ShapeLabel text wumpus_default_font black

updateText :: String -> ShapeLabel -> ShapeLabel
updateText text s = s { shapelabel_text = text } 



drawShapeLabel :: (Fractional u, Ord u) => ShapeLabel -> Point2 u -> Primitive u
drawShapeLabel sl ctr = textlabel attr (shapelabel_text sl) pt
  where
    attr     = (shapelabel_font_colour sl, shapelabel_font_props sl)
    font_sz  = font_size $ shapelabel_font_props sl
    text     = shapelabel_text sl
    twidth   = textWidth font_sz (length text)
    theight  = textHeight font_sz
    pt       = ctr .-^ V2 (twidth / 2) (theight / 2)


class AddLabel t where
  addLabel :: t -> String -> t

--------------------------------------------------------------------------------
-- Anchors

class AnchorCenter a where
  center :: DUnit a ~ u => a -> Point2 u

class AnchorCardinal a where
  north :: DUnit a ~ u => a -> Point2 u
  south :: DUnit a ~ u => a -> Point2 u
  east  :: DUnit a ~ u => a -> Point2 u
  west  :: DUnit a ~ u => a -> Point2 u

  northeast :: DUnit a ~ u => a -> Point2 u
  southeast :: DUnit a ~ u => a -> Point2 u
  southwest :: DUnit a ~ u => a -> Point2 u
  northwest :: DUnit a ~ u => a -> Point2 u

