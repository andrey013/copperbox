{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.TextLine
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Coordinate points
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shape.TextLine
  ( 
    TextLine(..)
  , textLine
  , drawTextLine

  ) where

import Wumpus.Core hiding ( CTM )
import Wumpus.Extra.Shape.Base
import Wumpus.Extra.Shape.Rectangle
import Wumpus.Extra.Utils

import Data.AffineSpace

--------------------------------------------------------------------------------
-- Text label

-- Note - a Textline needs some \"drawing attributes\" - Font 
-- style and size.
-- 
-- Unfortunately this /divides/ the TextLabel type class from 
-- wumpus-core into two halves - needs size and font, but wants
-- colour later (only when drawn).
--

data TextLine u = TextLine
      { text_string           :: String
      , text_font_props       :: FontAttr
      , text_rect             :: Rectangle u
      }

type instance DUnit (TextLine u) = u
      


instance (Floating u, Real u) => Rotate (TextLine u) where
  rotate r = pstar (\m s -> s { text_rect = rotate r m }) text_rect


instance (Floating u, Real u) => RotateAbout (TextLine u) where
  rotateAbout r pt = 
      pstar (\m s -> s { text_rect = rotateAbout r pt m }) text_rect

instance Num u => Scale (TextLine u) where
  scale x y = pstar (\m s -> s { text_rect = scale x y m }) text_rect

instance Num u => Translate (TextLine u) where
  translate x y = 
     pstar (\m s -> s { text_rect = translate x y m }) text_rect



instance (Fractional u) => AnchorCenter (TextLine u) where
  center = center . text_rect

instance (Fractional u) =>  AnchorCardinal (TextLine u) where
  north = north . text_rect
  south = south . text_rect
  east  = east  . text_rect
  west  = west  . text_rect

  northeast = northeast . text_rect
  southeast = southeast . text_rect 
  southwest = southwest . text_rect
  northwest = northwest . text_rect


textLine :: (Floating u, Fractional u, Ord u) 
         => FontAttr -> String -> Point2 u -> TextLine u
textLine attr s ctr = TextLine s attr $ rectangle w h ctr
  where 
    (V2 w h)           = tr .-. bl
    (bl, _br, tr, _tl) = corners $ boundary $ textlabel attr s ctr



drawTextLine :: (Num u, PSColour c) => c -> TextLine u -> Primitive u
drawTextLine c txt = 
    textlabel (psColour c, text_font_props txt) (text_string txt) bottom_left
  where
    bottom_left = rect_bottom_left $ text_rect txt