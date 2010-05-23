{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shape.Base
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

module Wumpus.Extra.Shape.Base
  ( 

  -- * Current transformation matrix
    CTM
  , translateCTM
  , scaleCTM
  , rotateCTM
  , rotateAboutCTM

  -- * Composite
  , Composite(..)
  , frameComposite
  , labelledComposite
  , simpleComposite

  -- * Shape label
  , ShapeLabel(..)
  , AddLabel(..)
  , basicLabel
  , updateText

  -- * Anchors
  , AnchorCenter(..)
  , AnchorCardinal(..)


  ) where

import Wumpus.Core hiding ( CTM )
import qualified Wumpus.Core.Colour as Colour

import Data.AffineSpace         -- package: vector-space

import qualified Data.Foldable          as F
import Data.Monoid
import Data.Sequence (Seq, (|>) )
import qualified Data.Sequence          as S


--------------------------------------------------------------------------------
-- Shapes will generally include a translation matrix...

type CTM u = Matrix3'3 u

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x y m = translationMatrix x y * m

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x y m = scalingMatrix x y * m

rotateCTM       :: (Floating u, Real u) => Radian -> CTM u -> CTM u
rotateCTM r m   = rotationMatrix r * m

rotateAboutCTM  :: (Floating u, Real u) => Radian -> Point2 u -> CTM u -> CTM u
rotateAboutCTM r pt m = originatedRotationMatrix r pt * m




--------------------------------------------------------------------------------

newtype Composite u = Composite { getPrimitives :: Seq (Primitive u) }

instance Monoid (Composite u) where
  mempty = Composite S.empty
  Composite sa `mappend` Composite sb = Composite $ sa `mappend` sb

labelledComposite :: (Fractional u, Ord u) 
                  => CTM u -> Point2 u -> Maybe ShapeLabel -> Primitive u -> Composite u
labelledComposite ctm ctr mb_lbl prim = maybe noLabel withLabel mb_lbl 
  where
    noLabel       = Composite $ S.singleton prim
    withLabel lbl = Composite (S.empty |> lbl_prim |> prim ) where 
                      lbl_prim = transform ctm $ drawShapeLabel lbl (ctm *# ctr)
            

simpleComposite :: Primitive u -> Composite u
simpleComposite = Composite . S.singleton 

frameComposite :: (Floating u, Ord u) => Composite u -> Picture u
frameComposite = frameMulti . F.toList . getPrimitives


data ShapeLabel = ShapeLabel
      { shapelabel_text         :: String
      , shapelabel_font_props   :: FontAttr
      , shapelabel_font_colour  :: PSRgb
      }

basicLabel :: String -> ShapeLabel
basicLabel text = ShapeLabel text wumpus_default_font Colour.black

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

