{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Objects
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - .
--
--------------------------------------------------------------------------------


module PDSS.Core.Objects
  ( 
    module PDSS.Core.Objects.Glue
  , module PDSS.Core.Objects.Math
   
  , Array
  , array

  , connect 

  , Floatatom
  , floatatom

  , Message
  , message



  -- * From the @Put@ menu
  , Bang
  , bang

  , Toggle
  , toggle

  , Slider
  , vslider
  , hslider

  , Radio
  , vradio
  , hradio


  , comment



  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.Objects.Glue
import PDSS.Core.Objects.Math
import PDSS.Core.PdDoc
import qualified PDSS.Core.Utils.FormatCombinators as PP

import Data.List ( intersperse )
import Prelude hiding ( print ) 





-- Helper


--------------------------------------------------------------------------------
-- Arrays


-- Array has no bounding box so it cannot wrap the Obj object.
--
newtype Array = Array { _arr_id :: Int }


array :: String -> Int -> Int -> Image Array
array ss sz save = primObject mkDoc Array
  where
    mkDoc = rec_array ss sz save




--------------------------------------------------------------------------------
-- Connect

connect :: ConnectorGraphic 
connect = promoteConn $ \p0 p1 -> 
    primElement $ rec_connect (parent_obj p0) (port_num p0)
                              (parent_obj p1) (port_num p1)


--------------------------------------------------------------------------------

newtype Floatatom = Floatatom { getFloatatom :: Obj }

-- | Width is probably number of chars...
floatatom :: Int -> LocImage Floatatom
floatatom w = promoteLoc $ \pt@(P2 x y) ->
    primObject (rec_floatatom x y w 0 0 LEFT noSRL)
               (\i -> Floatatom $ Obj { obj_id = i
                                      , obj_bb = BBox pt (P2 (x+w) (y + 10)) }) 


instance HasID Floatatom where
  getID = obj_id . getFloatatom


instance HasOut0 Floatatom


--------------------------------------------------------------------------------

newtype Message = Message { getMessage :: Obj }

message :: [String] -> LocImage Message
message xs = promoteLoc $ \pt@(P2 x y) -> 
    getMessageBBox (length ss) pt >>= \bbox ->    
    primObject (rec_msg x y $ map PP.string xs)
               (\i -> Message $ Obj { obj_id = i, obj_bb = bbox }) 
  where
    ss = concat $ intersperse " " xs

instance HasID Message where
  getID = obj_id . getMessage


instance HasIn0  Message
instance HasOut0 Message


--------------------------------------------------------------------------------


newtype Bang = Bang { getBang :: Obj }

bang :: LocImage Bang
bang = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    let bbox = bangBBox pt in 
        primObject (rec_bang x y 15 250 50 0 noSRL 0 (-6) props)
                   (\i -> Bang $ Obj { obj_id = i, obj_bb = bbox })


instance HasID Bang where
  getID = obj_id . getBang





--------------------------------------------------------------------------------

newtype Toggle = Toggle { getToggle :: Obj }


toggle :: Int -> LocImage Toggle
toggle sz = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    let bbox = toggleBBox pt in
        primObject (rec_toggle x y sz 1 noSRL xoff yoff props 234 234)
                   (\i -> Toggle $ Obj { obj_id = i, obj_bb = bbox })


instance HasID Toggle where
  getID = obj_id . getToggle

instance HasOut0 Toggle



newtype Slider = Slider { getSlider :: Obj }

instance HasID Slider where
  getID = obj_id . getSlider

instance HasOut0 Slider


vslider :: Int -> Int -> LocImage Slider
vslider lo hi = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    let bbox = vsliderBBox 15 128 pt in
        primObject (rec_vslider x y 15 28 lo hi SLIDER_LINEAR NONE_ON_LOAD
                                noSRL xoff yoff props 0 SLIDER_JUMPS)
                   (\i -> Slider $ Obj { obj_id = i, obj_bb = bbox })


hslider :: Int -> Int -> LocImage Slider
hslider lo hi = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    let bbox = vsliderBBox 15 128 pt in
        primObject (rec_vslider x y 15 28 lo hi SLIDER_LINEAR NONE_ON_LOAD
                                noSRL xoff yoff props 0 SLIDER_JUMPS)
                   (\i -> Slider $ Obj { obj_id = i, obj_bb = bbox })



--------------------------------------------------------------------------------

newtype Radio = Radio { getRadio :: Obj }

vradio :: Int -> LocImage Radio
vradio num = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    let bbox = vradioBBox num pt in
        primObject (rec_vradio x y 15 True 0 num noSRL xoff yoff props 0)
                   (\i -> Radio $ Obj { obj_id = i, obj_bb = bbox })


hradio :: Int -> LocImage Radio
hradio num = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    let bbox = hradioBBox num pt in
        primObject (rec_hradio x y 15 True 0 num noSRL xoff yoff props 0)
                   (\i -> Radio $ Obj { obj_id = i, obj_bb = bbox })



instance HasID Radio where
  getID = obj_id . getRadio

instance HasOut0 Radio


--------------------------------------------------------------------------------

comment :: String -> LocGraphic
comment ss = promoteLoc $ \(P2 x y) -> 
    primElement $ rec_text x y ss


