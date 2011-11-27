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
   
  , array

  , connect 

  , floatatom
  , symbolatom

  , message



  -- * From the @Put@ menu
  , bang
  , toggle
  , vslider
  , hslider
  , vradio
  , hradio
  , canvas
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

import Data.Sized.Ix                            -- package: sized-types

import Prelude hiding ( print ) 





--------------------------------------------------------------------------------
-- Arrays


-- Array has no bounding box so it cannot wrap the Obj object.
-- 
-- Maybe we still need the getID typeclass...
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


-- | Width is number of chars - default (for context?) is 5.
--
floatatom :: Int -> LocObject X1 X1
floatatom w = promoteLoc $ \pt@(P2 x y) ->
    getAtomBBox w pt >>= \bbox ->    
    primObject (rec_floatatom x y w 0 0 LEFT noSRL)
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 



symbolatom :: Int -> LocObject X1 X1
symbolatom w = promoteLoc $ \pt@(P2 x y) ->
    getAtomBBox w pt >>= \bbox ->    
    primObject (rec_symbolatom x y w 0 0 LEFT noSRL)
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 



--------------------------------------------------------------------------------


message :: [String] -> LocObject X1 X1
message xs = promoteLoc $ \pt@(P2 x y) -> 
    getMessageBBox (length ss) pt >>= \bbox ->    
    primObject (rec_msg x y $ map PP.string xs)
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 
  where
    ss = concatMap (' ':) xs



--------------------------------------------------------------------------------


bang :: LocObject X0 X1
bang = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs ->
    getBangOnLoad   >>= \bol -> 
    let bbox = bangBBox pt in 
        primObject (rec_bang x y 15 250 50 bol noSRL offs props)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })





--------------------------------------------------------------------------------


toggle :: Int -> LocObject X0 X1
toggle sz = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = toggleBBox pt in
        primObject (rec_toggle x y sz DEFAULT_ON_LOAD noSRL offs props 234 234)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })




vslider :: Int -> Int -> LocObject X0 X1
vslider lo hi = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = vsliderBBox 15 128 pt in
        primObject (rec_vslider x y 15 28 lo hi SLIDER_LINEAR NONE_ON_LOAD
                                noSRL offs props 0 SLIDER_JUMPS)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })


hslider :: Int -> Int -> LocObject X0 X1
hslider lo hi = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = vsliderBBox 15 128 pt in
        primObject (rec_vslider x y 15 28 lo hi SLIDER_LINEAR NONE_ON_LOAD
                                noSRL offs props 0 SLIDER_JUMPS)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })



--------------------------------------------------------------------------------

vradio :: Int -> LocObject X0 X1
vradio num = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = vradioBBox num pt in
        primObject (rec_vradio x y 15 NEW_ONLY NONE_ON_LOAD num noSRL offs props 0)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })


hradio :: Int -> LocObject X0 X1
hradio num = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = hradioBBox num pt in
        primObject (rec_hradio x y 15 NEW_ONLY NONE_ON_LOAD num noSRL offs props 0)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })



--------------------------------------------------------------------------------

canvas :: Int -> Int -> LocObject X0 X0
canvas w h = promoteLoc $ \pt@(P2 x y) -> 
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \offs -> 
    let bbox = BBox pt (P2 (x+w) (y+h)) in
        primObject (rec_canvas x y 15 w h noSRL offs props)
                   (\i -> Obj { obj_id = i, obj_bb = bbox })




--------------------------------------------------------------------------------

comment :: String -> LocGraphic
comment ss = promoteLoc $ \(P2 x y) -> 
    primElement $ rec_text x y ss


