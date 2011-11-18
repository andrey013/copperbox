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


    Array
  , array

  , connect 

  , Floatatom
  , floatatom

  , Msg
  , msg


  , Bang
  , bang

  , Toggle
  , toggle

  , Print
  , print


  , PdInt
  , int

  , Ftom
  , ftom

  , Mtof
  , mtof

  , text



  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc
import qualified PDSS.Core.Utils.FormatCombinators as PP

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

newtype Msg = Msg { getMsg :: Obj }

msg :: [String] -> LocImage Msg
msg xs = promoteLoc $ \pt@(P2 x y) -> 
    primObject (rec_msg x y $ map PP.string xs)
               (\i -> Msg $ Obj { obj_id = i
                                , obj_bb = BBox pt (P2 (x+10) (y + 10)) }) 


instance HasID Msg where
  getID = obj_id . getMsg


instance HasIn0  Msg
instance HasOut0 Msg


--------------------------------------------------------------------------------


newtype Bang = Bang { getBang :: Obj }

bang :: LocImage Bang
bang = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    primObject (rec_bang x y 15 250 50 0 noSRL 0 (-6) props)
               (\i -> Bang $ Obj { obj_id = i
                                 , obj_bb = BBox pt (P2 (x+15) (y+15)) })


instance HasID Bang where
  getID = obj_id . getBang





--------------------------------------------------------------------------------

newtype Toggle = Toggle { getToggle :: Obj }


toggle :: Int -> LocImage Toggle
toggle sz = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    getLabelOffsets >>= \(xoff,yoff) -> 
    primObject (rec_toggle x y sz 1 noSRL xoff yoff props 234 234)
               (\i -> Toggle $ Obj { obj_id = i
                                 , obj_bb = BBox pt (P2 (x+15) (y+15)) })


instance HasID Toggle where
  getID = obj_id . getToggle

instance HasOut0 Toggle


--------------------------------------------------------------------------------


-- Potentially Print should not be a newtype.
-- 
-- print is an obj with one in and no out ports.
--
-- There will be a lot of boilerplate if every object is newtype wrapped.
--

newtype Print = Print { getPrint :: Obj }

-- | TODO - correct bounding box...
--
print :: LocImage Print
print = promoteLoc $ \pt@(P2 x y) ->
    primObject (rec_obj x y "print" [])
               (\i -> Print $ Obj { obj_id = i
                                  , obj_bb = BBox pt (P2 x y) }) 

instance HasID Print where
  getID = obj_id . getPrint

instance HasIn0 Print


--------------------------------------------------------------------------------


newtype PdInt = PdInt { getPdInt :: Obj }


-- | TODO - correct bounding box...
--
int :: Int -> LocImage PdInt
int n = promoteLoc $ \pt@(P2 x y) ->
    primObject (rec_obj x y "int" [PP.int n])
               (\i -> PdInt $ Obj { obj_id = i
                                  , obj_bb = BBox pt (P2 x y) }) 

instance HasID PdInt where
  getID = obj_id . getPdInt

instance HasIn0 PdInt
instance HasIn1 PdInt

instance HasOut0 PdInt



--------------------------------------------------------------------------------

newtype Ftom = Ftom { getFtom :: Obj }

ftom :: LocImage Ftom
ftom = promoteLoc $ \pt@(P2 x y) ->
    primObject (rec_obj x y "ftom" [])
               (\i -> Ftom $ Obj { obj_id = i
                                 , obj_bb = BBox pt (P2 x y) }) 

instance HasID Ftom where
  getID = obj_id . getFtom

instance HasIn0 Ftom

instance HasOut0 Ftom




newtype Mtof = Mtof { getMtof :: Obj }

mtof :: LocImage Mtof
mtof = promoteLoc $ \pt@(P2 x y) ->
    primObject (rec_obj x y "mtof" [])
               (\i -> Mtof $ Obj { obj_id = i
                                 , obj_bb = BBox pt (P2 x y) }) 

instance HasID Mtof where
  getID = obj_id . getMtof

instance HasIn0 Mtof

instance HasOut0 Mtof

--------------------------------------------------------------------------------

text :: String -> LocGraphic
text ss = promoteLoc $ \(P2 x y) -> 
    primElement $ rec_text x y ss


