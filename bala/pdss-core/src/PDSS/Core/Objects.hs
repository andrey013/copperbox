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

    connect 

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


  , text



  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc
import PDSS.Core.Utils.FormatCombinators

import Prelude hiding ( print ) 



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
    primObject (rec_msg x y $ map string xs)
               (\i -> Msg $ Obj { obj_id = i
                                , obj_bb = BBox pt (P2 (x+10) (y + 10)) }) 





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





text :: String -> LocGraphic
text ss = promoteLoc $ \(P2 x y) -> 
    primElement $ rec_text x y ss


