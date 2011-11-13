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
  , text

  , Bang
  , bang

  , Print
  , print

  , Floatatom
  , floatatom

  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc

import Prelude hiding ( print ) 


connect :: ConnectorGraphic 
connect = promoteConn $ \p0 p1 -> 
    primElement $ rec_connect (parent_obj p0) (port_num p0)
                              (parent_obj p1) (port_num p1)


text :: String -> LocGraphic
text ss = promoteLoc $ \(P2 x y) -> 
    primElement $ rec_text x y ss




newtype Bang = Bang { getBang :: Obj }

bang :: LocImage Bang
bang = promoteLoc $ \pt@(P2 x y) ->
    getDisplayProps >>= \props -> 
    primObject (rec_bang x y 15 250 50 0 noSRL 0 (-6) props)
               (\i -> Bang $ Obj { obj_id = i
                                 , obj_bb = BBox pt (P2 (x+15) (y+15)) })


instance HasID Bang where
  getID = obj_id . getBang


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