{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Objects.Glue
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - Glue.
--
--------------------------------------------------------------------------------


module PDSS.Core.Objects.Glue
  ( 

    PdFloat
  , float

  , PdInt
  , int

  , Print
  , print

  ) where 

import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc
import qualified PDSS.Core.Utils.FormatCombinators as PP

import Prelude hiding ( print ) 



newtype PdFloat = PdFloat { getPdFloat :: Obj }


-- | TODO - correct bounding box...
--
float :: Double -> LocImage PdFloat
float d = promoteLoc $ \pt@(P2 x y) ->
    getTextBox (length ss) pt >>= \bbox ->
    primObject (rec_obj x y "float" [PP.double d])
               (\i -> PdFloat $ Obj { obj_id = i, obj_bb = bbox }) 
  where
    -- WARNING - uses Haskell show likely to be invalid for Pd...
    ss = "float " ++ show d


instance HasID PdFloat where
  getID = obj_id . getPdFloat

instance HasIn0 PdFloat
instance HasIn1 PdFloat

instance HasOut0 PdFloat

--------------------------------------------------------------------------------


newtype PdInt = PdInt { getPdInt :: Obj }


-- | TODO - correct bounding box...
--
int :: Int -> LocImage PdInt
int n = promoteLoc $ \pt@(P2 x y) ->
    getTextBox (length ss) pt >>= \bbox ->
    primObject (rec_obj x y "int" [PP.int n])
               (\i -> PdInt $ Obj { obj_id = i, obj_bb = bbox }) 
  where
    ss = "int " ++ show n


instance HasID PdInt where
  getID = obj_id . getPdInt

instance HasIn0 PdInt
instance HasIn1 PdInt

instance HasOut0 PdInt



newtype Print = Print { getPrint :: Obj }

-- | TODO - correct bounding box...
--
print :: LocImage Print
print = promoteLoc $ \pt@(P2 x y) ->
    getTextBox (length "print") pt >>= \bbox ->
    primObject (rec_obj x y "print" [])
               (\i -> Print $ Obj { obj_id = i, obj_bb = bbox }) 

instance HasID Print where
  getID = obj_id . getPrint

instance HasIn0 Print


--------------------------------------------------------------------------------



