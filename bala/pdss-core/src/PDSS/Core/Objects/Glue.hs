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

    float
  , int
  , print

  ) where 

import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc
import qualified PDSS.Core.Utils.FormatCombinators as PP

import Data.Sized.Ix                            -- package: sized types

import Prelude hiding ( print ) 





-- | TODO - correct bounding box...
--
float :: Double -> LocObject X2 X1
float d = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length ss) pt >>= \bbox ->
    primObject (rec_obj x y "float" [PP.double d])
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 
  where
    -- WARNING - uses Haskell show likely to be invalid for Pd...
    ss = "float " ++ show d



--------------------------------------------------------------------------------




-- | TODO - correct bounding box...
--
int :: Int -> LocObject X2 X1
int n = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length ss) pt >>= \bbox ->
    primObject (rec_obj x y "int" [PP.int n])
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 
  where
    ss = "int " ++ show n




-- newtype Print = Print { getPrint :: Obj }

-- | TODO - correct bounding box...
--
print :: LocObject X1 X0
print = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length "print") pt >>= \bbox ->
    primObject (rec_obj x y "print" [])
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 

-- instance HasID Print where
--   getID = obj_id . getPrint

-- instance HasIn0 Print


--------------------------------------------------------------------------------



