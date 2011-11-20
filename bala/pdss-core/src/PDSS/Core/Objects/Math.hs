{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Objects.Math
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Objects - Math.
--
--------------------------------------------------------------------------------


module PDSS.Core.Objects.Math
  ( 


    Mtof
  , mtof

  , Ftom
  , ftom

  ) where 

import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc




newtype Mtof = Mtof { getMtof :: Obj }

mtof :: LocImage Mtof
mtof = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length "mtof") pt >>= \bbox ->
    primObject (rec_obj x y "mtof" [])
               (\i -> Mtof $ Obj { obj_id = i, obj_bb = bbox }) 

instance HasID Mtof where
  getID = obj_id . getMtof

instance HasIn0 Mtof

instance HasOut0 Mtof



--------------------------------------------------------------------------------

newtype Ftom = Ftom { getFtom :: Obj }

ftom :: LocImage Ftom
ftom = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length "ftom") pt >>= \bbox ->
    primObject (rec_obj x y "ftom" [])
               (\i -> Ftom $ Obj { obj_id = i, obj_bb = bbox }) 

instance HasID Ftom where
  getID = obj_id . getFtom

instance HasIn0 Ftom

instance HasOut0 Ftom


