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

    mtof
  , ftom

  ) where 

import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc

import Data.Sized.Ix                            -- package: sized-types



mtof :: LocObject X1 X1
mtof = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length "mtof") pt >>= \bbox ->
    primObject (rec_obj x y "mtof" [])
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 



--------------------------------------------------------------------------------


ftom :: LocObject X1 X1
ftom = promoteLoc $ \pt@(P2 x y) ->
    getObjectBBox (length "ftom") pt >>= \bbox ->
    primObject (rec_obj x y "ftom" [])
               (\i -> Obj { obj_id = i, obj_bb = bbox }) 
