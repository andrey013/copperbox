{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------

module Wumpus.Djembe.Base 
  (
    Beat(..)
  , Bar
  , Group

  , DjembeBeat
  , DjembeBar
  , DjembeGroup

  , CStrokeBase(..)
  , CStrokeAnno(..)

  ) where

import Wumpus.Basic.Kernel

-- Flam is an @I@


data Beat a = I  a            -- one beat, could be unplayed (aka. a rest)
            | S  a            -- swing, one beat with fractional onset delay
            | Ha a a          -- half, beat divided into two  
            | Pl Int Int [a]  -- plet n in time of d (list-length should be n) 
  deriving (Eq,Show)

type Bar rep   = [Group rep]
type Group rep = [Beat rep]


type DjembeBeat  u = Beat (LocGraphic u)
type DjembeBar   u = Bar u
type DjembeGroup u = Group u


class CStrokeBase repr where
  rest_note        :: repr
  period           :: repr
 

class CStrokeAnno repr where
   optional     :: repr -> repr
   lead_in      :: repr -> repr
   accent       :: repr -> repr
   dominant     :: repr -> repr
   other_hand   :: repr -> repr

