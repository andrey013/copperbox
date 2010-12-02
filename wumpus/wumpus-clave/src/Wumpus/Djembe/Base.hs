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

  , CStroke(..)

  ) where

import Wumpus.Basic.Graphic

-- Where does flam sit in the data type?
-- Is flam an @I@ or should it have a constructor?


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


class CStroke repr where
   optional :: repr -> repr
   lead_in  :: repr -> repr
   accent   :: repr -> repr
