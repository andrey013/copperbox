{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PLRGroup
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- The 'neo-Riemannian group' PLR 
--
--------------------------------------------------------------------------------

module PLRGroup
  ( 

  -- Not quite the interface we had in mind...
    nrP
  , nrL
  , nrR
  , Triad

  ) where

import TIGroup
import Z12

type Triad = (Z12,Z12,Z12)

-- See Section 5. from Crans, Fiore and Satyendra: 
-- 'Musical Actions on Dihedral Groups' 
-- for the defintions of P, L & R

nrP :: Triad -> Triad 
nrP (y1,y2,y3) = invertn (fromZ12 $ y1+y3) (y1,y2,y3)

nrL :: Triad -> Triad
nrL (y1,y2,y3) = invertn (fromZ12 $ y2+y3) (y1,y2,y3)

nrR :: Triad -> Triad
nrR (y1,y2,y3) = invertn (fromZ12 $ y1+y2) (y1,y2,y3)
