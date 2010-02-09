{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.SyntaxStaff
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music syntax tree for staff notation.
--
--------------------------------------------------------------------------------

module Neume.SyntaxStaff
  (
  -- * Phrases and bars
    StaffPhrase(..)
  , StaffBar(..)


  -- * Staff expressions
  , CExpr(..)
  , N_PletDescr(..)

  -- * Staff glyphs
  , Glyph(..)
  , Note(..)
  , Tie
  , ChordPitch(..)


  ) where


import Neume.OneList
import Neume.StateMap


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Note - phrases, bars and CExprs are polymorphic on the glyph
-- type. They can use alternatives to the Glyph type. 

newtype StaffPhrase gly = StaffPhrase { getStaffPhrase :: [StaffBar gly] }
newtype StaffBar    gly = StaffBar    { getStaffBar    :: OneList (CExpr gly) } 




--------------------------------------------------------------------------------
-- Staff \Expressions\





-- | Contextual expression. This is a sequence of one or more 
-- notes together with some context to be communicated to the 
-- pretty printer - the context being either that the notes 
-- should be beamed or that they are n-plets (duplets, triplets, 
-- ...). 
--
-- Note this formulation permits beam groups within beam groups.
-- Ideally this would be disallowed, but beam groups may contain
-- n-plets (and n-plets must be recursive).
--

data CExpr gly = Atomic               (OneList gly) 
               | N_Plet  N_PletDescr  (CExpr   gly)
               | Beamed               (CExpr   gly)
  deriving (Eq,Show)

data N_PletDescr = N_PletDescr Int Int
  deriving (Eq,Show) 




--------------------------------------------------------------------------------
-- Staff Glyphs



data Glyph anno pch dur = GlyNote  (Note anno pch dur) !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (ChordPitch anno pch)) !dur !Tie
                        | Grace    (OneList (Note anno pch dur)) 
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur
  deriving (Eq,Show) 

type Tie = Bool

-- Note dur is not used... 
data ChordPitch anno pch = ChordPitch !anno !pch
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Instances

instance Functor StaffPhrase where
  fmap f (StaffPhrase xs) = StaffPhrase $ fmap (fmap f) xs

instance Functor StaffBar where
  fmap f (StaffBar os)    = StaffBar $ fmap (fmap f) os

instance Functor CExpr where
  fmap f (Atomic os)      = Atomic $ fmap f os
  fmap f (N_Plet d cexpr) = N_Plet d (fmap f cexpr) 
  fmap f (Beamed cexpr)   = Beamed $ fmap f cexpr



-- StateMap
instance StateMap StaffPhrase where
  stmap f (StaffPhrase xs) st = (StaffPhrase xs',st') 
                                where (xs',st') = stmap (stmap f) xs st

instance StateMap StaffBar where
  stmap f (StaffBar os) st = (StaffBar os',st') where (os',st') = stmap (stmap f) os st

instance StateMap CExpr where
  stmap f (Atomic os)   st = (Atomic os',st')   where (os',st') = stmap f os st
  stmap f (N_Plet d ce) st = (N_Plet d ce',st') where (ce',st') = stmap f ce st
  stmap f (Beamed ce)   st = (Beamed ce',st')   where (ce',st') = stmap f ce st
