{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.Rewriting
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Generating Mullein from Haskore automatically is problemmatic. 
-- Note lengths in Haskore need some degree of interpretation to turn 
-- into LilyPond or ABC note lengths, e.g. in Haskore trills/grace notes 
-- are arbitrary short durations. It is difficult to autmatically 
-- recognize trills a note list with short tuplets (n-plets), so perhaps
-- the most useful solution is to make the note list available to the 
-- user and allow arbitrary rewrites on it.
-- This is the first attempt...
--
--------------------------------------------------------------------------------


module Mullein.Rewriting where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.StringRewriting

import Control.Applicative
import Data.Ratio


data Alphabet e = N e Duration     -- Note
                | R Duration       -- Rest
                | S Duration       -- Spacer
                | T                -- Tie
                | G e Duration     -- Grace
  deriving (Eq,Show)

instance Temporal (Alphabet e) where
    duration (N _ d)          = d
    duration (R d)            = d
    duration (S d)            = d
    duration T                = 0
    duration (G _ _)          = 0   -- the duration of a grace is only for printing

    swapDuration d (N e _)    = N e d
    swapDuration d (R _)      = R d
    swapDuration d (S _)      = R d
    swapDuration _ T          = T
    swapDuration _ (G e d)    = G e d



--------------------------------------------------------------------------------
-- type changing rewrite from (Alphabet e) to Element e


-- Notes, rests, spacers map one to one 
mUnary :: Match r m e
mUnary = pmatchOne p where
  p (N _ _)     = True
  p (R _)       = True
  p (S _)       = True
  p _           = False

rUnary :: RuleTC (Alphabet e) (ElementP e) m
rUnary = (wrapH . fn) <$> mUnary where
  fn (N e d)    = Note e d
  fn (R d)      = Rest d
  fn (S d)      = Spacer d

  -- This should be unreachable if mUnary matches correctly
  fn _          = error $ "rUnary - match failure"


-- This is a problem!!!
-- ElementP will probably need extending with a Tie constructor.
-- For the moment ties are lost.

rTie :: Eq e => RuleTC (Alphabet e) (ElementP e) m
rTie = id <$ lit T

rGrace :: RuleTC (Alphabet e) (ElementP e) m
rGrace = (wrapH . fn) <$> many1 (pmatchOne p) where
  p (G _ _) = True
  p _       = False

  fn xs     = GraceNotes $ map extract xs
  
  extract (G e d) = (e,d)
  extract _       = error $ "rGrace - match failure" -- unreachable?


alphabetToElementP :: Eq e => [Alphabet e] -> [ElementP e]
alphabetToElementP = rewriteTC'id (choice [rUnary,rTie,rGrace])


--------------------------------------------------------------------------------


type Match r m e = MatcherT (Alphabet e) r m (Alphabet e)





note :: Match r m e
note = pmatchOne p where 
   p (N _ _) = True
   p _       = False

eqD :: Temporal e => Duration -> e -> Bool
eqD d e = d == duration e 

matchesD :: Duration -> Match r m e -> Match r m e
matchesD d fn = satisfies fn (d `eqD`)


appogiaturaQn :: RuleTP (Alphabet e) m
appogiaturaQn = (\(N e _) (N e' _) -> listH [G e tn, N e' qn]) 
    <$> matchesD (1%32) note  <*> matchesD (1%4 - 1%32) note



tn :: Duration
tn = 1%32

qn :: Duration
qn = 1%4

