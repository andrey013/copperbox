{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.Rewriting
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Automatically translating Haskore to Mullein is problemmatic. 
-- Note lengths in Haskore need some degree of interpretation to turn 
-- into LilyPond or ABC note lengths, e.g. in Haskore trills/grace notes 
-- are arbitrary short durations. It is difficult to autmatically 
-- recognize trills a note list with short tuplets (n-plets), so perhaps
-- the most useful solution is to make the note list available to the 
-- user and allow arbitrary rewrites on it.
--
--------------------------------------------------------------------------------


module MulleinHaskore.Rewriting ( 
  Alphabet(..),
  Match,
  alphabetToElementP,
  
  wrapD,
  listD,
  zeroD,

  preserving,
  idOne,
  matchnote,
  matchDur,
  eqDur,
  appogiaturaQn,

  ) where

import MulleinHaskore.StringRewriting
import Mullein.Core
import Mullein.Duration

import Control.Applicative
import qualified Data.DList as D
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

type Match r m e = MatcherT (Alphabet e) r m (Alphabet e)


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
rUnary = (wrapD . fn) <$> mUnary where
  fn (N e d)    = Note e d
  fn (R d)      = Rest d
  fn (S d)      = Spacer d

  -- This should be unreachable if mUnary matches correctly
  fn _          = error $ "rUnary - match failure"


-- This is a problem!!!
-- ElementP will probably need extending with a Tie constructor.
-- For the moment ties are lost.

rTie :: Eq e => RuleTC (Alphabet e) (ElementP e) m
rTie = zeroD <$ lit T

rGrace :: RuleTC (Alphabet e) (ElementP e) m
rGrace = (wrapD . fn) <$> many1 (pmatchOne p) where
  p (G _ _) = True
  p _       = False

  fn xs     = GraceNotes $ map extract xs
  
  extract (G e d) = (e,d)
  extract _       = error $ "rGrace - match failure" -- unreachable?


alphabetToElementP :: Eq e => [Alphabet e] -> [ElementP e]
alphabetToElementP = rewriteTC'id (choice [rUnary,rTie,rGrace])


--------------------------------------------------------------------------------



wrapD :: a -> D.DList a
wrapD a = D.singleton a

listD :: [a] -> D.DList a
listD = D.fromList

zeroD :: D.DList a
zeroD = D.empty


preserving :: RuleTP (Alphabet e) m -> RuleTP (Alphabet e) m
preserving r = r <|> idOne

idOne :: RuleTP tok m
idOne = wrapD <$> one





matchnote :: Match r m e
matchnote = pmatchOne p where 
   p (N _ _) = True
   p _       = False

eqDur :: Temporal e => Duration -> e -> Bool
eqDur d e = d == duration e 

matchDur :: Duration -> Match r m e -> Match r m e
matchDur d fn = satisfies fn (d `eqDur`)


appogiaturaQn :: RuleTP (Alphabet e) m
appogiaturaQn = (\(N e _) (N e' _) -> listD [G e tn, N e' qn]) 
    <$> matchDur (1%32) matchnote  <*> matchDur (1%4 - 1%32) matchnote
  where
    tn :: Duration
    tn = 1%32

    qn :: Duration
    qn = 1%4

