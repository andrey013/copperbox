{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  M2.SyntaxKURE
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music rep syntax tree.
--
--------------------------------------------------------------------------------

module M2.SyntaxKURE
  (
    SGeneric(..)

  ) where


import M2.Syntax

import Language.KURE            -- package kure

import Control.Monad
import qualified Data.Foldable          as F
import Data.Monoid
import qualified Data.Traversable       as T


data SGeneric anno pch dur = GPhrase     (Phrase      anno pch dur)
                           | GBar        (Bar         anno pch dur)
                           | GCExpr      (CExpr       anno pch dur)
                           | GAExpr      (AExpr       anno pch dur)
                           | GGlyph      (Glyph       anno pch dur)
                           | GNote       (Note        anno pch dur)
                           | GChordPitch (ChordPitch  anno pch dur)


-- SGeneric is its own Generic root.
instance Term (SGeneric anno pch dur) where
  type Generic (SGeneric anno pch dur)    = SGeneric  anno pch dur
  inject                                  = id
  select e                                = return e

instance Term (Phrase anno pch dur) where
  type Generic (Phrase anno pch dur)      = SGeneric anno pch dur
  inject                                  = GPhrase
  select (GPhrase a)                      = Just a
  select _                                = Nothing

instance Term (CExpr anno pch dur) where
  type Generic (CExpr anno pch dur)       = SGeneric anno pch dur
  inject                                  = GCExpr
  select (GCExpr a)                       = Just a
  select _                                = Nothing

instance Term (AExpr anno pch dur) where
  type Generic (AExpr anno pch dur)       = SGeneric anno pch dur
  inject                                  = GAExpr
  select (GAExpr a)                       = Just a
  select _                                = Nothing

instance Term (Glyph anno pch dur) where
  type Generic (Glyph anno pch dur)       = SGeneric anno pch dur
  inject                                  = GGlyph
  select (GGlyph a)                       = Just a
  select _                                = Nothing

instance Term (Note anno pch dur) where
  type Generic (Note anno pch dur)        = SGeneric anno pch dur
  inject                                  = GNote
  select (GNote a)                        = Just a
  select _                                = Nothing


instance Term (ChordPitch anno pch dur) where
  type Generic (ChordPitch anno pch dur)  = SGeneric anno pch dur
  inject                                  = GChordPitch
  select (GChordPitch a)                  = Just a
  select _                                = Nothing



--------------------------------------------------------------------------------
-- Walkers

-- Phrase
{-

instance (Monoid dec, Monad m) => Walker m dec (Phrase anno pch dur) where
  allR rr   = phraseR (extractR rr)
  crushU rr = phraseU (extractU rr)


phraseR   :: (Monoid dec, Monad m) 
          => Rewrite m dec (Bar    anno pch dur) 
          -> Rewrite m dec (Phrase anno pch dur)
phraseR rr = transparently $ translate $ \ (Phrase xs) -> 
    liftM Phrase (mapM (apply rr) xs)
   

phraseU   :: (Monad m, Monoid dec, Monoid r)
          => Translate m dec (Bar    anno pch dur) r 
          -> Translate m dec (Phrase anno pch dur) r
phraseU rr = translate $ \ (Phrase xs) -> 
    liftM mconcat $ mapM (apply rr) xs

-}



-- Glyph

instance (Monoid dec, Monad m) => Walker m dec (Glyph anno pch dur) where
  allR rr   = glyNoteR (extractR rr) 
           <+ restR
           <+ spacerR
           <+ chordR   (extractR rr)
  crushU rr = glyNoteU (extractU rr) 
           <+ restU
           <+ spacerU
           <+ chordU   (extractU rr)

glyNoteR        :: (Monoid dec, Monad m) 
                => Rewrite m dec (Note  anno pch dur) 
                -> Rewrite m dec (Glyph anno pch dur)
glyNoteR rr     = transparently $ translate $ \ e -> case e of         -- translate?
                    GlyNote e1 t -> liftM (GlyNote `flip` t) (apply rr e1)
                    _            -> fail "glyNoteR"

glyNoteU        :: (Monad m, Monoid dec, Monoid r)
                => Translate m dec (Note  anno pch dur) r 
                -> Translate m dec (Glyph anno pch dur) r
glyNoteU rr     = translate $ \ e -> case e of 
                    GlyNote e1 _ -> apply rr e1
                    _            -> fail "glyNoteU"

restR           :: (Monoid dec, Monad m) 
                => Rewrite m dec (Glyph anno pch dur)
restR           = transparently $ translate $ \ e -> case e of
                    r1@(Rest _) -> return r1
                    _           -> fail "restR"

restU           :: (Monad m, Monoid dec, Monoid r)
                => Translate m dec (Glyph anno pch dur) r
restU           = translate $ \ e -> case e of 
                    Rest _ -> return mempty
                    _      -> fail "restU"




spacerR         :: (Monoid dec, Monad m) 
                => Rewrite m dec (Glyph anno pch dur)
spacerR         = transparently $ translate $ \ e -> case e of
                    s1@(Spacer _) -> return s1
                    _             -> fail "spacerR"

spacerU         :: (Monad m, Monoid dec, Monoid r)
                => Translate m dec (Glyph anno pch dur) r
spacerU         = translate $ \ e -> case e of 
                    Spacer _ -> return mempty
                    _        -> fail "spacerU"


chordR          :: (Monoid dec, Monad m) 
                => Rewrite m dec (ChordPitch  anno pch dur) 
                -> Rewrite m dec (Glyph       anno pch dur)
chordR rr       = transparently $ translate $ \ e -> case e of         -- translate?
                    Chord e1 d t -> liftM (\xs -> Chord xs d t) 
                                          (T.mapM (apply rr) e1)
                    _            -> fail "chordR"


chordU          :: (Monad m, Monoid dec, Monoid r)
                => Translate m dec (ChordPitch  anno pch dur) r 
                -> Translate m dec (Glyph       anno pch dur) r
chordU rr       = translate $ \ e -> case e of 
                    Chord e1 _ _ -> liftM (F.foldl' mappend mempty) 
                                          (T.mapM (apply rr) e1)
                    _            -> fail "chordU"



-- Note

instance (Monoid dec, Monad m) => Walker m dec (Note anno pch dur) where
  allR _   = noteR
  crushU _ = noteU


noteR           :: (Monoid dec, Monad m) 
                => Rewrite m dec (Note anno pch dur)
noteR           = transparently $ rewrite $ return 
   

noteU           :: (Monad m, Monoid dec, Monoid r)
                => Translate m dec (Note anno pch dur) r
noteU           = translate $ \ _ -> return mempty


-- ChordPitch

instance (Monoid dec, Monad m) => Walker m dec (ChordPitch anno pch dur) where
  allR _   = chordPitchR
  crushU _ = chordPitchU


chordPitchR   :: (Monoid dec, Monad m) 
              => Rewrite m dec (ChordPitch anno pch dur)
chordPitchR   = transparently $ rewrite $ return 
   

chordPitchU   :: (Monad m, Monoid dec, Monoid r)
              => Translate m dec (ChordPitch anno pch dur) r
chordPitchU   = translate $ \ _ -> return mempty
