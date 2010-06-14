{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.AbcTrafo
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pitch spelling and duration multiplier transformations
--
--------------------------------------------------------------------------------


module Neume.Core.AbcTrafo
  (

  -- * Pitch spelling transformation
    AbcPitchSpellTrafo(..)
  , AbcPitchSpellStep(..)
  , runPitchSpellTrafo


  -- * Duration multiplier transformation
  , DurMult
  , AbcDurMultTrafo(..)
  , AbcDurMultStep(..)
  , runDurMultTrafo

  ) where

import Neume.Core.Duration
import Neume.Core.ModularSyntax
import Neume.Core.Pitch
import Neume.Core.SpellingMap

import MonadLib.Monads                  -- package: monadLib

import Control.Applicative
import qualified Data.Traversable as T



--------------------------------------------------------------------------------

type PitchSpellTrafo a = Reader AbcSpellingMap a
 


runPitchSpellTrafo :: (AbcPitchSpellTrafo repr, AbcPitchSpellStep gly)
                   => AbcSpellingMap -> repr gly -> repr gly
runPitchSpellTrafo sm = runReader sm . abcPitchSpellTrafo


class AbcPitchSpellTrafo repr where
  abcPitchSpellTrafo :: AbcPitchSpellStep gly => 
                        repr gly -> PitchSpellTrafo (repr gly)

class AbcPitchSpellStep gly where
  spStep :: gly -> PitchSpellTrafo gly


instance AbcPitchSpellTrafo Full where
  abcPitchSpellTrafo (Full (Phrase bars)) = 
      (Full . Phrase) <$> mapM (mapM (T.mapM spStep)) bars
      
instance AbcPitchSpellTrafo Undiv where
  abcPitchSpellTrafo (Undiv (Phrase bars)) = 
      (Undiv . Phrase) <$> mapM (T.mapM spStep) bars

      
instance AbcPitchSpellTrafo Unmetered where
  abcPitchSpellTrafo (Unmetered (Phrase mdivs)) = 
      (Unmetered . Phrase) <$> mapM (T.mapM spStep) mdivs
      

instance AbcPitchSpellStep (Glyph anno Pitch dur) where
  spStep (GlyNote n d t)  = (\n' -> GlyNote n' d t) <$> spStep n
  spStep (Rest    d)      = return $ Rest d
  spStep (Spacer  d)      = return $ Spacer d
  spStep (Chord   xs d t) = (\xs' -> Chord xs' d t) <$> T.mapM spStep xs
  spStep (Graces  xs)     = Graces <$> T.mapM spStep xs


instance AbcPitchSpellStep (Note anno Pitch) where
  spStep (Note a p) = (\p' -> Note a p') <$> spellPitch p

instance AbcPitchSpellStep (GraceNote anno Pitch dur) where
  spStep (GraceNote a p d) = (\p' -> GraceNote a p' d) <$> spellPitch p

spellPitch :: Pitch -> PitchSpellTrafo Pitch
spellPitch p = (\sm -> spell sm p) <$> ask

--------------------------------------------------------------------------------


type DurMultTrafo a = Reader Rational a

type family DurMult gly :: *

type instance DurMult (Glyph anno pch Duration) = Glyph anno pch AbcMultiplier

type instance DurMult (GraceNote anno pch Duration) = 
                       GraceNote anno pch AbcMultiplier


runDurMultTrafo :: ( AbcDurMultTrafo repr, AbcDurMultStep gly
                   , gly' ~ DurMult gly)
                => Rational -> repr gly -> repr gly'
runDurMultTrafo r = runReader r . abcDurMultTrafo


-- Note -- the monadic effect is not at the top level of this 
-- class. We want to start each bar with the first note printing
-- it\'s duration.

class AbcDurMultTrafo repr where
   abcDurMultTrafo :: ( AbcDurMultStep gly, gly' ~ DurMult gly) 
                   => repr gly -> DurMultTrafo (repr gly')


class AbcDurMultStep gly where
   dmStep :: gly' ~ DurMult gly => gly -> DurMultTrafo gly'


instance AbcDurMultTrafo Full where
  abcDurMultTrafo (Full (Phrase bars)) = 
      (Full . Phrase) <$> mapM (T.mapM (T.mapM dmStep)) bars


instance AbcDurMultTrafo Undiv where
  abcDurMultTrafo (Undiv (Phrase bars)) = 
      (Undiv . Phrase) <$> mapM (T.mapM dmStep) bars
      

instance AbcDurMultTrafo Unmetered where
  abcDurMultTrafo (Unmetered (Phrase mdivs)) =
     (Unmetered . Phrase) <$> mapM (T.mapM dmStep) mdivs


instance AbcDurMultStep (Glyph anno pch Duration) where
  dmStep (GlyNote n d t)  = (\d' -> GlyNote n d' t) <$> durationMult d
  dmStep (Rest    d)      = (\d' -> Rest d')        <$> durationMult d
  dmStep (Spacer  d)      = (\d' -> Spacer d')      <$> durationMult d
  dmStep (Chord   xs d t) = (\d' -> Chord xs d' t)  <$> durationMult d
  dmStep (Graces  xs)     = Graces  <$> T.mapM dmStep xs


instance AbcDurMultStep (GraceNote anno pch Duration) where
  dmStep (GraceNote a p d) = (\d' -> GraceNote a p d') <$> durationMult d


-- Never set a dotted duration as the state...
durationMult :: Duration -> DurMultTrafo AbcMultiplier
durationMult dur = (\r -> fn $ abcRepresentation r dur) <$> ask
  where
    fn = maybe IdenM id
