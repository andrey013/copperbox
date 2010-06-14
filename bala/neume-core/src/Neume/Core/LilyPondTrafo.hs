{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondTrafo
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Relative / absolute pitch transformation and relative duration
-- transformations.
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondTrafo
  (
  -- * Relative pitch transformation
    LyRelPitchTrafo(..)
  , LyRelPitchStep(..)
  , runRelPitchTrafo

  -- * Absolute pitch transformation
  , LyAbsPitchTrafo(..)
  , LyAbsPitchStep(..)
  , runAbsPitchTrafo

  -- * Relative duration transformation
  , OptDur
  , LyRelDurTrafo(..)
  , LyRelDurStep(..)
  , runLyRelDurTrafo

  ) where

import Neume.Core.Duration
import Neume.Core.ModularSyntax
import Neume.Core.Pitch
import Neume.Core.Utils.OneList

import MonadLib.Monads                  -- package: monadLib

import Control.Applicative
import qualified Data.Traversable as T

-- pitch transforms don't change type...

type RelPitchTrafo a = State Pitch a


-- Note - need to supply the initial pitch, this permits 
-- scores made of multiple phrases.
--

runRelPitchTrafo :: (LyRelPitchTrafo repr, LyRelPitchStep gly)
                 => Pitch -> repr gly -> (repr gly,Pitch)
runRelPitchTrafo p = runState p . lyRelPitchTrafo 

class LyRelPitchTrafo repr where
   lyRelPitchTrafo :: LyRelPitchStep gly => repr gly -> RelPitchTrafo (repr gly)

class LyRelPitchStep gly where
   rpStep :: gly -> RelPitchTrafo gly

instance LyRelPitchTrafo Full where
  lyRelPitchTrafo (Full (Phrase name bars)) =  
      (Full . Phrase name) <$> mapM rpBarMD bars

instance LyRelPitchTrafo Undiv where
  lyRelPitchTrafo (Undiv (Phrase name bars)) = 
      (Undiv . Phrase name) <$> mapM rpBar bars 

instance LyRelPitchTrafo Unmetered where
  lyRelPitchTrafo (Unmetered (Phrase name mdivs)) = 
      (Unmetered . Phrase name) <$>  mapM rpMetricalDiv mdivs


rpBarMD :: LyRelPitchStep gly
        => Bar (MetricalDiv gly) -> RelPitchTrafo (Bar (MetricalDiv gly))
rpBarMD = mapM rpMetricalDiv

rpBar :: LyRelPitchStep gly => Bar gly -> RelPitchTrafo (Bar gly)
rpBar = mapM rpStep


rpMetricalDiv :: LyRelPitchStep gly
              => MetricalDiv gly -> RelPitchTrafo (MetricalDiv gly)
rpMetricalDiv (Atom e)       = Atom      <$> rpStep e
rpMetricalDiv (Beamed    xs) = Beamed    <$> mapM rpMetricalDiv xs
rpMetricalDiv (N_Plet mp xs) = N_Plet mp <$> mapM rpMetricalDiv xs 




instance LyRelPitchStep (Glyph anno Pitch dur) where
  rpStep (GlyNote n d t)  = (\n' -> GlyNote n' d t) <$> rpStep n
  rpStep (Rest    d)      = return $ Rest d
  rpStep (Spacer  d)      = return $ Spacer d
  rpStep (Chord   xs d t) = (\xs' -> Chord xs' d t) <$> rpChordNotes xs
  rpStep (Graces  xs)     = Graces <$> T.mapM rpStep xs


instance LyRelPitchStep (Note anno Pitch) where
  rpStep (Note a p) = (\p' -> Note a p') <$> relativePitch p

instance LyRelPitchStep (GraceNote anno Pitch dur) where
  rpStep (GraceNote a p d) = (\p' -> GraceNote a p' d) <$> relativePitch p


-- | The second and later notes of a chord are renamed relative 
-- to the first note. But globally, further notes, chords etc. 
-- in the note list are relative to the first chord note and
-- not the last chord note.
--
-- (AM I SURE ABOUT THIS?)
--
rpChordNotes :: OneList (Note anno Pitch) 
             -> RelPitchTrafo (OneList (Note anno Pitch))
rpChordNotes = step1 . viewl where
  step1 (OneL n)   = one <$> rpStep n
  step1 (x :<< xs) = do { y       <- rpStep x
                        ; pitch1  <- get 
                        ; ys      <- T.mapM rpStep xs
                        ; set pitch1
                        ; return (cons y ys)
                        }


-- | Need to return the \original\ pitch as the state, not the
-- octave modified new value.
--
relativePitch :: Pitch -> RelPitchTrafo Pitch
relativePitch p = get     >>= \ prev ->
                  set p   >>
                  return (setOctave (lyOctaveDist prev p) p)

--------------------------------------------------------------------------------

type AbsPitchTrafo a = Reader (Octave -> Octave) a
 


runAbsPitchTrafo :: (LyAbsPitchTrafo repr, LyAbsPitchStep gly)
                 => (Octave -> Octave) -> repr gly -> repr gly
runAbsPitchTrafo fn = runReader fn . lyAbsPitchTrafo


class LyAbsPitchTrafo repr where
  lyAbsPitchTrafo :: LyAbsPitchStep gly => repr gly -> AbsPitchTrafo (repr gly)

class LyAbsPitchStep gly where
  apStep :: gly -> AbsPitchTrafo gly


instance LyAbsPitchTrafo Full where
  lyAbsPitchTrafo (Full (Phrase name bars)) = 
      (Full . Phrase name) <$> mapM (mapM (T.mapM apStep)) bars
      
instance LyAbsPitchTrafo Undiv where
  lyAbsPitchTrafo (Undiv (Phrase name bars)) = 
      (Undiv . Phrase name) <$> mapM (T.mapM apStep) bars

      
instance LyAbsPitchTrafo Unmetered where
  lyAbsPitchTrafo (Unmetered (Phrase name mdivs)) = 
      (Unmetered . Phrase name) <$> mapM (T.mapM apStep) mdivs
      

instance LyAbsPitchStep (Glyph anno Pitch dur) where
  apStep (GlyNote n d t)  = (\n' -> GlyNote n' d t) <$> apStep n
  apStep (Rest    d)      = return $ Rest d
  apStep (Spacer  d)      = return $ Spacer d
  apStep (Chord   xs d t) = (\xs' -> Chord xs' d t) <$> T.mapM apStep xs
  apStep (Graces  xs)     = Graces <$> T.mapM apStep xs


instance LyAbsPitchStep (Note anno Pitch) where
  apStep (Note a p) = (\p' -> Note a p') <$> absolutePitch p

instance LyAbsPitchStep (GraceNote anno Pitch dur) where
  apStep (GraceNote a p d) = (\p' -> GraceNote a p' d) <$> absolutePitch p

absolutePitch :: Pitch -> AbsPitchTrafo Pitch
absolutePitch (Pitch l oa o) = (\f -> Pitch l oa (f o)) <$> ask

--------------------------------------------------------------------------------


type RelDurTrafo a = State (Maybe Duration) a

type family OptDur gly :: *

type instance OptDur (Glyph anno pch Duration) = Glyph anno pch (Maybe Duration)
type instance OptDur (Graphic gly Duration)    = Graphic gly (Maybe Duration)

runLyRelDurTrafo :: (LyRelDurTrafo repr, LyRelDurStep gly, gly' ~ OptDur gly)
                 => repr gly -> repr gly'
runLyRelDurTrafo = lyRelDurTrafo


-- Note -- the monadic effect is not at the top level of this 
-- class. We want to start each bar with the first note printing
-- it\'s duration.

class LyRelDurTrafo repr where
   lyRelDurTrafo :: ( LyRelDurStep gly
                    , gly' ~ OptDur gly) => repr gly -> repr gly'


class LyRelDurStep gly where
   rdStep :: gly' ~ OptDur gly => gly -> RelDurTrafo gly'

instance LyRelDurTrafo Full where
  lyRelDurTrafo (Full (Phrase name bars)) = 
      Full $ Phrase name (map rdBarMD bars)
      
instance LyRelDurTrafo Undiv where
  lyRelDurTrafo (Undiv (Phrase name bars)) = 
      Undiv $ Phrase name (map rdBar bars)
      
instance LyRelDurTrafo Unmetered where
  lyRelDurTrafo (Unmetered (Phrase name mdivs)) = Unmetered $ Phrase name mdivs'
    where
      mdivs' = fst $ runState Nothing (mapM rdMetricalDiv mdivs)
      


instance LyRelDurStep (Glyph anno pch Duration) where
  rdStep (GlyNote n d t)  = (\d' -> GlyNote n d' t) <$> relativeDuration d
  rdStep (Rest    d)      = (\d' -> Rest d')        <$> relativeDuration d
  rdStep (Spacer  d)      = (\d' -> Spacer d')      <$> relativeDuration d
  rdStep (Chord   xs d t) = (\d' -> Chord xs d' t)  <$> relativeDuration d
  rdStep (Graces  xs)     = (Graces $ fmap fn xs)   <$  set Nothing
    where
      fn (GraceNote a p d) = GraceNote a p (Just d)



instance LyRelDurStep (Graphic gly Duration) where
  rdStep (Graphic g d)  = (\d' -> Graphic g d') <$> relativeDuration d
  rdStep (Skip    d)    = (\d' -> Skip d')      <$> relativeDuration d
 
rdBarMD :: (LyRelDurStep gly, gly' ~ OptDur gly) 
        => Bar (MetricalDiv gly) -> Bar (MetricalDiv gly')
rdBarMD bar = fst $ runState Nothing (mapM rdMetricalDiv bar)

rdBar :: (LyRelDurStep gly, gly' ~ OptDur gly) 
      => Bar gly -> Bar gly'
rdBar bar = fst $ runState Nothing (mapM rdStep bar)

rdMetricalDiv :: (LyRelDurStep gly, gly' ~ OptDur gly)
              => MetricalDiv gly -> RelDurTrafo (MetricalDiv gly')
rdMetricalDiv (Atom e)       = Atom      <$> rdStep e
rdMetricalDiv (N_Plet mp xs) = N_Plet mp <$> mapM rdMetricalDiv xs 
rdMetricalDiv (Beamed    xs) = Beamed    <$> mapM rdMetricalDiv xs


-- Never set a dotted duration as the state...
relativeDuration :: Duration -> RelDurTrafo (Maybe Duration)
relativeDuration dur = 
    get              >>= \ prev ->
    updateState  dur >>
    return (fn dur prev)
  where
    fn d (Just prev) | prev == d = Nothing 
    fn d _                       = Just d

    updateState d    | isDotted d = set Nothing 
    updateState d                 = set (Just d)
