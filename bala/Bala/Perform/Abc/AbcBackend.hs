{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.AbcBackend
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from AbcScore.
--
--------------------------------------------------------------------------------

module Bala.Perform.Abc.AbcBackend (
  Perform_Abc_Env(..), default_abc_env,
  generateAbcScore 
  ) where

import Bala.Format.Output.OutputAbc
import Bala.Perform.Abc.AbcScoreDatatypes
import Bala.Perform.Base.Datatypes
import Bala.Perform.Base.PerformMonad
import Bala.Perform.Score.Utils

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldlM,foldrM)
import Data.Maybe (catMaybes)
import Data.Ratio

type ProcessM a = PerformM Perform_Abc_State Perform_Abc_Env a



data Perform_Abc_State = Perform_Abc_State { 
    abc_unknown_st      :: ()
  }  
  deriving (Show)
  
data Perform_Abc_Env= Perform_Abc_Env {
    default_note_length      :: Duration
  }

state0 = Perform_Abc_State ()

default_abc_env :: Perform_Abc_Env
default_abc_env = Perform_Abc_Env {
    default_note_length        = quarternote
  }
  
    
infixl 7 *!
(*!) e oa   = maybe e (e !) oa

infixl 7 !*>
(!*>) oa e   = maybe e ((flip (!>)) e) oa


abcPitchLetter   :: Pitch -> AbcPitchLetter
abcPitchLetter = toEnum . fromEnum . pitch_letter



oabcAccidental :: Pitch -> Maybe AbcAccidental
oabcAccidental = fn . accidental
  where 
    fn Nat            = Nothing
    fn Sharp          = Just sharp
    fn Flat           = Just flat 
    fn DoubleSharp    = Just doubleSharp
    fn DoubleFlat     = Just doubleFlat


suffixWith :: Append cxts cxta
           => Abc cxts 
           -> ProcessM (Abc cxta) 
           -> ProcessM (Abc cxts)
suffixWith ctx f = (ctx +++) <$> f 



type EltS = AbcCxt_Body -> AbcCxt_Body


suffix :: (Append cxts cxta) => Abc cxta -> (Abc cxts -> Abc cxts)
suffix = flip (+++)

suffixA :: (Append cxts cxta, Functor f) 
        => f (Abc cxta) 
        -> f (Abc cxts -> Abc cxts)
suffixA f = suffix <$> f

concatS :: [a -> a] -> a -> a
concatS = foldr ( #. ) id

-- foldlOp :: (Functor f) => (t -> f (b -> c)) -> (a -> b) -> t -> f (a -> c)
foldlOpA f op = \acc e -> (acc `op`) <$> f e 


-- first step get the original Abc rendering working
-- then worry if we should return a function or not 
generateAbcScore :: AbcScTuneBook -> Perform_Abc_Env -> [AbcCxt_Body]
generateAbcScore sc env = evalPerform (renderTuneBook sc) abc_state env
  where 
    abc_state = state0
    
    
renderTuneBook (AbcScTuneBook se) = 
    foldlM (foldlOpA renderTune (flip (:))) [] se

renderTune :: AbcScTune -> ProcessM AbcCxt_Body                 
renderTune (AbcScTune i se) = foldlM renderPolyPhrase body se


renderPolyPhrase :: AbcCxt_Body -> AbcScPolyPhrase -> ProcessM AbcCxt_Body
renderPolyPhrase cxt (AbcScSingletonPhrase x) = ( cxt # ) <$> renderMeasure x
renderPolyPhrase cxt (AbcScPolyPhrase xs) = 
    error "Abc renderPolyPhrase - to do" -- foldlM (foldlOpA renderMeasure (flip ( #. ))) id xs


            
renderMeasure :: (Append cxts AbcGraceNotesT,
                  Append cxts AbcChordT,
                  Append cxts AbcNoteT,
                  Append cxts AbcRestT)
              => AbcScMeasure
              -> ProcessM (Abc cxts -> Abc cxts)
            
renderMeasure (AbcScMeasure i xs se) = foldlM (foldlOpA renderGlyph ( #. )) id se




-- Glyphs may generate elements of different types, hence we 
-- return a 'suffix function' instead...
-- Also we have the very general return type ...(Abc cxts -> Abc cxts) 
-- rather than ...EltS as we might need to think about beaming
-- which prints note without separating spaces at some point.
renderGlyph :: (Append cxts AbcGraceNotesT,
                Append cxts AbcChordT,
                Append cxts AbcNoteT,
                Append cxts AbcRestT)
            => AbcScGlyph
            -> ProcessM (Abc cxts -> Abc cxts)

renderGlyph (AbcScNote scp d)            = suffixA $
    (*!) <$> renderPitch scp  <*> abcDuration d



renderGlyph (AbcScRest d)                = suffixA $
    (rest *!)   <$> abcDuration d
    
renderGlyph (AbcScSpacer d)               = suffixA $ 
    (spacer *!) <$> abcDuration d


-- Notes inside chords have duration (though they all _should_ be the same)
renderGlyph (AbcScChord xs dur)               = suffixA $ 
    chord <$> foldrM (pitch1 dur) [] xs  

  where
    pitch1 d scp acc  = fn acc <$> renderPitch scp <*> abcDuration d
    
    fn acc p od = p *! od : acc 


renderGlyph (AbcScGraceNotes xs)          = suffixA $ 
    gracenotes <$> mapM graceNote xs
   
    

graceNote :: (Pitch,Duration) -> ProcessM AbcNote
graceNote (scp,d)  = (*!) <$> renderPitch scp  <*> abcDuration d

  

-- | @AbcScPitch --> AbcNote@
renderPitch :: Pitch -> ProcessM AbcNote 
renderPitch pch = 
    fn <$> pure (abcPitchLetter pch) <*> pure (oabcAccidental pch) 
  where
    fn pn oa = oa !*> (note pn)   
    
abcDuration :: Duration -> ProcessM (Maybe AbcDuration)
abcDuration d = fn d <$> asks default_note_length
  where
    fn dur1 deft
      | dur1 == deft  = Nothing
      | otherwise     = let scale = denominator (toRatio deft)
                            r     = toRatio dur1
                            (n,d) = (numerator r, denominator r)    
                        in Just $ dur ( n*scale, d)