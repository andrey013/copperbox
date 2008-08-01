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
-- Emit Abc from Score.
--
--------------------------------------------------------------------------------

module Bala.Perform.Abc.AbcBackend where

import Bala.Format.Output.OutputAbc
import Bala.Perform.Abc.Class
import Bala.Perform.Abc.AbcScoreDatatypes
import Bala.Perform.Base.PerformMonad
import Bala.Perform.Score.Utils

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldlM,foldrM)
import Data.Maybe (catMaybes)
import Data.Ratio

type ProcessM pch dur a = 
       PerformM (Perform_Abc_State) (Perform_Abc_Env dur) a



data Perform_Abc_State = Perform_Abc_State { 
    abc_unknown_st      :: ()
  }  
  deriving (Show)
  
data Perform_Abc_Env dur = Perform_Abc_Env {
    default_note_length      :: dur
  }

state0 = Perform_Abc_State ()

default_abc_env :: DurationAbc dur => Perform_Abc_Env dur 
default_abc_env = Perform_Abc_Env {
    default_note_length        = quaternoteAbc
  }
  
    
infixl 7 *!
(*!) e oa   = maybe e (e !) oa

infixl 7 !*>
(!*>) oa e   = maybe e ((flip (!>)) e) oa




suffixWith :: Append cxts cxta
           => Abc cxts 
           -> ProcessM pch dur (Abc cxta) 
           -> ProcessM pch dur (Abc cxts)
suffixWith ctx f = (ctx +++) <$> f 



type EltS = AbcCxt_Element -> AbcCxt_Element


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
generateAbcScore :: (PitchAbc pch, DurationAbc dur)
                      => AbcScTuneBook pch dur
                      -> Perform_Abc_Env dur 
                      -> [AbcCxt_Body]
generateAbcScore sc env = evalPerform (renderTuneBook sc) abc_state env
  where 
    abc_state = state0
    
    
renderTuneBook (AbcScTuneBook se) = 
    foldlM (foldlOpA renderTune (flip (:))) [] se

renderTune :: (PitchAbc pch, DurationAbc dur)
           => AbcScTune pch dur 
           -> ProcessM pch dur AbcCxt_Body                 
renderTune (AbcScTune i se) = -- foldlM (foldlOpA renderPolyPhrase (flip (:))) [] se
    (body +++) <$> foldlM renderPolyPhrase elementStart se


renderPolyPhrase :: (PitchAbc pch, DurationAbc dur)
                 => AbcCxt_Element
                 -> AbcScPolyPhrase pch dur 
                 -> ProcessM pch dur AbcCxt_Element
              
renderPolyPhrase cxt (AbcScSingletonPhrase x) = ( cxt # ) <$> renderMeasure x
renderPolyPhrase cxt (AbcScPolyPhrase xs) = 
    undefined -- foldlM (foldlOpA renderMeasure (flip ( #. ))) id xs


            
renderMeasure :: (PitchAbc pch, 
                  DurationAbc dur, 
                  Append cxts AbcGraceNotesT,
                  Append cxts AbcChordT,
                  Append cxts AbcNoteT,
                  Append cxts AbcRestT)
              => AbcScMeasure pch dur 
              -> ProcessM pch dur (Abc cxts -> Abc cxts)
            
renderMeasure (AbcScMeasure i xs se) = foldlM (foldlOpA renderGlyph ( #. )) id se




-- Glyphs may generate elements of different types, hence we 
-- return a 'suffix function' instead...
-- Also we have the very general return type ...(Abc cxts -> Abc cxts) 
-- rather than ...EltS as we might need to think about beaming
-- which prints note without separating spaces at some point.
renderGlyph :: (PitchAbc pch, 
                DurationAbc dur, 
                Append cxts AbcGraceNotesT,
                Append cxts AbcChordT,
                Append cxts AbcNoteT,
                Append cxts AbcRestT)
            => AbcScGlyph pch dur 
            -> ProcessM pch dur (Abc cxts -> Abc cxts)

renderGlyph (AbcScNote scp d)            = suffixA $
    (*!) <$> renderPitch scp  <*> abcDuration d



renderGlyph (AbcScRest d)                = suffixA $
    (rest *!)   <$> abcDuration d
    
renderGlyph (AbcScSpacer d)               = suffixA $ 
    (spacer *!) <$> abcDuration d


-- Notes inside chords have duration (though they all _should_ be the same)
renderGlyph (AbcScChord xs)               = suffixA $ 
    chord <$> foldrM pitch1 [] xs  

  where
    pitch1 (AbcScNote scp d) acc = fn acc <$> renderPitch scp <*> abcDuration d
    pitch1 _                 acc = pure acc
    
    fn acc p od = p *! od : acc 


renderGlyph (AbcScGraceNotes xs)          = suffixA $ 
    gracenotes . catMaybes <$> mapM justNote xs
   
    
getPitch (AbcScNote scp _)  = Just scp
getPitch _                  = Nothing

justNote :: (PitchAbc pch, DurationAbc dur) 
         => AbcScGlyph pch dur 
         -> ProcessM pch dur (Maybe AbcNote)
justNote (AbcScNote scp d)  = fn <$> renderPitch scp  <*> abcDuration d
  where fn p od = Just $ p *! od
  
justNote _                  = pure Nothing

-- | @AbcScPitch --> AbcNote@
renderPitch :: PitchAbc pch => pch -> ProcessM pch dur (AbcNote) 
renderPitch pch = 
    fn <$> pure (abcPitchLetter pch) <*> pure (abcAccidental pch) 
  where
    fn pn oa = oa !*> (note pn)   
    
abcDuration :: DurationAbc dur => dur -> ProcessM pch dur (Maybe AbcDuration)
abcDuration d = fn d <$> asks default_note_length
  where
    fn dur1 deft
      | dur1 == deft  = Nothing
      | otherwise     = let scale = denominator (asRational deft)
                            r     = asRational dur1
                            (n,d) = (numerator r, denominator r)    
                        in Just $ dur ( n*scale, d)