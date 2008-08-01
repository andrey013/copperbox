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
import Bala.Format.Score.PolyDatatypes

import Bala.Perform.Base.PerformMonad
import Bala.Perform.Abc.Class

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (foldlM,foldrM)
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
  
infixl 7 *!
(*!) e oa   = maybe e (e !) oa

infixl 7 !*>
(!*>) oa e   = maybe e ((flip (!>)) e) oa

-- reverse compose
( #. ) :: (a -> b) -> (b -> c) -> a -> c
f #. g = g . f


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


renderScore (PScScore se) = foldlM (foldlOpA renderPart (flip (:))) [] se

renderPart (PScPart i se) = foldlM (foldlOpA renderPolyUnit (flip (:))) [] se


renderPolyUnit (PScPolyUnit xs) = foldlM (foldlOpA renderSegment (flip (:))) [] xs

-- return a list of measure making functions so we can think about barlines
renderSegment :: (PitchAbc pch, 
                  DurationAbc dur, 
                  Append cxts AbcGraceNotesT,
                  Append cxts AbcChordT,
                  Append cxts AbcNoteT,
                  Append cxts AbcRestT)
              => PScSegment pch dur 
              -> ProcessM pch dur [(Abc cxts -> Abc cxts)] 
renderSegment (PScSegment se) = foldlM (foldlOpA renderMeasure (flip (:))) [] se

            
renderMeasure :: (PitchAbc pch, 
                  DurationAbc dur, 
                  Append cxts AbcGraceNotesT,
                  Append cxts AbcChordT,
                  Append cxts AbcNoteT,
                  Append cxts AbcRestT)
              => PScMeasure pch dur 
              -> ProcessM pch dur (Abc cxts -> Abc cxts)
            
renderMeasure (PScMeasure i xs se) = foldlM (foldlOpA renderGlyph ( #. )) id se




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
            => PScGlyph pch dur 
            -> ProcessM pch dur (Abc cxts -> Abc cxts)

renderGlyph (PScNote scp d)            = suffixA $
    (*!) <$> renderPitch scp  <*> abcDuration d



renderGlyph (PScRest d)                = suffixA $
    (rest *!)   <$> abcDuration d
    
renderGlyph (PScSpacer d)              = suffixA $ 
    (spacer *!) <$> abcDuration d


-- Notes inside chords have duration (though they all _should_ be the same)
renderGlyph a@(PScGroup PScChord xs)    = suffixA $ 
    chord <$> foldrM pitch1 [] xs  

  where
    pitch1 (PScNote scp d) acc = fn acc <$> renderPitch scp <*> abcDuration d
    pitch1 _               acc = pure acc
    
    fn acc p od = p *! od : acc 


renderGlyph (PScGroup PScGraceNotes xs) = suffixA $ 
    gracenotes <$> mapM (renderPitch . getPitch) xs
    
    
getPitch (PScNote scp _) = scp



-- | @PScPitch --> AbcNote@
renderPitch :: PitchAbc pch => PScPitch pch -> ProcessM pch dur (AbcNote) 
renderPitch (PScPitch pch) = 
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