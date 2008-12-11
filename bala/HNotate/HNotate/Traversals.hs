{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Traversals
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Shape/contents separated traversals
--
--------------------------------------------------------------------------------


module HNotate.Traversals where

import HNotate.Duration
import HNotate.FocusedShapeContents
import HNotate.MusicRepDatatypes ( LabelSet(..), naturalize )
import HNotate.NoteListDatatypes
import HNotate.Pitch

import Control.Applicative
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Ratio
import qualified Data.Traversable as T

instance Applicative (State LyState) where
  pure = return
  (<*>) = ap 


--------------------------------------------------------------------------------
-- A state type for stateful LilyPond transformations 

data LyState = LyState { rel_pitch :: Pitch, rel_duration :: Duration }

mkLyState :: Pitch -> LyState
mkLyState pch = LyState { rel_pitch = pch, rel_duration = quarter }

--------------------------------------------------------------------------------
-- LilyPond
        
-- Just change the relative duration        
lyRelativeDuration :: T.Traversable t => t Grouping -> t Grouping
lyRelativeDuration = shapeContentsTraversal durationFocus fn 
  where
    fn xs = evalState (mapM diffDuration xs) (mkLyState c4)

durationFocus :: Focus Grouping Duration
durationFocus = FN { focus = not . isNplet, 
                     extract = rhythmicValue,
                     putback = modifyDuration }
  where
    isNplet (Nplet _ _ _ _)  = True
    isNplet _                = False


-- Change the relative pitch and relative duration.
-- Commonly Lilypond scores will be in this form
lyRelativePitchDuration :: T.Traversable t => Pitch -> t Grouping -> t Grouping 
lyRelativePitchDuration p = shapeContentsTraversal dlpcFocus fn where
    fn xs = evalState (mapM combinedUpdate xs) (mkLyState p)
    
    combinedUpdate :: (Duration, LyPitchContent) 
                          -> State LyState (Duration, LyPitchContent)
    combinedUpdate (d,lpc) = (,) <$> diffDuration d <*> pitchChange lpc

    
    
-- Translating to @relative@ mode for LilyPond cannot use PitchContent as is.
-- A note has a single pitch.
-- A chord has a set of pitches, but only the first one changes LilyPond's 
-- relative pitch state.
-- Tuplets and grace notes have sequences of pitches, any of which can 
-- change LilyPond's relative pitch state 
data LyPitchContent = RegularPitchContent PitchContent
                    | ChordPitches PitchContent

dlpcFocus :: Focus Grouping (Duration, LyPitchContent)
dlpcFocus = FN { focus = fcs, extract = ext, putback = back } 
  where
    fcs (Singleton a) = rhythmicValue a /= duration_zero
    fcs _             = True
    
    ext a@(Chord _ _ _) = (rhythmicValue a, ChordPitches $ pitchValue a)
    ext a               = (rhythmicValue a, RegularPitchContent $ pitchValue a)
   
    back a (d,RegularPitchContent pc) = modifyPitch (modifyDuration a d) pc
    back a (d,ChordPitches pc)        = modifyPitch (modifyDuration a d) pc 
    
    
   

pitchChange :: LyPitchContent -> State LyState LyPitchContent
pitchChange (ChordPitches xs)         = 
    ChordPitches . F.toList <$> convChordPitches xs 
    
pitchChange (RegularPitchContent xs)  = 
    RegularPitchContent <$> T.mapM convPitch xs




convChordPitches :: [Pitch] -> State LyState [Pitch]
convChordPitches [] = return []
convChordPitches (x:xs) = do 
    y  <- convPitch x
    ys <- localPitch $ mapM convPitch xs 
    return $ y:ys
  where    
    localPitch :: State LyState ans -> State LyState ans
    localPitch mf = do 
        initial <- gets rel_pitch
        ans     <- mf
        modify (\s -> s { rel_pitch = initial })
        return ans   

convPitch :: Pitch -> State LyState Pitch
convPitch p = do
    base <-  gets rel_pitch
    modify (\s -> s { rel_pitch = p })
    return $ p `changeOctaveWrt` base
     
changeOctaveWrt :: Pitch -> Pitch -> Pitch
changeOctaveWrt pch@(Pitch l a _) base = Pitch l a (base `octaveDist` pch)
    

diffDuration :: Duration -> State LyState Duration
diffDuration d = do
    old <- gets rel_duration
    if (old == d) then return no_duration
                  else modify (\s -> s {rel_duration=d}) >> return d

--------------------------------------------------------------------------------
-- Abc

dpFocus :: Focus Grouping (Duration, PitchContent)
dpFocus = FN { focus = fcs, extract = ext, putback = back } 
  where
    fcs (Singleton a) = rhythmicValue a /= duration_zero
    fcs _             = True
    
    ext a             = (rhythmicValue a, pitchValue a)
   
    back a (d,pc)     = modifyPitch (modifyDuration a d) pc
    
    
-- Abc needs two transformations:
-- Pitch needs relabeling according to the names in a scale.
-- Duration is scaled relative to the unit duration... 
-- If the duration is equal to the unit_duration then it is not printed,
-- so it is given the value duration_zero.      
abcPitchDurationTrafo :: 
      T.Traversable t => Duration -> LabelSet -> t Grouping -> t Grouping
abcPitchDurationTrafo unit_drn label_set = 
    shapeContentsTraversal dpFocus (fmap fn) 
  where
    fn (d,pc) = (recalcDuration d, relabelPitchContent label_set pc) 
    
    relabelPitchContent :: LabelSet -> PitchContent -> PitchContent
    relabelPitchContent ls pc = map (naturalize `flip` ls) pc  
    
    recalcDuration drn
        | drn == unit_drn = no_duration
        | otherwise       = scaleDuration drn unit_drn
                 
    
    scaleDuration :: Duration -> Duration -> Duration
    scaleDuration drn unl = 
        let (nr,dr)  = ratioElements drn
            (un,ud)  = ratioElements unl
        in  (nr%dr) / (un%ud)
                   