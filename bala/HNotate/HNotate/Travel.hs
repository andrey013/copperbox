
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Travel
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


module HNotate.Travel where

import HNotate.Duration
import HNotate.FocusedShapeContents
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.Traversals

import Control.Monad.State
import Data.Sequence
import qualified Data.Traversable as T


durationFocus :: Focus Grouping Duration
durationFocus = FN { focus = not . nplet, 
                     extract = rhythmicValue,
                     putback = modifyDuration }
  where
    nplet (Nplet _ _ _ _)  = True
    nplet _                = False

-- A note has a single pitch
-- A chord has a set of pitches, but only the first one changes 
-- LilyPond's relative pitch state
-- Tuplets and grace notes have sequences of pitches, all of which 
-- can change LilyPond's relative pitch state 
data LyPitchContainer = Unpitched
                      | SinglePitch Pitch
                      | ChordPitches (Seq Pitch) 
                      | PitchGroup (Seq Pitch)
    
dpcFocus :: Focus Grouping (Duration, LyPitchContainer)
dpcFocus = FN { focus = fcs, extract = ext, putback = back } 
  where
    fcs (Singleton a) = rhythmicValue a /= duration_zero
    fcs _             = True
    
    ext (Singleton a)         = 
        let opc = maybe Unpitched SinglePitch (pitchValue a)
        in (rhythmicValue a, opc)
    
    ext e@(Chord se _ _)      = (rhythmicValue e, ChordPitches se) 
    ext e@(GraceNotes se _ _) = (rhythmicValue e, PitchGroup $ fmap fst se)
    
    ext e@(Nplet _ _ se _)    = (rhythmicValue e, PitchGroup se) 
      
        
                        
    back (Singleton a)        (d, Unpitched)        = 
        Singleton (modifyDuration a d)
    
    back (Singleton a)        (d, SinglePitch p)    = 
        Singleton (modifyPitch (modifyDuration a d) p) 
    
    back (Chord _ _ a)        (d, ChordPitches se)  = Chord se d a
    
    back (GraceNotes se' m a) (d, PitchGroup se)    =
        GraceNotes (swapPitch se' se) m a    
    
    back (Nplet i ud _ a)     (d, PitchGroup se)    = Nplet i ud se a
    
    back _                    _                      = 
        error $ "urk should be unreachable..."                            
                             
    swapPitch se sp = step (viewl se) (viewl sp) where        
        step ((_,d) :< sa) (p :< sb)  = (p,d) <| step (viewl sa) (viewl sb)   
        step _             _          = empty


    
lyRelativePitchDuration :: T.Traversable t => Pitch -> t Grouping -> t Grouping 
lyRelativePitchDuration p = rejoin dpcFocus . change . separate dpcFocus
  where
    change (shape,contents) = (shape, contents') where
        contents' = evalState (mapM combinedUpdate contents) (mkLyState p)

combinedUpdate :: 
    (Duration, LyPitchContainer) -> State LyState (Duration, LyPitchContainer)
combinedUpdate (d,lpc) = do
    d'    <- diffDuration d
    lpc'  <- pitchChange lpc
    return (d',lpc')    

pitchChange :: LyPitchContainer -> State LyState LyPitchContainer
pitchChange Unpitched         = return Unpitched
pitchChange (SinglePitch p)   = convPitch p >>= return . SinglePitch  
pitchChange (ChordPitches se) = convChordPitches se >>= return . ChordPitches
pitchChange (PitchGroup se)   = T.mapM convPitch se >>= return . PitchGroup



                      