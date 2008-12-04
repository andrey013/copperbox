
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
import qualified Data.Foldable as F
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
data LyPitchContent = RegularPitchContent PitchContent
                    | ChordPitches PitchContent

    
dpcFocus :: Focus Grouping (Duration, LyPitchContent)
dpcFocus = FN { focus = fcs, extract = ext, putback = back } 
  where
    fcs (Singleton a) = rhythmicValue a /= duration_zero
    fcs _             = True
    
    ext a@(Chord _ _ _) = (rhythmicValue a, ChordPitches $ pitchValue a)
    ext a               = (rhythmicValue a, RegularPitchContent $ pitchValue a)
   
    back a (d,RegularPitchContent pc) = modifyPitch (modifyDuration a d) pc
    back a (d,ChordPitches pc)        = modifyPitch (modifyDuration a d) pc 
       


    
lyRelativePitchDuration :: T.Traversable t => Pitch -> t Grouping -> t Grouping 
lyRelativePitchDuration p = rejoin dpcFocus . change . separate dpcFocus
  where
    change (shape,contents) = (shape, contents') where
        contents' = evalState (mapM combinedUpdate contents) (mkLyState p)

combinedUpdate :: 
    (Duration, LyPitchContent) -> State LyState (Duration, LyPitchContent)
combinedUpdate (d,lpc) = do
    d'    <- diffDuration d
    lpc'  <- pitchChange lpc
    return (d',lpc')    

pitchChange :: LyPitchContent -> State LyState LyPitchContent
pitchChange (ChordPitches xs)         = 
    convChordPitches (fromList xs) >>= return . ChordPitches . F.toList
    
pitchChange (RegularPitchContent xs)  = 
    T.mapM convPitch xs >>= return . RegularPitchContent



                      