{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit LilyPond from Score representation.
--
--------------------------------------------------------------------------------

module HNotate.BackendLilyPond (
    translateLilyPond
  ) where


import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, gracenotes)
import HNotate.Pitch
import HNotate.PrintLy
import HNotate.PrintMonad
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import Data.Traversable
import Text.PrettyPrint.Leijen (Doc)

  

translateLilyPond :: Env -> NoteList -> NoteListOutput
translateLilyPond env =
    (execPrintM `flip` pmZero) . outputNoteList . lilypondRelativeForm env

updatePitch st p = st { rel_pitch = p }

-- Do we need a state type, like this one?
-- data LyState = LyState { rel_pitch :: Pitch, rel_dur :: Duration }

lilypondRelativeForm :: Env -> NoteList -> NoteList
lilypondRelativeForm env = (evalState `flip` st) . unwrapMonad . inner 
  where       
    inner   = (evalState `flip` st) . unwrapMonad 
                                    . unComp 
                                    . traverse (proBody `comp` drleBody)

    st      = lyState0 `updatePitch` (relative_pitch env)
    
outputNoteList :: NoteList -> PrintM ()
outputNoteList (NoteList se) = F.mapM_  outputBlock se

outputBlock :: Block -> PrintM ()
outputBlock (SingleBlock i s) = barNumberCheck i >> outputMeasure s >> barcheck
outputBlock (PolyBlock i se)  = barNumberCheck i >> polyphony se >> barcheck

    
outputMeasure :: Bar -> PrintM ()
outputMeasure (Bar se) = F.mapM_ outputGlyph se

outputGlyph :: Glyph -> PrintM ()
outputGlyph (Note p d)          = note p d
outputGlyph (Rest d)            = rest d
outputGlyph (Spacer d)          = spacer d
outputGlyph (Chord se d)        = chord (unseq se) d 
outputGlyph (GraceNotes se)     = gracenotes (unseq se)
outputGlyph (BeamStart)         = return ()
outputGlyph (BeamEnd)           = return ()
outputGlyph (Tie)               = tie

polyphony :: Seq Bar -> PrintM ()
polyphony = step1 . viewl
  where 
    step1 EmptyL      = return ()
    step1 (s :< se)   = polystart >> rstep s (viewl se)
    
    rstep e EmptyL    = outputMeasure e >> polyend
    rstep e (s :< se) = outputMeasure e >> polyc >> rstep s (viewl se)
    

{-
polyphony :: LyCxt_Element -> [LyCxt_Element] -> LyCxt_Element
polyphony cxt (a:b:xs)  = polywork ((cxt +++ openPoly) `mappend` a) b xs
polyphony cxt [a]       = cxt `mappend` a
polyphony cxt []        = cxt


polywork cxt x []     = (cxt \\ x) +++ closePoly
polywork cxt x (y:ys) = polywork (cxt \\ x) y ys
-} 


    
    

    
{-

barNumber :: Int -> PrintM ()
barNumber = barNumberCheck


addBarline :: LyCxt_Element -> LyCxt_Element
addBarline cxt = cxt +++ suffixLinebreak barcheck
-}
