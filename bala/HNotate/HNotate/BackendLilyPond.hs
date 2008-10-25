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
import HNotate.NotateMonad
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, gracenotes)
import HNotate.Pitch
import HNotate.PrettyInstances
import HNotate.PrintLy
import HNotate.PrintMonad
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import Data.Traversable
import Text.PrettyPrint.Leijen (Doc, char)

  

translateLilyPond :: Monad m => NoteList -> NotateT m NoteListOutput
translateLilyPond = fwd <=< printStep <=< lilypondRelativeForm
  where
    printStep = return . (execPrintM `flip` pmZero) . outputNoteList
    
    fwd m = ask >>= \env ->
            witness 3 "Current environment is..." env >>
            witness 3 "LilyPond output..." m

updatePitch st p = st { rel_pitch = p }

-- Do we need a state type, like this one?
-- data LyState = LyState { rel_pitch :: Pitch, rel_dur :: Duration }

lilypondRelativeForm :: Monad m => NoteList -> NotateT m NoteList
lilypondRelativeForm evts = asks relative_pitch >>= \p -> 
    return $ (evalState `flip` st p) $ unwrapMonad $ inner p $ evts
  where       
    inner p = (evalState `flip` st p) . unwrapMonad 
                                      . unComp 
                                      . traverse (proBody `comp` drleBody)

    st p     = lyState0 `updatePitch` p
    
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
    
    rstep e EmptyL    = bracedMeasure e >> polyend
    rstep e (s :< se) = bracedMeasure e >> polyc >> rstep s (viewl se)
    
bracedMeasure e = element (char '{') >> outputMeasure e >> element (char '}')      
    

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
