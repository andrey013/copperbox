{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from Score representation.
--
--------------------------------------------------------------------------------


module HNotate.BackendAbc (
    translateAbc   
  ) where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, gracenotes)
import HNotate.Pitch
import HNotate.PrintAbc
import HNotate.PrintMonad
import HNotate.Transformations
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (take)
import Data.Traversable
import Text.PrettyPrint.Leijen (Doc, (<>), (<+>), text)
  





translateAbc :: Env -> NoteList -> NoteListOutput
translateAbc env = (execPrintM `flip` pmZero) . outputNoteList 
                                              . beamNoteList env 
                                              . abcForm env 

    
abcForm :: Env -> NoteList -> NoteList
abcForm env = (runReader `flip` env) . unwrapMonad . inner
  where
    inner   = (runReader `flip` env) . unwrapMonad 
                                     . unComp 
                                     . traverse (unleBody `comp` plrBody)
   
 
    
    
outputNoteList :: NoteList -> PrintM ()
outputNoteList (NoteList se) = F.mapM_ outputBlock se

outputBlock :: Block -> PrintM ()
outputBlock (SingleBlock i s) = barNumber i >> outputMeasure s >> barline
outputBlock (PolyBlock i se)  = barNumber i >> outputVoiceOverlay se >> barline



outputMeasure :: Bar -> PrintM ()
outputMeasure (Bar se)         = F.mapM_ outputGlyph se

outputGlyph :: Glyph -> PrintM ()
outputGlyph (Note p d)          = note p d
outputGlyph (Rest d)            = rest d
outputGlyph (Spacer d)          = spacer d
outputGlyph (Chord se d)        = chord (unseq se) d
outputGlyph (GraceNotes se)     = gracenotes (unseq se)
outputGlyph (BeamStart)         = appendOp (<>)
outputGlyph (BeamEnd)           = appendOp (<+>)
outputGlyph (Tie)               = tie



outputVoiceOverlay :: Seq Bar -> PrintM ()
outputVoiceOverlay = step1 . viewl
  where 
    step1 EmptyL      = return ()
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = outputMeasure e >> barline
    rstep e (s :< se) = outputMeasure e >> voc >> rstep s (viewl se)
    

barNumber :: Int -> PrintM ()
barNumber = comment . ("bar " ++) . show



  