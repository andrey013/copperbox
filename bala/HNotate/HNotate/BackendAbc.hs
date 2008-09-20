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
    translateAbc, AbcNoteList   
  ) where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.NoteList hiding (note, rest, spacer, chord, gracenotes)
import HNotate.Pitch
import HNotate.PrintAbc
import HNotate.PrintMonad
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ratio
import Data.Sequence hiding (take)
import Data.Traversable
import Text.PrettyPrint.Leijen (Doc, (<>), (<+>), text)
  
type AbcNoteList = Doc


unseq :: Seq a -> [a]
unseq = F.foldr (:) [] 

translateAbc :: ScoreNoteList -> Env -> AbcNoteList
translateAbc notes env =
    let abc_notes = abcForm notes env
    in  execPrintM (outputNoteList abc_notes) st0 

    
abcForm :: ScoreNoteList -> Env -> ScoreNoteList
abcForm se env = 
    runReader (unwrapMonad $ inner se) env
  where
    inner se = runReader (unwrapMonad $ unComp $ trav se) env 
    trav     = traverse (unleBody `comp` plrBody)
   
 
    
    
outputNoteList :: ScoreNoteList -> PrintM ()
outputNoteList (ScNoteList se) = F.mapM_ outputBlock se

outputBlock :: ScoreBlock -> PrintM ()
outputBlock (ScSingleBlock i s) = barNumber i >> outputMeasure s
outputBlock (ScPolyBlock i se)  = barNumber i >> outputVoiceOverlay se



outputMeasure :: ScoreMeasure -> PrintM ()
outputMeasure (ScMeasure se) = F.mapM_ outputGlyph se

outputGlyph :: ScoreGlyph -> PrintM ()
outputGlyph (SgNote p d)          = note p d
outputGlyph (SgRest d)            = rest d 
outputGlyph (SgSpacer d)          = spacer d
outputGlyph (SgChord se d)        = chord (unseq se) d
outputGlyph (SgGraceNotes se)     = gracenotes (unseq se)
outputGlyph (SgBeamStart)         = appendOp (<>)
outputGlyph (SgBeamEnd)           = appendOp (<+>)
outputGlyph (SgTie)               = tie



outputVoiceOverlay :: Seq ScoreMeasure -> PrintM ()
outputVoiceOverlay = step1 . viewl
  where 
    step1 EmptyL      = return ()
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = outputMeasure e >> barline
    rstep e (s :< se) = outputMeasure e >> voc >> rstep s (viewl se)
    
     
lastS EmptyL    = error "lastS EmptyL"
lastS (e :< se) = work e (viewl se)

work e EmptyL    = e
work _ (e :< se) = work e (viewl se)

barNumber :: Int -> PrintM ()
barNumber = comment . ("bar " ++) . show



  