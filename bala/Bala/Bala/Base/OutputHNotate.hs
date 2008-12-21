{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.OutputHNotate
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Output Midi for Structural2
--
--------------------------------------------------------------------------------



module Bala.Base.OutputHNotate where

import Bala.Base.DrumOutput
import Bala.Base.Pitch
import Bala.Base.Structural

import qualified HNotate as H
import qualified HNotate.Marks as H
import qualified HNotate.NoteListDatatypes as H
import HNotate ( ( # ) )

import Control.Applicative 
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.Sequence

type EventFoldStep = H.EventList -> Event -> H.EventList
data Env = Env { _fold_step :: EventFoldStep }
type OutputM a = Reader Env a


instance Applicative (Reader Env) where
  pure  = return
  (<*>) = ap

generateEventList :: Section -> H.EventList
generateEventList = genGenerateEventList motifFoldStep


genGenerateEventList :: EventFoldStep -> Section -> H.EventList
genGenerateEventList fstep sn = 
    runReader (F.foldlM phraseFoldStep H.root se) (Env fstep)    
  where
    (Section _ se) = packToLength sn
    
       

phraseFoldStep :: H.EventList -> Phrase -> OutputM H.EventList
phraseFoldStep t (Single mo)      = addMotif t mo
phraseFoldStep t (Overlay mo smo) = 
    (\x xs -> t # H.poly (x:xs)) <$> f mo <*> ovs (viewl smo)
  where
    ovs :: ViewL Motif -> OutputM [H.EventList]
    ovs EmptyL     = return []
    ovs (a :< sa)  = (:) <$> f a <*> ovs (viewl sa)
    
    f :: Motif -> OutputM H.EventList
    f m = addMotif H.root m
    

addMotif :: H.EventList -> Motif -> OutputM H.EventList
addMotif t (Motif se) = 
  (\fstep -> F.foldl fstep t se) <$> asks _fold_step


motifFoldStep :: H.EventList -> Event -> H.EventList
motifFoldStep t (NoteE p d)         = t # (H.note p d)
motifFoldStep t (RestE d)           = t # (H.rest d)
motifFoldStep t (ChordE se d)       = t # (H.chord se d)
motifFoldStep t (SpacerE d)         = t # (H.spacer d)
motifFoldStep t (AGraceE se p d)    = t # H.graces se # H.note p d
motifFoldStep t (UGraceE p d se)    = t # H.note p d  # H.graces se
motifFoldStep t (MarkE m)           = t # (mkMark m)

mkMark :: Mark -> H.EventList -> H.EventList
mkMark Tie          = H.tie 
    


genDrumFoldStep :: DrumMapping a =>
    (a -> H.Mark H.DrumMark) -> H.EventList -> Event -> H.EventList
genDrumFoldStep fn t (NoteE p d)          = 
    maybe (t # H.spacer d) (\name -> t # H.drumnote (fn name) d) (drumName p)    
genDrumFoldStep fn t (ChordE se d)        = 
    case drumChord fn (F.toList se) of
        []   -> t # H.spacer d
        [x]  -> t # H.drumnote x d
        xs   -> t # H.drumchord xs d
         
genDrumFoldStep fn t a                    = motifFoldStep t a



drumChord :: DrumMapping a => 
    (a -> H.Mark H.DrumMark) -> [Pitch] -> [H.Mark H.DrumMark]
drumChord fn = map fn . catMaybes . map drumName




            