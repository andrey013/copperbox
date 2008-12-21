{-# OPTIONS -Wall #-}

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
    abcConcat, translateAbc, abcForm
  ) where

import HNotate.CommonUtils
import HNotate.DocAbc
import HNotate.Document
import HNotate.Duration
import HNotate.Env (label_set, unit_note_length)
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, 
                                         nplet, element)
import HNotate.NotateMonad
import HNotate.Pitch
import HNotate.PPInstances () -- for witness instances
import HNotate.ProcessingBase
import HNotate.Traversals



import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Sequence hiding (take)

import Prelude hiding (length)  



-- TODO - variants of this to support bar numbering, etc.
abcConcat :: BarConcatFun
abcConcat = vsep . map snd

translateAbc :: Monad m => BarConcatFun -> NoteList -> NotateT m ODoc
translateAbc bf = fwd <=< printStep <=< abcForm  
  where
    printStep = return . outputNoteList bf
    
    fwd m = ask >>= \env ->
            witness 3 "Current environment is..." env >>
            witness 3 "Abc output..." m



abcForm :: Monad m => NoteList -> NotateT m NoteList
abcForm nl = (\ud ls -> abcPitchDurationTrafo ud ls nl) 
          <$> asks unit_note_length <*> asks label_set 
 

outputNoteList :: BarConcatFun -> NoteList -> ODoc 
outputNoteList bf = 
    bf . F.foldr ((:) `onl` blockDoc) [] . number 0 . getNoteList 


blockDoc :: (Int,Block) -> (Int,ODoc)
blockDoc (i, SingleBlock s)    = (i, barDoc s <+> barline)
blockDoc (i, OverlayBlock se)  = (i, voiceOverlay se <+> barline)


voiceOverlay :: Seq Bar -> ODoc
voiceOverlay = step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = barDoc e
    rstep e (s :< se) = barDoc e <+> voc <+> rstep s (viewl se)

type ODocConcat = ODoc -> ODoc -> ODoc
    
barDoc :: Bar -> ODoc
barDoc (Bar smw) = F.foldl fn emptyDoc smw
  where    
    fn :: ODoc -> MetricalWord -> ODoc
    fn a (Singleton e)   = a <+> element e
    fn a (BeamGroup se)  = a <+> hcat (fmap element se) 
    
    
  
element :: Element -> ODoc
element (Atom e)              = atom e
element (Chord se d a)        = chord se d a
element (GraceNotes se a)     = gracenotes se a
element (Nplet i ud se a)     = nplet i ud se a


-- Note ABC tie is dash!
atom :: Atom -> ODoc
atom (Note p d a)          = applyAbcAnno a $ note p d
atom (Rest d a)            = applyAbcAnno a $ rest d
atom (Spacer d a)          = applyAbcAnno a $ spacer d
atom (RhythmicMark _ d m)  = abcOutput m <> duration d
atom (Mark _ m)            = abcOutput m
atom Tie                   = char '-'       





--------------------------------------------------------------------------------
-- Pretty printers

-- Ones that may be useful to /document view/ should be defined in DocAbc



note :: Pitch -> Duration -> ODoc 
note p d = pitch p <> duration d


rest :: Duration -> ODoc
rest d = char 'z' <> duration d
    
spacer :: Duration -> ODoc
spacer d = char 'x' <> duration d

chord :: Seq (Pitch,Annotation) -> Duration -> Annotation -> ODoc
chord se d a = 
    applyAbcAnno a (brackets $ hcat $ smapl (pitch . fst) se) <> duration d
    
gracenotes :: Seq (Pitch,Duration,Annotation) -> Annotation -> ODoc
gracenotes se a = applyAbcAnno a (braces $ hcat $ smapl fn se)
  where fn (p,d,_) = pitch p <> duration d  

nplet :: Int -> Duration -> Seq (Pitch,Annotation) -> Annotation -> ODoc
nplet mult ud se a = 
    applyAbcAnno a $ pqr <+> (hcat $ smapl (pitch . fst) se)
  where
    pqr = lparen <> int (length se)       <> colon 
                 <> duration (scale ud)   <> colon
                 <> int (length se)
    
    scale :: Duration -> Duration
    scale drn = drn * (makeDuration mult 1)

comment :: String -> ODoc
comment s = text $ '%':' ':s  

barline :: ODoc
barline = char '|'

-- voc - voice overlay continuation
voc :: ODoc
voc = text "&\\"



    


  