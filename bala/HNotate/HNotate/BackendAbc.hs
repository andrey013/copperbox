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
import HNotate.Env (label_set, unit_note_length, Config(..))
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

translateAbc :: Monad m => 
    BarConcatFun -> AnnoEval -> NoteList -> NotateT m ODoc
translateAbc bf aeval notes = do
    abc_notes <- abcForm notes
    report $ outputNoteList bf aeval abc_notes
  where
    report :: Monad m => ODoc -> NotateT m ODoc
    report m = ask >>= \env ->
            witness 3 "Current environment is..." env >>
            witness 3 "Abc output..." m



abcForm :: Monad m => NoteList -> NotateT m NoteList
abcForm nl = (\ud ls -> abcPitchDurationTrafo ud ls nl) 
          <$> asks unit_note_length <*> asks label_set 
 

outputNoteList :: BarConcatFun -> AnnoEval -> NoteList -> ODoc 
outputNoteList bf ai = 
    bf . F.foldr ((:) `onl` blockDoc ai) [] . number 0 . getNoteList 


blockDoc :: AnnoEval -> (Int,Block) -> (Int,ODoc)
blockDoc ai (i, SingleBlock s)    = (i, barDoc ai s <+> barline)
blockDoc ai (i, OverlayBlock se)  = (i, voiceOverlay ai se <+> barline)


voiceOverlay :: AnnoEval -> Seq Bar -> ODoc
voiceOverlay ai = step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = barDoc ai e
    rstep e (s :< se) = barDoc ai e <+> voc <+> rstep s (viewl se)

type ODocConcat = ODoc -> ODoc -> ODoc
    
barDoc :: AnnoEval -> Bar -> ODoc
barDoc ai (Bar smw) = F.foldl fn emptyDoc smw
  where    
    fn :: ODoc -> MetricalWord -> ODoc
    fn a (Singleton e)   = a <+> element ai e
    fn a (BeamGroup se)  = a <+> hcat (fmap (element ai) se) 
    
    
  
element :: AnnoEval -> Element -> ODoc
element (AnnoEval _ _ h) (Atom e)              = atom h e

element (AnnoEval f g _) (Chord se d a)        =
    f HnChord a $ (brackets $ hcat $ ds) <> duration d
  where
    ds = fmap (\(p,a') -> g HnChord a' $ pitch p) se
            
element (AnnoEval f g _) (GraceNotes se a)     = 
    f HnGraceNotes a $ (braces $ hcat $ ds)
  where
    ds = fmap (\(p,d,a') -> g HnGraceNotes a' $ pitch p <> duration d) se  
    
element (AnnoEval f g _) (Nplet i ud se a)     =
    f HnNplet a $ pqr <+> (hcat $ ds)
  where
    ds = fmap (\(p,a') -> g HnNplet a' $ pitch p) se
    
    pqr = lparen <> int (length se)       <> colon 
                 <> duration (scale ud)   <> colon
                 <> int (length se)
    
    scale :: Duration -> Duration
    scale drn = drn * (makeDuration i 1)
    
    

-- Note ABC tie is dash!
atom :: (HnAtom -> Annotation -> ODocS) -> Atom -> ODoc
atom f (Note p d a)          = f HnNote a $ note p d
atom f (Rest d a)            = f HnRest a $ rest d
atom f (Spacer d a)          = f HnSpacer a $ spacer d
atom _ (RhythmicMark _ d m)  = abcOutput m <> duration d
atom _ (Mark _ m)            = abcOutput m
atom _ Tie                   = char '-'       





--------------------------------------------------------------------------------
-- Pretty printers

-- Ones that may be useful to /document view/ should be defined in DocAbc



note :: Pitch -> Duration -> ODoc 
note p d = pitch p <> duration d


rest :: Duration -> ODoc
rest d = char 'z' <> duration d
    
spacer :: Duration -> ODoc
spacer d = char 'x' <> duration d

comment :: String -> ODoc
comment s = text $ '%':' ':s  

barline :: ODoc
barline = char '|'

-- voc - voice overlay continuation
voc :: ODoc
voc = text "&\\"



    


  