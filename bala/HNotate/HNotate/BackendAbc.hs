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
    abcConcat, translateAbc 
  ) where

import HNotate.CommonUtils
import HNotate.DocAbc
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord)
import HNotate.NotateMonad
import HNotate.Pitch
import HNotate.PPInstances
import HNotate.ProcessingTypes
import HNotate.Transformations
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Ratio
import Data.Sequence hiding (take)
import Data.Traversable
  



-- TODO - variants of this to support bar numbering, etc.
abcConcat :: BarConcatFun
abcConcat = vsep . map snd

translateAbc :: Monad m => BarConcatFun -> NoteList -> NotateT m ODoc
translateAbc bf = fwd <=< printStep <=< beamNoteList <=< abcForm 
  where
    printStep = return . outputNoteList bf
    
    fwd m = ask >>= \env ->
            witness 3 "Current environment is..." env >>
            witness 3 "Abc output..." m



abcForm :: Monad m => NoteList -> NotateT m NoteList
abcForm = unwrapMonad <=< inner  
  where
    inner = unwrapMonad . unComp . traverse (unleBody `comp` plrBody)

outputNoteList :: BarConcatFun -> NoteList -> ODoc 
outputNoteList bf = bf . F.foldr ((:) `onl` blockDoc) [] . getNoteList 


blockDoc :: Block -> (Int,ODoc)
blockDoc (SingleBlock i s) = (i, barDoc s <+> barline)
blockDoc (PolyBlock i se)  = (i, voiceOverlay se <+> barline)


voiceOverlay :: Seq Bar -> ODoc
voiceOverlay = step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = barDoc e
    rstep e (s :< se) = barDoc e <+> voc <+> rstep s (viewl se)

type ODocConcat = ODoc -> ODoc -> ODoc
    
barDoc :: Bar -> ODoc
barDoc = collapse . F.foldl fn (emptyDoc,(<+>),emptyDoc)
  where 
    collapse (out,op,tip) = out `op` tip
  
    fn :: (ODoc, ODocConcat, ODoc) -> Tile -> (ODoc, ODocConcat, ODoc)
    fn (out,op,tip) (Singleton e)   
          | isBeamStart e               = (out `op` tip, (<>),  emptyDoc)
          | isBeamEnd e                 = (out `op` tip, (<+>), emptyDoc)
          | otherwise                   = (out `op` tip,  op,   glyph e)
          
    fn (out,op,tip) (Chord se d a)      = (out `op` tip,  op, chord se d a)
    fn (out,op,tip) (GraceNotes se m a) = (out `op` tip,  op, gracenotes se a)
     
                                
                  
   



glyph :: Glyph -> ODoc
glyph (Note p d a)          = applyAbcAnno a $ note p d
glyph (Rest Marked d a)     = applyAbcAnno a $ rest d
glyph (Rest Spacer d a)     = applyAbcAnno a $ spacer d

glyph (RhythmicMark _ d m)  = abcOutput m <> duration d
glyph (Mark _ m)            = abcOutput m




--------------------------------------------------------------------------------
-- Pretty printers

-- Ones that may be useful to /document view/ should be defined in DocAbc



note :: Pitch -> Duration -> ODoc 
note p d = pitch p <> duration d


rest :: Duration -> ODoc
rest d = char 'z' <> duration d
    
spacer :: Duration -> ODoc
spacer d = char 'x' <> duration d

chord :: Seq Pitch -> Duration -> Annotation -> ODoc
chord se d a = 
    applyAbcAnno a (brackets $ hcat $ unseqMap pitch se) <> duration d
    
gracenotes :: Seq (Pitch,Duration) -> Annotation -> ODoc
gracenotes se a = applyAbcAnno a (braces $ hcat $ unseqMap fn se)
  where fn (p,d) = pitch p <> duration d  

tie :: ODoc
tie = char '-'

comment :: String -> ODoc
comment s = text $ '%':' ':s  

barline :: ODoc
barline = char '|'

-- voc - voice overlay continuation
voc :: ODoc
voc = text "&\\"



    


  