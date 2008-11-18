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
    lyConcat, 
    translateLilyPond, 
    lilypondRelativeForm, 
    lilypondAbsoluteForm
  ) where


import HNotate.CommonUtils
import HNotate.DocLilyPond
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.NotateMonad
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord)
import HNotate.Pitch
import HNotate.PPInstances
import HNotate.ProcessingTypes
import HNotate.Traversals

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Foldable as F
import Data.Sequence
import Data.Traversable

  
lyConcat :: BarConcatFun
lyConcat = vsep . map snd


translateLilyPond :: Monad m => 
          BarConcatFun -> NoteListPostProcessFun m -> NoteList -> NotateT m ODoc
translateLilyPond bf procF = fwd <=< printStep <=< procF
  where
    printStep = return . outputNoteList bf
    
    fwd m = ask >>= \env ->
            witness 3 "Current environment is..." env >>
            witness 3 "LilyPond output..." m

lilypondAbsoluteForm :: Monad m => NoteList -> NotateT m NoteList
lilypondAbsoluteForm = return . traverseIdentity losBody
    


lilypondRelativeForm :: Monad m => NoteListPostProcessFun m
lilypondRelativeForm evts = getRelativePitch >>= \p -> 
    return $ (evalState `flip` mkLyState p) $ unwrapMonad $ inner p $ evts
  where       
    inner p = (evalState `flip` mkLyState p) 
                      . unwrapMonad 
                      . unComp 
                      . traverse (proBody `comp` drleBody)



getRelativePitch :: Monad m => NotateT m Pitch
getRelativePitch = asks relative_pitch >>= maybe no_rp_err (return)
  where
    no_rp_err = throwError $ strMsg msg
    msg       = "ERROR - \\relative output directive used, but no " ++
                "relative pitch detected in the current scope."
  
    
outputNoteList :: BarConcatFun -> NoteList -> ODoc 
outputNoteList bf = bf . F.foldr ((:) `onl` blockDoc) [] . getNoteList 


blockDoc :: Block -> (Int,ODoc)
blockDoc (SingleBlock i s) = (i, barDoc s <+> barcheck)
blockDoc (PolyBlock i se)  = (i, polyphony se <+> barcheck)

    


glyph :: Glyph -> ODoc
glyph (Note p d a)          = applyLyAnno a $ note p d
glyph (Rest Marked d a)     = applyLyAnno a $ rest d
glyph (Rest Spacer d a)     = applyLyAnno a $ spacer d

glyph (RhythmicMark _ d m)  = lyOutput m <> duration d
glyph (Mark _ m)            = lyOutput m


polyphony :: Seq Bar -> ODoc
polyphony = dblangles' . step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = braces' (barDoc e)
    rstep e (s :< se) = braces' (barDoc e) <+> polysep <+> rstep s (viewl se)
    
    polysep :: ODoc
    polysep = text "\\\\"


-- Unlike the Abc version of this function, the LilyPond version doesn't
-- need to have an 'ODocConcat' function as part of its state. 

barDoc :: Bar -> ODoc
barDoc = collapse . F.foldl fn (emptyDoc,emptyDoc)
  where 
    collapse (out,tip) = out <+> tip
    
    fn :: (ODoc, ODoc) -> Tile -> (ODoc, ODoc)
    fn (out,tip) (Singleton e)   
          | isBeamStart e             = (out <+> tip, anno bSt tip)
          | isBeamEnd e               = (out <+> tip, anno bEnd tip)
          | otherwise                 = (out <+> tip, glyph e)
          
    fn (out,tip) (Chord se d a)       = (out <+> tip, chord se d a)
    fn (out,tip) (GraceNotes se m a)  = (out <+> tip, gracenotes se a) 
                

    anno :: (ODoc -> ODoc) -> ODoc -> ODoc
    anno fn e | isEmpty e   = e
              | otherwise   = fn e
          
    bSt, bEnd :: (ODoc -> ODoc)
    bSt   = (<> lbracket)
    bEnd  = (<> rbracket) 


--------------------------------------------------------------------------------
-- Pretty printers

-- Ones that may be useful to /document view/ should be defined in DocLilyPond


note :: Pitch -> Duration -> ODoc
note p d = pitch p <> duration d

rest :: Duration -> ODoc
rest = (char 'r' <>) . duration
    
spacer :: Duration -> ODoc
spacer = (char 's' <>) . duration

chord :: Seq Pitch -> Duration -> Annotation -> ODoc
chord ps d a = 
    applyLyAnno a $ (angles $ hsep $ unseqMap pitch ps)  <> duration d
  

gracenotes :: Seq (Pitch,Duration) -> Annotation -> ODoc
gracenotes ps a = 
    applyLyAnno a $ command1 "grace" (braces $ hsep $ unseqMap fn ps)
  where fn (p,d) = pitch p <> duration d


tie :: ODoc
tie = char '~'

barcheck :: ODoc
barcheck = char '|'

