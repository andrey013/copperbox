{-# OPTIONS -Wall #-}

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
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, 
                                         nplet, element)
import HNotate.Pitch
import HNotate.PPInstances () -- get Witness instances
import HNotate.ProcessingBase
import HNotate.Traversals

import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Sequence

import Prelude hiding (length)
  
lyConcat :: BarConcatFun
lyConcat = vsep . map snd


translateLilyPond :: Monad m => 
    BarConcatFun -> AnnoEval -> NoteList -> NotateT m ODoc
translateLilyPond bf aeval nl = report $ outputNoteList bf aeval nl 
  where  
    report :: Monad m => ODoc -> NotateT m ODoc
    report m = do env <- ask
                  witness "Current environment is..." env
                  witness "LilyPond output..." m
            

lilypondAbsoluteForm :: Monad m => NoteList -> NotateT m NoteList
lilypondAbsoluteForm = return . lyRelativeDurationAbsPitch
 
lilypondRelativeForm :: Monad m => NoteList -> NotateT m NoteList
lilypondRelativeForm evts = getRelativePitch >>= \p -> 
    return $ lyRelativePitchDuration p evts  
    


getRelativePitch :: Monad m => NotateT m Pitch
getRelativePitch = asks relative_pitch >>= maybe no_rp_err (return)
  where
    no_rp_err = throwError $ strMsg msg
    msg       = "ERROR - \\relative output directive used, but no " ++
                "relative pitch detected in the current scope."
  
    
outputNoteList :: BarConcatFun -> AnnoEval -> NoteList -> ODoc 
outputNoteList bf ai = 
  bf . F.foldr ((:) `onl` blockDoc ai) [] . number 0 . getNoteList 


blockDoc :: AnnoEval -> (Int,Block) -> (Int,ODoc)
blockDoc ai (i, SingleBlock s)    = (i, barDoc ai s <+> barcheck)
blockDoc ai (i, OverlayBlock se)  = (i, polyphony ai se <+> barcheck)

    




polyphony :: AnnoEval -> Seq Bar -> ODoc
polyphony ai = dblangles' . step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = braces' (barDoc ai e)
    rstep e (s :< se) = braces' (barDoc ai e) <+> polysep <+> rstep s (viewl se)
    
    polysep :: ODoc
    polysep = text "\\\\"



barDoc :: AnnoEval -> Bar -> ODoc
barDoc ai (Bar smw) = F.foldl fn emptyDoc smw
  where    
    fn :: ODoc -> MetricalWord -> ODoc
    fn a (Singleton e)   = a <+> element ai e
    fn a (BeamGroup se)  = a <+> printBeamed (viewl se)
    
    printBeamed EmptyL    = emptyDoc
    printBeamed (e :< se) = element ai e <>  lbracket 
                                         <+> hsep (fmap (element ai) se) 
                                         <>  rbracket 

element :: AnnoEval -> Element -> ODoc
element (AnnoEval _ _ h) (Atom e)          = atom h e
element (AnnoEval f g _) (Chord se d a)    = 
    f HnChord a $ angles (hsep ds) <> duration d
  where
    ds = fmap (\(p,a') -> g HnChord a' $ pitch p) se
    
element (AnnoEval f g _) (GraceNotes se a) =
    f HnGraceNotes a $ command1 "grace" (braces $ hsep ds)
  where 
    ds = fmap (\(p,d,a') -> g HnGraceNotes a' $ pitch p <> duration d) se
  
element (AnnoEval f g _) (Nplet i ud se a) =
    f HnNplet a $ command1 "times" fract <+> braces' (hsep $ plet1 (viewl se))
  where
    fract = int i <> char '/' <> int (length se)
    
    plet1 EmptyL          = []
    plet1 ((p,a') :< sp)  = (g HnNplet a' $ note p ud) : plet (viewl sp)
    
    plet ((p,a') :< sp)   = (g HnNplet a' $ pitch p) : plet (viewl sp)
    plet EmptyL           = []
    
    

atom :: (HnAtom -> Annotation -> ODocS) -> Atom -> ODoc
atom f (Note p d a)          = f HnNote a $ note p d
atom f (Rest d a)            = f HnRest a $ rest d
atom f (Spacer d a)          = f HnSpacer a $ spacer d
atom _ (RhythmicMark _ d m)  = lyOutput m <> duration d
atom _ (Mark _ m)            = lyOutput m
atom _ Tie                   = char '~'



--------------------------------------------------------------------------------
-- Pretty printers

-- Ones that may be useful to /document view/ should be defined in DocLilyPond


note :: Pitch -> Duration -> ODoc
note p d = pitch p <> duration d

rest :: Duration -> ODoc
rest = (char 'r' <>) . duration
    
spacer :: Duration -> ODoc
spacer = (char 's' <>) . duration
    
barcheck :: ODoc
barcheck = char '|'
