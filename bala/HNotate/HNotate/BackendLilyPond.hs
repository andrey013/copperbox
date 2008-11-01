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
import HNotate.Document
import HNotate.Duration hiding (duration)
import HNotate.Env
import HNotate.NotateMonad
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, gracenotes)
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
glyph (Note p d)          = note p d
glyph (Rest d)            = rest d
glyph (Spacer d)          = spacer d
glyph (Chord se d)        = chord (unseq se) d
glyph (GraceNotes se)     = gracenotes (unseq se)
glyph (Tie)               = tie
glyph (BeamStart)         = emptyDoc
glyph (BeamEnd)           = emptyDoc
glyph (Annotation fmt fn) = emptyDoc


polyphony :: Seq Bar -> ODoc
polyphony = dblangles' . step1 . viewl
  where 
    step1 EmptyL      = emptyDoc
    step1 (s :< se)   = rstep s (viewl se)
    
    rstep e EmptyL    = braces' (barDoc e)
    rstep e (s :< se) = braces' (barDoc e) <+> polysep <+> rstep s (viewl se)
    
    polysep :: ODoc
    polysep = text "\\\\"

type ODocConcat = ODoc -> ODoc -> ODoc

barDoc :: Bar -> ODoc
barDoc xs = collapse $ F.foldl fn (emptyDoc,(<+>),emptyDoc) xs
  where 
    collapse (out,op,tip) = out `op` tip
    
    fn :: (ODoc, ODocConcat, ODoc) -> Glyph -> (ODoc, ODocConcat, ODoc)
    fn (out,op,tip) (BeamStart)     = (out `op` tip, (<>),  anno bSt tip)
    fn (out,op,tip) (BeamEnd)       = (out `op` tip, (<+>), anno bEnd tip)
    fn (out,op,tip) (Annotation fmt fn) 
          | fmt == Ly               = (out,           op,   anno fn tip)
          | otherwise               = (out,           op,   tip)
    fn (out,op,tip) e               = (out `op` tip,  op,   glyph e)                 

anno :: (ODoc -> ODoc) -> ODoc -> ODoc
anno fn e | isEmpty e   = e
          | otherwise   = fn e
          
bSt :: (ODoc -> ODoc)
bSt   = (<> lbracket)
bEnd  = (<> rbracket) 


--------------------------------------------------------------------------------
-- pretty printers to 'ODoc'


note :: Pitch -> Duration -> ODoc
note p d = pitch p <> duration d

pitch :: Pitch -> ODoc
pitch (Pitch l a o) = octave o $ accidental a $ (char . toLowerLChar) l
  where
    accidental :: Accidental -> ODoc -> ODoc
    accidental Nat            = id
    accidental Sharp          = (<> text "is")
    accidental Flat           = (<> text "es")
    accidental DoubleSharp    = (<> text "isis")
    accidental DoubleFlat     = (<> text "eses")
     
    octave :: Int -> ODoc -> ODoc
    octave i | i > 0            = (<> text (replicate i '\''))
             | i < 0            = (<> text (replicate i ','))
             | otherwise        = id 
    
    
duration :: Duration -> ODoc
duration drn
    | drn == no_duration  = emptyDoc
    | otherwise           = let (n,d,dc) = pdElements $ printableDuration drn 
                            in dots dc $ durn n d
  where 
    durn 4 1      = command "longa"  
    durn 2 1      = command "breve" 
    durn 1 i      = int i
    -- TODO - we shouldn't have 'error' errors, we should be using throwError.
    -- But that means making a lot of pure code monadic - is there another 
    -- way to do it?
    -- Could we have Duration transformed to a 'fail-free' type by this point?  
    durn n d      = error $ "durationD failed on - " ++ show n ++ "%" ++ show d
    
    dots :: Int -> ODoc -> ODoc
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id
        

rest :: Duration -> ODoc
rest = (char 'r' <>) . duration
    
spacer :: Duration -> ODoc
spacer = (char 's' <>) . duration

chord :: [Pitch] -> Duration -> ODoc
chord ps d = (angles $ hsep $ map pitch ps)  <> duration d
  

gracenotes :: [Pitch] -> ODoc
gracenotes ps = command1 "grace" (braces $ hsep $ map pitch ps)


tie :: ODoc
tie = char '~'

barcheck :: ODoc
barcheck = char '|'

