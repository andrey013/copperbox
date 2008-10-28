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
import HNotate.Document
import HNotate.Duration hiding (duration)
import HNotate.Env
import HNotate.NoteListDatatypes hiding (note, rest, spacer, chord, gracenotes)
import HNotate.NotateMonad
import HNotate.Pitch
import HNotate.PPInstances
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
    
barDoc :: Bar -> ODoc
barDoc xs = collapse $ F.foldl fn (emptyDoc,(<+>), emptyDoc) xs
  where collapse (out,op,tip) = out `op` tip
  

fn (out,op,tip) (BeamStart)         = (out `op` tip, (<>),  emptyDoc)
fn (out,op,tip) (BeamEnd)           = (out `op` tip, (<+>), emptyDoc)
fn (out,op,tip) (Annotation fmt fn) 
      | fmt == Abc                  = (out,           op,   anno fn tip)
      | otherwise                   = (out,           op,   tip)
fn (out,op,tip) e                   = (out `op` tip,  op,   glyph e)                 

anno :: (ODoc -> ODoc) -> ODoc -> ODoc
anno fn e | isEmpty e   = e
          | otherwise   = fn e
                  
   



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



--------------------------------------------------------------------------------
-- pretty printers to 'ODoc'



note :: Pitch -> Duration -> ODoc 
note p d = pitch p <> duration d


pitch :: Pitch -> ODoc
pitch (Pitch l a o) 
    | o > 4     = accidental a $ octave o $ (char . toLowerLChar) l
    | otherwise = accidental a $ octave o $ (char . toUpperLChar) l
  where     
    letter :: PitchLetter -> Char
    letter = fn . show
      where fn [x] = x
            fn xs  = error $ "letter " ++ xs 

    accidental :: Accidental -> ODoc -> ODoc 
    accidental Nat           = id    
    accidental Sharp         = (char '^'  <>)
    accidental Flat          = (char '_'  <>)
    accidental DoubleSharp   = (text "^^" <>)
    accidental DoubleFlat    = (text "__" <>)
   
    octave :: Int -> ODoc -> ODoc
    octave i  | i > 5       = (<> text (replicate (i-5) '\'')) 
              | i < 4       = (<> text (replicate (4-i) ','))
              | otherwise   = id



duration :: Duration -> ODoc
duration d | d == no_duration = emptyDoc
           | otherwise        = fn $ ratioElements $ convRational d
  where
    fn (n,1) = int n
    fn (1,d) = char '/' <> int d
    fn (n,d) = int n <> char '/' <> int d


rest :: Duration -> ODoc
rest d = char 'z' <> duration d
    
spacer :: Duration -> ODoc
spacer d = char 'x' <> duration d

chord :: [Pitch] -> Duration -> ODoc
chord ps d = (brackets $ hcat $ map pitch ps) <> duration d
    
gracenotes :: [Pitch] -> ODoc
gracenotes = braces . hcat . map pitch  

tie :: ODoc
tie = char '-'

comment :: String -> ODoc
comment s = text $ '%':' ':s  



barline :: ODoc
barline = char '|'


-- voc - voice overlay continuation
voc :: ODoc
voc = text "&\\"



    


  