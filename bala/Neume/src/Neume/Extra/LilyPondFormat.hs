{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.LilyPondFormat
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Formatting operations for bars (repeats etc.)
--
--------------------------------------------------------------------------------


module Neume.Extra.LilyPondFormat
  (

  
    Ly_Std_Format_Config(..)
  , Ly_Relative_Rewrite_Config(..)
  , Ly_Absolute_Rewrite_Config(..)

  , barNumber

  , renderLyRelative
  , renderLyRelative_overlay2  -- ugly prototype
  , renderLyDrums


  , parallelPhrases


  ) where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic ( pitch, spacer )
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.NoteList
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.Extended
import Neume.Extra.LilyPondDoc hiding ( score )


import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.List ( foldl' )

data Ly_Std_Format_Config = Ly_Std_Format_Config
    { bar_numbering_func   :: BarNum -> DocS }


type OctaveDisplacement = Int


data Ly_Relative_Rewrite_Config = Ly_Relative_Rewrite_Config
    { base_pitch                :: Pitch
    , meter_pattern_rel         :: MeterPattern 
    }

data Ly_Absolute_Rewrite_Config = Ly_Absolute_Rewrite_Config
    { octave_displacement       :: OctaveDisplacement
    , meter_pattern_abs         :: MeterPattern 
    }



barNumber :: BarNum -> DocS
barNumber i = (lineComment ("Bar " ++ show i) <$>)


renderLyRelative :: Ly_Std_Format_Config
                 -> Ly_Relative_Rewrite_Config
                 -> [Section (NoteList StdGlyph)] 
                 -> Doc
renderLyRelative (Ly_Std_Format_Config func) rw1 = 
    concatDocSections func . fst . stmap renderSectionRel rw1


renderSectionRel :: Ly_Relative_Rewrite_Config
                 -> Section (NoteList StdGlyph) 
                 -> (Section PhraseImage, Ly_Relative_Rewrite_Config)
renderSectionRel cfg = stmap_extr extr (phraseImageRel mp) cfg
  where
    extr = Extractable base_pitch (\p c -> c {base_pitch = p}) 
    mp   = meter_pattern_rel cfg


renderLyRelative_overlay2 :: Duration 
                          -> Ly_Std_Format_Config
                          -> Ly_Relative_Rewrite_Config
                          -> Ly_Relative_Rewrite_Config
                          -> [Section (NoteList StdGlyph, NoteList StdGlyph)] 
                          -> Doc
renderLyRelative_overlay2 bar_len (Ly_Std_Format_Config func) rw1 rw2 = 
    concatDocSections func . map (merge2 bar_len) 
                           . fst . stmap renderSectionRel2 (rw1,rw2)

merge2 :: Duration -> Section (PhraseImage, PhraseImage) -> Section PhraseOverlayImage
merge2 bar_len = fmap (\(x,y) -> parallelPhrases bar_len [x,y])

renderSectionRel2 :: (Ly_Relative_Rewrite_Config,Ly_Relative_Rewrite_Config)
                  -> Section (NoteList StdGlyph, NoteList StdGlyph)
                  -> (Section (PhraseImage, PhraseImage), 
                      (Ly_Relative_Rewrite_Config,Ly_Relative_Rewrite_Config))
renderSectionRel2 (cfg1,cfg2) = 
    stmap_extr2 extrA extrB (phraseImageRel mpA) (phraseImageRel mpB) (cfg1,cfg2)
  where
    extrA = Extractable (base_pitch . fst) (\p (c,d) -> (c {base_pitch = p}, d)) 
    mpA   = meter_pattern_rel cfg1
    extrB = Extractable (base_pitch . snd) (\p (c,d) -> (c, d {base_pitch = p})) 
    mpB   = meter_pattern_rel cfg2



phraseImageRel :: MeterPattern
               -> Pitch
               -> NoteList (Glyph anno Pitch Duration)
               -> (PhraseImage,Pitch)
phraseImageRel mp pch = 
    fmap2a (renderPhrase pitch) . lyRelativeRewrite pch . phrase mp


renderLyDrums :: Ly_Std_Format_Config
              -> MeterPattern 
              -> [Section (NoteList DrumGlyph)] 
              -> Doc
renderLyDrums (Ly_Std_Format_Config func) mp = 
    concatDocSections func . map (renderSectionDrums mp) 


renderSectionDrums :: MeterPattern 
                   -> Section (NoteList DrumGlyph)
                   -> Section PhraseImage
renderSectionDrums mp = fmap (phraseImageDrums mp) 


phraseImageDrums :: MeterPattern
                 -> NoteList DrumGlyph
                 -> PhraseImage
phraseImageDrums mp = 
    renderPhrase (text . drumShortName) . rewriteDurationOpt . phrase mp




concatDocSections :: (BarNum -> DocS) -> [Section PhraseImage] -> Doc
concatDocSections fn = vsep . fst . stmap section1 1  
  where
    section1 :: BarNum -> Section PhraseImage -> (Doc,BarNum)
    section1 n (Straight a)       = flatStraight  fn n a
    section1 n (Repeated a)       = flatRepeated  fn n a 
    section1 n (AltRepeat a alts) = flatAltRepeat fn n a alts

--------------------------------------------------------------------------------
 

-- The internal argument to renderPhrase should be extracted 
-- from these and made a prarameter...
--
-- Like wise they need to handle an anacrusis 
--

{-
lyPhraseAbsolute :: MeterPattern -> [StdGlyph] -> Int -> PhraseImage
lyPhraseAbsolute mp xs n = 
    renderPhrase pitch $ rewritePitchAbs n
                       $ rewriteDurationOpt
                       $ phrase mp
                       $ simpleNoteList xs

lyPhraseDrums :: MeterPattern -> [DrumGlyph] -> PhraseImage
lyPhraseDrums mp xs = 
    renderPhrase (text . drumShortName) $ rewriteDurationOpt
                                        $ phrase mp
                                        $ simpleNoteList xs
-}
    
--------------------------------------------------------------------------------

flatStraight :: (BarNum -> DocS) -> BarNum -> [BarImage] -> (Doc,BarNum)
flatStraight = flatBars

flatRepeated :: (BarNum -> DocS) -> BarNum -> [BarImage] -> (Doc,BarNum)
flatRepeated = fmap2a (repeatvolta 2) `ooo` flatBars

flatAltRepeat :: (BarNum -> DocS) 
              -> BarNum
              -> [BarImage] 
              -> [[BarImage]] 
              -> (Doc,BarNum)
flatAltRepeat upf n xs xss = (body <$> alts, n'') 
  where
    (body,n')  = fmap2a (repeatvolta $ length xss) $ flatBars upf n xs
    (alts,n'') = alternatives upf n' xss


alternatives :: (Int -> DocS) -> BarNum -> [[BarImage]] -> (Doc,Int)
alternatives upf = fmap2a alternative `oo` stmap (flatBars upf)

flatBars :: (Int -> DocS) -> BarNum -> [BarImage] -> (Doc,BarNum)
flatBars upf = fmap2a vsep `oo` stmap phi
  where
    phi i d      = (upf i $ d <+> singleBar,i+1)



--------------------------------------------------------------------------------
-- parallel...


-- Handily parallel overlays are 'context free' 

-- More convoluted than ABC as LilyPond \\parallelMusic has
-- to be a proper matrix.
--

type Depth = Int


parallelPhrases :: Duration -> [PhraseImage] -> [BarOverlayImage]
parallelPhrases _       []     = []
parallelPhrases bar_len (x:xs) = snd $ foldl' (parallel2 bar_len) (1,x) xs

parallel2  :: Duration 
           -> (Int,[BarOverlayImage]) 
           -> PhraseImage 
           -> (Int,[BarOverlayImage])
parallel2 bar_len (depth,bs1) bs2 = (depth+1, step bs1 bs2) 
  where
    step (x:xs) (y:ys) = parallelLy x y : step xs ys
    step (x:xs) []     = parallelLy x (spacer $ Just bar_len) : step xs [] 
    step []     (y:ys) = parallelLy (blank depth bar_len) y : step [] ys 
    step []     []     = []

blank :: Depth -> Duration -> BarOverlayImage
blank n d = step sbar (n-1) where
  step bar i | i <= 0    = bar
             | otherwise = step (parallelLy bar sbar) (i-1)

  sbar       = spacer $ Just d


parallelLy :: BarOverlayImage -> BarImage -> BarOverlayImage
parallelLy ov b = ov <+> singleBar <$> b
