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
  , renderLyRelative
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



renderLyRelative :: Ly_Std_Format_Config
                 -> Ly_Relative_Rewrite_Config
                 -> MeterPattern 
                 -> [Section [PletTree StdGlyph]] 
                 -> Doc
renderLyRelative (Ly_Std_Format_Config func) (Ly_Relative_Rewrite_Config pch) mp = 
    concatDocSections func . fst . stmap (renderSectionRel mp) pch

renderLyDrums :: Ly_Std_Format_Config
              -> MeterPattern 
              -> [Section [PletTree DrumGlyph]] 
              -> Doc
renderLyDrums (Ly_Std_Format_Config func) mp = 
    concatDocSections func . map (renderSectionDrums mp) 



renderSectionRel :: MeterPattern
                 -> Pitch
                 -> Section [PletTree StdGlyph] 
                 -> (Section PhraseImage,Pitch)
renderSectionRel mp = stmap fn 
  where
    fn :: Pitch -> [PletTree StdGlyph] -> (PhraseImage,Pitch)
    fn pch = fmap2a (renderPhrase pitch) . lyRelativeRewrite pch . phrase mp

renderSectionDrums :: MeterPattern 
                   -> Section [PletTree DrumGlyph] 
                   -> Section PhraseImage
renderSectionDrums mp = fmap fn
  where
    fn :: [PletTree DrumGlyph] -> PhraseImage
    fn = renderPhrase (text . drumShortName) . rewriteDurationOpt . phrase mp


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

parallelPhrases :: Duration -> [PhraseImage] -> [OverlayImage]
parallelPhrases _       []     = []
parallelPhrases bar_len (x:xs) = snd $ foldl' (parallel2 bar_len) (1,parallel1 x) xs

type Depth = Int

parallel2  :: Duration -> (Int,[OverlayImage]) -> PhraseImage -> (Int,[OverlayImage])
parallel2 bar_len (depth,bs1) bs2 = (depth+1, step bs1 bs2) 
  where
    step (x:xs) (y:ys) = parallelLy x y : step xs ys
    step (x:xs) []     = parallelLy x (spacer $ Just bar_len) : step xs [] 
    step []     (y:ys) = parallelLy (blank depth bar_len) y : step [] ys 
    step []     []     = []

blank :: Depth -> Duration -> OverlayImage
blank n d = step (OverlayImage sbar) (n-1) where
  step bar i | i <= 0    = bar
             | otherwise = step (parallelLy bar sbar) (i-1)

  sbar       = spacer $ Just d

parallel1 :: PhraseImage -> [OverlayImage]
parallel1 = map OverlayImage

parallelLy :: OverlayImage -> Doc -> OverlayImage
parallelLy (OverlayImage v1) v2 = OverlayImage $ v1 <+> singleBar <$> v2 
