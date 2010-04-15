{-# LANGUAGE GADTs                      #-}
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

  , renderLyRelative_parallel2  -- ugly prototype
  , scoreLy_parallel2           -- ditto

  , renderLyDrums

  , concatDocSections           -- needs exposing for Fret Diags etc.

  ) where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic ( pitch, spacer )
import Neume.Core.LilyPondOutput
import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.SyntaxInterim
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
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
                 -> Score sh (NoteList (Glyph anno Pitch Duration))
                 -> Doc
renderLyRelative (Ly_Std_Format_Config func) rw1 = 
    concatDocSections func . scoreImageRel rw1


scoreImageRel :: Ly_Relative_Rewrite_Config
              -> Score sh (NoteList (Glyph anno Pitch Duration)) 
              -> Score sh PhraseImage
scoreImageRel cfg = fst . stmap (phraseImageRel mp) (base_pitch cfg)
  where
    mp   = meter_pattern_rel cfg


renderLyRelative_parallel2 :: Duration 
                           -> Ly_Std_Format_Config
                           -> Ly_Relative_Rewrite_Config
                           -> Ly_Relative_Rewrite_Config
                           -> Score sh (NoteList (Glyph anno Pitch Duration))
                           -> Score sh (NoteList (Glyph anno Pitch Duration)) 
                           -> Doc
renderLyRelative_parallel2 bar_len (Ly_Std_Format_Config func) rw1 rw2 sc1 sc2 = 
    parallelDefs func $ merge2 bar_len sc1' sc2'
  where 
    sc1' = scoreImageRel rw1 sc1
    sc2' = scoreImageRel rw2 sc2


scoreLy_parallel2 :: Score sh (NoteList (Glyph anno Pitch Duration)) -> Doc
scoreLy_parallel2 = vsep . step . scoreAsNamed 
  where
    step :: Score sh PhraseName -> [Doc]
    step Nil               = []
    step (Linear e xs)     = variableUse e : step xs
    step (Repeat e xs)     = repeatvolta 2 (variableUse e) : step xs
    step (RepAlt e es xs)  = doc : step xs 
      where doc = repeatvolta (length es) (variableUse e)
              <$> alternative (map variableUse es) 


scoreAsNamed :: Score sh (NoteList (Glyph anno Pitch Duration)) 
             -> Score sh PhraseName
scoreAsNamed Nil                           = Nil
scoreAsNamed (Linear (NoteList n _) xs)    = Linear n $ scoreAsNamed xs
scoreAsNamed (Repeat (NoteList n _) xs)    = Repeat n $ scoreAsNamed xs
scoreAsNamed (RepAlt (NoteList n _) es xs) = RepAlt n es' $ scoreAsNamed xs
  where es' = map note_list_name es 


merge2 :: Duration 
       -> Score sh PhraseImage
       -> Score sh PhraseImage 
       -> Score sh PhraseOverlayImage
merge2 bar_len = scoreZipWith (\x y -> parallelPhrases bar_len [x,y])
                            

phraseImageRel :: MeterPattern
               -> Pitch
               -> NoteList (Glyph anno Pitch Duration)
               -> (PhraseImage,Pitch)
phraseImageRel mp pch = 
    fmap2a (renderPhrase (renderGlyph pitch)) . lyRelativeRewrite pch . phrase mp
  


renderLyDrums :: Ly_Std_Format_Config
              -> MeterPattern 
              -> Score sh (NoteList DrumGlyph)
              -> Doc
renderLyDrums (Ly_Std_Format_Config func) mp = 
    concatDocSections func . renderScoreDrums mp


renderScoreDrums :: MeterPattern 
                 -> Score sh (NoteList DrumGlyph)
                 -> Score sh PhraseImage
renderScoreDrums mp = fmap (phraseImageDrums mp) 


phraseImageDrums :: MeterPattern
                 -> NoteList DrumGlyph
                 -> PhraseImage
phraseImageDrums mp = 
    renderPhrase (renderGlyph (text . drumShortName)) . rewriteDurationOpt . phrase mp




concatDocSections :: (BarNum -> DocS) -> Score sh PhraseImage -> Doc
concatDocSections fn = vsep . step 1  
  where
    step :: BarNum -> Score sh PhraseImage -> [Doc]
    step _ Nil              = []

    step n (Linear e xs)    = d : step n' xs
      where (d,n') = flatLinear fn n (extractBarImages e)

    step n (Repeat e xs)    = d : step n' xs
      where (d,n') = flatRepeat fn n (extractBarImages e) 

    step n (RepAlt e es xs) = d : step n' xs
      where 
        (d,n') = flatRepAlt fn n (extractBarImages e) es'
        es'   =  map extractBarImages es



parallelDefs :: (BarNum -> DocS) -> Score sh PhraseOverlayImage -> Doc
parallelDefs fn = vsep . step 1  
  where
    step :: BarNum -> Score sh PhraseOverlayImage -> [Doc]
    step _ Nil              = []

    step n (Linear e xs)    = d : step n' xs
      where (d,n') = parallelSection fn n e

    step n (Repeat e xs)    = d : step n' xs
      where (d,n') = parallelSection fn n e

    step n (RepAlt e es xs) = (d:ds) ++ step n'' xs  
      where
        (d,n')     = parallelSection fn n e
        (ds,n'')   = stmap (parallelSection fn) n' es


parallelSection :: (BarNum -> DocS)
                -> BarNum
                -> PhraseOverlayImage 
                -> (Doc,BarNum)
parallelSection fn i (PhraseOverlayImage ns bars) = 
    fmap2a (parallelMusic ns) $ flatLinear fn i bars


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

flatLinear :: (BarNum -> DocS) -> BarNum -> [BarImage] -> (Doc,BarNum)
flatLinear = flatBars

flatRepeat :: (BarNum -> DocS) -> BarNum -> [BarImage] -> (Doc,BarNum)
flatRepeat = fmap2a (repeatvolta 2) `ooo` flatBars

flatRepAlt :: (BarNum -> DocS) 
           -> BarNum
           -> [BarImage] 
           -> [[BarImage]] 
           -> (Doc,BarNum)
flatRepAlt upf n xs xss = (body <$> alts, n'') 
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


parallelPhrases :: Duration -> [PhraseImage] -> PhraseOverlayImage
parallelPhrases _       []                      = PhraseOverlayImage [] []
parallelPhrases bar_len ((PhraseImage n1 im):xs) = 
    PhraseOverlayImage (n1:ns) $ snd $ foldl' (parallel2 bar_len) (1,im) xs
  where
    ns = map phrase_image_name xs

parallel2  :: Duration 
           -> (Depth,[BarOverlayImage]) 
           -> PhraseImage 
           -> (Depth,[BarOverlayImage])
parallel2 bar_len (depth,bs1) (PhraseImage _ bs2) = (depth+1, step bs1 bs2) 
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
