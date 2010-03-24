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

  
    simpleOutput
  , lyPhraseRelative
  , lyPhraseAbsolute
  , lyPhraseDrums

  , lilypondScore
  , lilypondDrumScore

  , parallelPhrases


  ) where

import Neume.Core.Bracket
import Neume.Core.Datatypes
import Neume.Core.Duration
import Neume.Core.LilyPondBasic ( pitch, spacer )
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.Extended
import Neume.Extra.LilyPondDoc hiding ( score )


import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.List ( foldl' )

simpleOutput :: PhraseImage -> Doc
simpleOutput = vsep . map (<+> singleBar) . getPhraseImage



-- The internal argument to renderPhrase should be extracted 
-- from these and made a prarameter...
--
-- Like wise they need to handle an anacrusis 
--

lyPhraseRelative :: MeterPattern -> Pitch -> [StdGlyph] -> (PhraseImage,Pitch)
lyPhraseRelative mp pch xs = 
    fmap2a (renderPhrase pitch) $ rewritePitchRel pch
                                $ rewriteDurationOpt
                                $ phrase mp
                                $ simpleNoteList xs


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


-- 
{-
-- NO!
lyOverlayRelative :: MeterPattern -> Overlay StdGlyph -> Pitch -> (PhraseImage,Pitch)
lyOverlayRelative mp (Overlay xss) pch = 
    fmap2a (parallelPhrases  ) stmap fn xss 
  where
    fn = fmap2a (renderPhrase pitch) . rewritePitchRel pch
                                     . rewriteDurationOpt
                                     . phrase mp
                                     . simpleNoteList

-}


--------------------------------------------------------------------------------


newtype LyStdRel a = LyStdRel { 
          unLyStdRel :: (Pitch -> a -> (PhraseImage,Pitch)) 
                     -> (BarNum -> DocS) 
                     -> Pitch -> BarNum -> (Doc,(Pitch,BarNum)) }



lilypondScore :: (Pitch -> a -> (PhraseImage,Pitch)) 
              -> (BarNum -> DocS) 
              -> Pitch
              -> (() -> LyStdRel a) 
              -> Doc
lilypondScore rf upf pch score = fst $ unLyStdRel (score ()) rf upf pch 1


instance Score (LyStdRel [StdGlyph]) where
  type ScoreBase (LyStdRel [StdGlyph]) = [StdGlyph]
  straight a = LyStdRel $ \rf upf pch bn ->
                 let (bars,pch') = renderToBars_st rf pch a
                     (out,bn')   = flatStraight upf bn bars
                 in (out,(pch',bn'))

  repeated a = LyStdRel $ \rf upf pch bn ->
                 let (bars,pch') = renderToBars_st rf pch a 
                     (out,bn')   = flatRepeated upf bn bars
                 in (out,(pch',bn'))
                 
  altRepeat a b = LyStdRel $ \rf upf pch bn ->
                    let ((bars,alts),pch') = psimap_st (renderToBars_st rf) pch a b
                        (out,bn')          = flatAltRepeat upf bn bars alts
                    in (out,(pch',bn'))

  caten ra rb   = LyStdRel $ \rf upf pch bn ->
                    let f a (p,n) = (unLyStdRel a) rf upf p n
                    in stcombWith (<$>) (f ra) (f rb) (pch,bn)



--------------------------------------------------------------------------------



--------------------------------------------------------------------------------


newtype LyDrum a = LyDrum { 
          unLyDrum :: (a -> PhraseImage) 
                   -> (BarNum -> DocS) 
                   -> BarNum -> (Doc,BarNum) }


lilypondDrumScore :: (a -> PhraseImage)
                  -> (Int -> DocS) 
                  -> (() -> LyDrum a) 
                  -> Doc
lilypondDrumScore rf upf score = fst $ unLyDrum (score ()) rf upf 1


instance Score (LyDrum [DrumGlyph]) where
  type ScoreBase (LyDrum [DrumGlyph]) = [DrumGlyph]
  straight a = LyDrum $ \rf upf bn ->
                 let bars = renderToBars rf a
                 in flatStraight upf bn bars


  repeated a = LyDrum $ \rf upf bn ->
                 let bars = renderToBars rf a
                 in flatRepeated upf bn bars
                 
  altRepeat a b = LyDrum $ \rf upf bn ->
                    let (bars,alts)  = psimap (renderToBars rf) a b
                    in flatAltRepeat upf bn bars alts

  caten ra rb   = LyDrum $ \rf upf bn ->  
                    let f a = (unLyDrum a) rf upf
                    in stcombWith (<$>) (f ra) (f rb) bn 

    
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
parallel2 bar_len (depth,bs1) (PhraseImage bs2) = (depth+1, step bs1 bs2) 
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
parallel1 = map OverlayImage . getPhraseImage

parallelLy :: OverlayImage -> Doc -> OverlayImage
parallelLy (OverlayImage v1) v2 = OverlayImage $ v1 <+> singleBar <$> v2 
