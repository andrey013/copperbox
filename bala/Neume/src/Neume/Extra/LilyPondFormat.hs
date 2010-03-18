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

  , lilypondScore

  , parallelPhrases


  ) where

import Neume.Core.Duration
import Neume.Core.LilyPondBasic ( spacer )
import Neume.Core.Pitch
import Neume.Core.SyntaxDoc
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.LilyPondDoc hiding ( score )

-- import Data.JoinList ( toList )                 -- package: joinlist

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

import Data.List ( foldl' )

simpleOutput :: PhraseImage -> Doc
simpleOutput = vsep . map (<+> singleBar) . getPhraseImage




{-
lyPhrase :: (Pitch -> Doc) -> Pitch -> MeterPattern -> [StdGlyph] -> (Phrase LY,Pitch)
lyPhrase pf base_pitch mp = 
   fmap2a (renderPhrase pf) . rewritePitchRel base_pitch
                            . rewriteDurationOpt
                            . phrase mp
                            . simpleNoteList
-}



newtype LyStdRel a = LyStdRel { 
          unLyStdRel :: (a -> Pitch -> (PhraseImage,Pitch)) 
                     -> (BarNum -> DocS) 
                     -> Pitch -> BarNum -> (Doc,(Pitch,BarNum)) }



lilypondScore :: (a -> Pitch -> (PhraseImage,Pitch)) 
              -> (Int -> DocS) 
              -> Pitch
              -> (() -> LyStdRel a) 
              -> Doc
lilypondScore rf upf pch score = fst $ unLyStdRel (score ()) rf upf pch 1


instance Score LyStdRel [StdGlyph] where
  straight a = LyStdRel $ \rf upf pch bn ->
                 let (bars,pch') = fmap2a getPhraseImage $ rf a pch
                     (out,bn')   = flatStraight upf bars bn
                 in (out,(pch',bn'))

  repeated a = LyStdRel $ \rf upf pch bn ->
                 let (bars,pch') = fmap2a getPhraseImage $ rf a pch
                     (out,bn')   = flatRepeated upf bars bn
                 in (out,(pch',bn'))
                 
  altRepeat a b = LyStdRel $ \rf upf pch bn ->
                    let (bars,pch')  = fmap2a getPhraseImage $ rf a pch
                        (alts,pch'') = fmap2a (map getPhraseImage) $ stmap rf b pch'
                        (out,bn')    = flatAltRepeat upf bars alts bn
                    in (out,(pch'',bn'))

  caten ra rb   = LyStdRel $ \rf upf pch bn ->
                    let (d1,(pch',bn'))  = (unLyStdRel ra) rf upf pch  bn 
                        (d2,st)          = (unLyStdRel rb) rf upf pch' bn'
                    in (d1 <$> d2, st)



flatStraight :: (BarNum -> DocS) -> [BarImage] -> BarNum -> (Doc,BarNum)
flatStraight = phrase

flatRepeated :: (BarNum -> DocS) -> [BarImage] -> BarNum -> (Doc,BarNum)
flatRepeated = fmap2a (repeatvolta 2) `ooo` phrase

flatAltRepeat :: (BarNum -> DocS) 
              -> [BarImage] 
              -> [[BarImage]] 
              -> BarNum 
              -> (Doc,BarNum)
flatAltRepeat upf xs xss n = (body <$> alts, n'') 
  where
    (body,n')  = fmap2a (repeatvolta $ length xss) $ phrase upf xs n
    (alts,n'') = alternatives upf xss n'


alternatives :: (Int -> DocS) -> [[BarImage]] -> Int -> (Doc,Int)
alternatives upf = fmap2a alternative `oo` stmap (phrase upf)

phrase :: (Int -> DocS) -> [BarImage] -> BarNum -> (Doc,BarNum)
phrase upf = fmap2a vsep `oo` stmap phi
  where
    phi d i      = (upf i $ d <+> singleBar,i+1)



--------------------------------------------------------------------------------
-- parallel...


-- Handily parallel overlays are 'context free' 

-- More convoluted than ABC as LilyPond \\parallelMusic has
-- to be a proper matrix.
--

parallelPhrases :: Duration -> [PhraseImage] -> [OverlayBar]
parallelPhrases _       []     = []
parallelPhrases bar_len (x:xs) = snd $ foldl' (parallel2 bar_len) (1,parallel1 x) xs

type Depth = Int

parallel2  :: Duration -> (Int,[OverlayBar]) -> PhraseImage -> (Int,[OverlayBar])
parallel2 bar_len (depth,bs1) (PhraseImage bs2) = (depth+1, step bs1 bs2) 
  where
    step (x:xs) (y:ys) = parallelLy x y : step xs ys
    step (x:xs) []     = parallelLy x (spacer $ Just bar_len) : step xs [] 
    step []     (y:ys) = parallelLy (blank depth bar_len) y : step [] ys 
    step []     []     = []

blank :: Depth -> Duration -> OverlayBar
blank n d = step (OverlayBar sbar) (n-1) where
  step bar i | i <= 0    = bar
             | otherwise = step (parallelLy bar sbar) (i-1)

  sbar       = spacer $ Just d

parallel1 :: PhraseImage -> [OverlayBar]
parallel1 = map OverlayBar . getPhraseImage

parallelLy :: OverlayBar -> Doc -> OverlayBar
parallelLy (OverlayBar v1) v2 = OverlayBar $ v1 <+> singleBar <$> v2 
