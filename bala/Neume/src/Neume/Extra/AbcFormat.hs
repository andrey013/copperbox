{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcFormat
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


module Neume.Extra.AbcFormat
  (

    simpleOutput
  , tempOutput

  , overlayPhrases

  , hyphen

  ) where

import Neume.Core.SyntaxDoc
import Neume.Core.Utils.StateMap
import Neume.Extra.AbcDoc

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl', unfoldr )

-- | Output ABC, four bars printed on each line. 
simpleOutput :: AbcPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . getAbcBar) . getAbcPhrase


tempOutput :: [OverlayBar] -> Doc
tempOutput = four . map ((<+> singleBar) . getOverlayBar) 


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs


-- | ABC does not automatically decide how many bars-per-line to 
-- print.
type LineStk = [Int] 


-- Interspersing bars:
--
-- 1. The first bar should not be prefixed - even if it is a 
--    repeat.
--
-- 2. A 'straight' can be printed with at bar-line after the last
--    bar, regardless of what follows it. 
--     
-- 3. A repeat-start can start a line.
--
-- 4. Back-to-back repeats can print :| on one line and |: on the
--    next line.
--
-- 5. Alternative repeats - each repeat is started with its number 
--    e.g. [1 . The initial repeats are terminated with :| , the 
--    final repeat is terminated with ||
--
-- 6. A tune should end with || or :|
--

-- The start and end are index-sensitive:
-- 
-- 1. Don't print repeat-start on the first bar.
-- 
-- 2. Change | to || on last bar (leave repeat as is)
--
-- 3. Don't print LINE_CONT on last bar
--



type BarNum  = Int
type HyphenSpec = (BarNum,Hyph)


data FlatSection = FlatStraight Doc Hyph 
                 | FlatRepeated Doc Hyph
                 | FlatRepVolta Doc Hyph
  deriving Show

data Hyph = CONT | LINE_BREAK
  deriving (Eq,Show)


-- Note don't want to hyphenate last bar in case it is the final 
-- one...

flatSection :: (Int -> Doc -> Doc) 
          -> Section Doc 
          -> [HyphenSpec] 
          -> (FlatSection,[HyphenSpec])
flatSection pre sec hys = step sec where
   step (Straight xs)     = ansk FlatStraight $ interspersedBars pre xs hys
   step (Repeated xs)     = ansk FlatRepeated $ interspersedBars pre xs hys
   step (RepVolta xs yss) = let ((d,h),rest) = interspersedBars pre xs hys
                                body         = barCont d h  
                                (alts,rest') = alternatives pre yss rest
                            in (FlatRepVolta undefined undefined,rest')

   ansk cstr ((d,h),rest) = (cstr d h, rest) 

-- not quite right phi shouldn't do barCont...

alternatives :: (Int -> Doc -> Doc) -> [[Doc]] -> [HyphenSpec] -> (Doc,[HyphenSpec])
alternatives pre alts hys = post $ stmap phi (zip [0..] alts) hys
  where
    phi (n,alt) hs = let ((d,h),rest) = interspersedBars (prex n) alt hs
                     in (barCont d h,rest) 

    post (xs,rest) = (vsep xs, rest)
  
    prex n         = \x -> pre x . (alternative n <+>)


interspersedBars :: (Int -> Doc -> Doc) 
                 -> [Doc] 
                 -> [HyphenSpec] 
                 -> ((Doc,Hyph),[HyphenSpec])
interspersedBars pre = step where
  step [b]    (h:hs)  = (terminalBookend pre h b,hs)
  step (b:bs) (h:hs)  = let bar               = bookend pre h b
                            ((body,hy),srest) = step bs hs
                        in  ((bar <$> body,hy), srest)
  step _      hs      = ((empty,LINE_BREAK), hs)


terminalBookend :: (Int -> Doc -> Doc) -> HyphenSpec -> Doc -> (Doc,Hyph)
terminalBookend pre (n,c)  d = (pre n d,c) 

bookend :: (Int -> Doc -> Doc) -> HyphenSpec -> Doc -> Doc
bookend pre (n,CONT)       d = pre n d <+> singleBar <> lineCont
bookend pre (n,LINE_BREAK) d = pre n d <+> singleBar

barCont :: Doc -> Hyph -> Doc
barCont d CONT       = d <+> singleBar <> lineCont
barCont d LINE_BREAK = d <+> singleBar
 



firstBar :: [HyphenSpec] -> Bool
firstBar ((1,_):_) = True
firstBar _         = False


hyphen :: LineStk -> [(Int,Hyph)]
hyphen stk = unfoldr phi (1,stk) where
  phi (b,(x:xs)) | x > 1     = Just ((b,CONT),       (b+1,x-1:xs))
                 | otherwise = Just ((b,LINE_BREAK), (b+1,xs))
  phi (b,_)                  = Just ((b,LINE_BREAK), (b+1,[]))


-- Handily overlays are 'context free' 

overlayPhrases :: [AbcPhrase] -> [OverlayBar]
overlayPhrases []     = []
overlayPhrases (x:xs) = foldl' overlay2 (overlay1 x) xs


overlay2  :: [OverlayBar] -> AbcPhrase -> [OverlayBar]
overlay2 bs1 (AbcPhrase bs2) = step bs1 bs2 where
  step (x:xs) (y:ys) = overlayAbc x (getAbcBar y) : step xs ys
  step xs     []     = xs 
  step []     ys     = map (OverlayBar . getAbcBar) ys 

overlay1 :: AbcPhrase -> [OverlayBar]
overlay1 = map (OverlayBar . getAbcBar) . getAbcPhrase

overlayAbc :: OverlayBar -> Doc -> OverlayBar
overlayAbc (OverlayBar v1) v2 = OverlayBar $ v1 <+> overlay <> lineCont <$> v2 
