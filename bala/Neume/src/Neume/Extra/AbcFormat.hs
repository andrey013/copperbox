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



data SeparatedDoc = SDoc { user_prefix :: Doc -> Doc   
                         , sdoc_body   :: Doc
                         , end_hyphen  :: Hyph
                         }


sconcat :: Doc -> SeparatedDoc -> SeparatedDoc -> SeparatedDoc
sconcat barsym (SDoc pf b hy) (SDoc pf' b' hy') = SDoc pf body hy'
  where
    body    = b <+> barsym <> (fn hy) <$> (pf' b)
    fn CONT = lineCont
    fn _    = empty


sglconcat :: SeparatedDoc -> SeparatedDoc -> SeparatedDoc
sglconcat = sconcat singleBar

type BarNum  = Int
type HyphenSpec = (BarNum,Hyph)


-- Turn this one into an ENUM - less work pattern matching

type FlatSection = (SectionForm,SeparatedDoc)

data SectionForm = STRAIGHT | REPEATED | REP_VOLTA
  deriving (Eq,Show)

data Hyph = CONT | LINE_BREAK
  deriving (Eq,Show)


-- Still can't add end - for last SDoc we might need to change | to || 
initialSD :: SectionForm -> SeparatedDoc -> (Doc,Hyph)
initialSD _ (SDoc pf body hy) = (pf body,hy)

bodySD :: SectionForm -> SeparatedDoc -> (Doc,Hyph)
bodySD _ (SDoc pf body hy) = (pf (lrepeat <+> body), hy)

-- NO - final can also be first....
finalDoc :: SectionForm -> SeparatedDoc -> Doc
finalDoc STRAIGHT (SDoc pf body _) = pf body <+> doubleBar
finalDoc REPEATED (SDoc pf body _) = pf (lrepeat <+> body <+> rrepeat)
finalDoc _        _                = undefined

{-
flatDoc :: [FlatSection] -> Doc
flatDoc []     = empty
flatDoc (x:xs) = start x xs where
  start (_,d,h) xs = body d h xs

  body d c [a] = final d c a
  body d c (
  
  final d c a = undefined
-}


flatten :: (Int -> Doc -> Doc) -> LineStk -> [Section Doc] -> [FlatSection]
flatten pre stk xs = fst $ stmap (flatSection pre) xs (hyphen stk)


-- Note don't want to hyphenate last bar in case it is the final 
-- one...


flatSection :: (Int -> Doc -> Doc) 
          -> Section Doc 
          -> [HyphenSpec] 
          -> (FlatSection,[HyphenSpec])
flatSection pre sec hys = step sec where
   step (Straight xs)     = ansk STRAIGHT $ interspersedBars pre xs hys
   step (Repeated xs)     = ansk REPEATED $ interspersedBars pre xs hys
   step (RepVolta xs yss) = let (body,rest)   = interspersedBars pre xs hys
                                (alts,rest')  = alternatives pre yss rest
                            in ((REP_VOLTA, body `sglconcat` alts), rest')
   ansk en (sdoc,rest) = ((en,sdoc), rest) 



alternatives :: (Int -> Doc -> Doc) -> [[Doc]] -> [HyphenSpec] 
             -> (SeparatedDoc,[HyphenSpec])
alternatives pre alts hys = post $ stmap phi (zip [0..] alts) hys
  where
    phi (n,alt) hs = interspersedBars (prex n) alt hs

    post (xs,rest) = step xs where
      step [a]     = (a,rest)
      step (d:ds)  = let (dbody,rest') = step ds
                     in (d `sglconcat` dbody,rest')
      step []      = (noDoc,rest)  
  
    prex n         = \x -> pre x . (alternative n <+>)



interspersedBars :: (Int -> Doc -> Doc) 
                 -> [Doc] 
                 -> [HyphenSpec] 
                 -> (SeparatedDoc,[HyphenSpec])
interspersedBars pre = step where
  step [b]    (h:hs)  = (separatedDoc pre h b,hs)
  step (b:bs) (h:hs)  = let sbar              = separatedDoc pre h b
                            (sbody,srest) = step bs hs
                        in (sbar `sglconcat` sbody, srest)
  step _      hs      = (noDoc, hs)

noDoc :: SeparatedDoc
noDoc = SDoc id empty LINE_BREAK

separatedDoc :: (Int -> Doc -> Doc) -> HyphenSpec -> Doc -> SeparatedDoc
separatedDoc pre (n,c)  d = SDoc (pre n) d c 

bookend :: (Int -> Doc -> Doc) -> HyphenSpec -> Doc -> Doc
bookend pre (n,CONT)       d = pre n d <+> singleBar <> lineCont
bookend pre (n,LINE_BREAK) d = pre n d <+> singleBar

barCont :: Doc -> Hyph -> Doc
barCont d CONT       = d <+> singleBar <> lineCont
barCont d LINE_BREAK = d <+> singleBar
 



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
