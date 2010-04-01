{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
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

    barNumber
  , anaBarNumber

  , renderABC 
  , ABC_Std_Format_Config(..)
  , ABC_Std_Rewrite_Config(..)

  , overlayPhrases


  ) where

import Neume.Core.AbcOutput
import Neume.Core.Duration
import Neume.Core.Bracket
import Neume.Core.Metrical
import Neume.Core.NoteList
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.AbcDoc


import Data.Semigroup                   -- package: algebra

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl' )
import Prelude hiding ( null )


barNumber :: BarNum -> DocS
barNumber i = (comment ("Bar " ++ show i) <$>)

anaBarNumber :: BarNum -> DocS
anaBarNumber i | i <= 1    = id
               | otherwise = barNumber (i-1)

-- | ABC does not automatically decide how many bars-per-line to 
-- print.
type LineStk = [Int] 

type HyphenSpec = [(BarNum,Hyphen)]


data Hyphen = CONT | LINE_BREAK
  deriving (Eq,Show)


data StartSymbol = START_NONE           -- Straight section
                 | START_REP            -- Repeat
                 | START_ALT Int        -- Alternative ending
  deriving (Eq,Show)

data EndSymbol   = END_SGL              -- Straight section
                 | END_REP              -- Repeat
                 | END_DBL              -- Alt_repeat
  deriving (Eq,Show)



data ABC_Std_Format_Config = ABC_Std_Format_Config
    { line_widths          :: LineStk
    , bar_numbering_func   :: BarNum -> DocS
    }


data ABC_Std_Rewrite_Config = ABC_Std_Rewrite_Config 
    { spelling_map    :: SpellingMap
    , unit_duration   :: DurationMeasure 
    }


--------------------------------------------------------------------------------

renderABC :: ABC_Std_Format_Config
          -> ABC_Std_Rewrite_Config
          -> MeterPattern 
          -> [Section [PletTree StdGlyph]] -> Doc
renderABC (ABC_Std_Format_Config line_stk func) rw_cfg mp = 
    concatDocSections func line_stk . map (renderSection rw_cfg mp)

renderSection :: ABC_Std_Rewrite_Config 
              -> MeterPattern
              -> Section [PletTree StdGlyph] 
              -> Section PhraseImage
renderSection (ABC_Std_Rewrite_Config spellmap unit_drn) mp = 
    fmap (renderPhrase . abcRewrite spellmap unit_drn . phrase mp)



concatDocSections :: (BarNum -> DocS) -> LineStk -> [Section PhraseImage] -> Doc
concatDocSections _  _   []     = empty
concatDocSections fn stk (x:xs) = let (d1,hy) = section1 (infHyphenSpec stk) x
                                  in finalFromInterim $ step d1 hy xs
  where
    step :: InterimDoc -> HyphenSpec -> [Section [BarImage]] -> InterimDoc 
    step _   _  []          = error "unreachable - concatDocSections"
    step acc hy [a]         = acc `append` (fst $ section1 hy a)
    step acc hy (a:as)      = let (a',hy') = section1 hy a 
                              in step (acc `append` a') hy' as
 
    section1 :: HyphenSpec -> Section [BarImage] -> (InterimDoc,HyphenSpec)
    section1 hy (Straight ds)       = interStraight fn hy ds
    section1 hy (Repeated ds)       = interRepeated fn hy ds
    section1 hy (AltRepeat ds alts) = interAltRepeat fn hy ds alts

--------------------------------------------------------------------------------


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

-- User-prefix function has to be a (Doc -> Doc) because 
-- their is no null predicate on Docs.





infHyphenSpec :: LineStk -> HyphenSpec
infHyphenSpec stk = step (1,stk) where
  step (n,[])               = (n,LINE_BREAK) : step (n+1,[])
  step (n,x:xs) | x>1       = (n,CONT)       : step (n+1,x-1:xs)
                | otherwise = (n,LINE_BREAK) : step (n+1,xs)


--------------------------------------------------------------------------------
-- new algorithm...


type InterimPrefix = (DocS,StartSymbol)
type InterimSuffix = (EndSymbol,Hyphen)

data InterimDoc = InterimDoc { _iprefix    :: InterimPrefix
                             , _interbody  :: Doc
                             , _isuffix    :: InterimSuffix
                             }

instance Semigroup InterimDoc where
  (InterimDoc p d s) `append` (InterimDoc p' d' s') = InterimDoc p doc_body s'
    where
      doc_body = d <+> interimSuffix s <$> withInterimPrefix p' d'



finalFromInterim :: InterimDoc -> Doc
finalFromInterim (InterimDoc (fn,_) doc (e,_)) = fn $ doc <> finalMark e
  where
    finalMark END_REP    = rrepeat 
    finalMark _          = doubleBar  -- promote final barline to a double bar

interimSuffix :: InterimSuffix -> Doc 
interimSuffix (end_mark,hy) = step end_mark where
   step END_SGL = singleBar `suffix` hy
   step END_REP = rrepeat   `suffix` hy
   step END_DBL = doubleBar `suffix` hy

   suffix d CONT = d <> lineCont
   suffix d _    = d

withInterimPrefix :: InterimPrefix -> Doc -> Doc
withInterimPrefix (fn,START_NONE)  d = fn d
withInterimPrefix (fn,START_REP)   d = fn $ lrepeat <+> d
withInterimPrefix (fn,START_ALT n) d = fn $ lrepeat <> int n <+> d


interStraight :: (Int -> DocS) 
              -> HyphenSpec 
              -> [BarImage] 
              -> (InterimDoc,HyphenSpec)
interStraight df = interimBars df (START_NONE,END_SGL)

interRepeated :: (Int -> DocS) 
              -> HyphenSpec 
              -> [BarImage] 
              -> (InterimDoc,HyphenSpec)
interRepeated df = interimBars df (START_REP,END_REP)

interAltRepeat :: (Int -> DocS) 
               -> HyphenSpec 
               -> [BarImage] 
               -> [[BarImage]]
               -> (InterimDoc,HyphenSpec)
interAltRepeat df hy bs bss = (oneconcat body alts, hy'') 
  where
    (body,hy')          = interimBars df (START_NONE,END_SGL) hy bs
    (alts,hy'')         = stmap stfun hy' (zip [1..] bss)

    stfun h (n,alt)     =  interimBars df (START_ALT n, END_REP) h alt

    oneconcat d []      = d
    oneconcat d (a:as)  = oneconcat (d `append` a) as 



-- candidate to use OneList for BarImage ?
--
interimBars :: (BarNum -> DocS) 
            -> (StartSymbol, EndSymbol)
            -> HyphenSpec 
            -> [BarImage]
            -> (InterimDoc,HyphenSpec)
interimBars _  _    _        []     = error "interimBars - empty list"
interimBars _  _    []       _      = error "unreachable - hys are infinite"
interimBars fn syms (hy:hys) (b:bs) = step (interimDoc1 fn syms hy b) hys bs
  where
    step acc hs     []     = (acc,hs)
    step acc []     ds     = badStep acc ds
    step acc (h:hs) (d:ds) = step (snocInterimDoc fn acc h d) hs ds


    -- run out of hyphen specs - so don't try to print bar number...
    badStep acc []       = (acc,[]) 
    badStep acc (d:ds)   = badStep (snocInterimDoc strip acc bad_hyph d) ds 

    bad_hyph = (-1,LINE_BREAK)



interimDoc1 :: (BarNum -> DocS) 
            -> (StartSymbol,EndSymbol) 
            -> (BarNum,Hyphen)
            -> BarImage 
            -> InterimDoc
interimDoc1 fn (start,end) (n,hy) d = InterimDoc (fn n,start) d (end,hy)

-- snoc-ing always delimits with a single bar
--
snocInterimDoc :: (BarNum -> DocS) 
               -> InterimDoc 
               -> (BarNum,Hyphen) 
               -> BarImage
               -> InterimDoc
snocInterimDoc fn (InterimDoc p d (end_sym,hyph)) (n,hy) d' =
    InterimDoc p doc_body (end_sym,hy)
  where
    doc_body = d <+> singleBar <> lc hyph <$> fn n d'
    lc CONT  = lineCont
    lc _     = empty

 

--------------------------------------------------------------------------------
-- Overlays...




-- Handily overlays are 'context free' 

overlayPhrases :: [PhraseImage] -> [OverlayImage]
overlayPhrases []     = []
overlayPhrases (x:xs) = foldl' overlay2 (overlay1 x) xs


overlay2  :: [OverlayImage] -> PhraseImage -> [OverlayImage]
overlay2 bs1 bs2 = step bs1 bs2 where
  step (x:xs) (y:ys) = overlayAbc x y : step xs ys
  step xs     []     = xs 
  step []     ys     = map OverlayImage ys 

overlay1 :: PhraseImage -> [OverlayImage]
overlay1 = map OverlayImage

overlayAbc :: OverlayImage -> Doc -> OverlayImage
overlayAbc (OverlayImage v1) v2 = 
    OverlayImage $ v1 <+> overlay <> lineCont <$> v2 
