{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.AbcFmt2
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


module Neume.Extra.AbcFmt2
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
import Neume.Core.Score2
import Neume.Core.SyntaxScore hiding ( Score )
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.AbcDoc



import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl' )
import Data.Monoid
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
    , meter_pattern   :: MeterPattern
    }


--------------------------------------------------------------------------------



renderABC :: ABC_Std_Format_Config
          -> ABC_Std_Rewrite_Config
          -> Score sh (NoteList StdGlyph)
          -> Doc
renderABC (ABC_Std_Format_Config line_stk func) rw = 
    concatDocSections func line_stk . renderScore rw



renderScore :: ABC_Std_Rewrite_Config 
              -> Score sh (NoteList StdGlyph)
              -> Score sh PhraseImage
renderScore cfg = fmap (phraseImage cfg)



phraseImage :: ABC_Std_Rewrite_Config
            -> NoteList (Glyph anno Pitch Duration)
            -> PhraseImage
phraseImage cfg = renderPhrase . abcRewrite spellmap unit_drn . phrase mp 
  where 
    spellmap = spelling_map  cfg
    unit_drn = unit_duration cfg
    mp       = meter_pattern cfg





-- Note this is polymorphic on phrase, so it can handle 
-- PhraseOverlayImages as well

concatDocSections :: ExtractBarImages phrase 
                  => (BarNum -> DocS) -> LineStk -> Score sh phrase -> Doc
concatDocSections fn stk score = 
    finalFromInterim $ step (infHyphenSpec stk) score
  where
    step :: ExtractBarImages phrase 
         => HyphenSpec -> Score sh phrase -> InterimDoc 
    step _  Nil              = mempty

    step hy (Linear e xs)    = d `mappend` step hy' xs
      where (d,hy') = interLinear fn hy (extractBarImages e)

    step hy (Repeat e xs)    = d `mappend` step hy' xs
      where (d,hy') = interRepeat fn hy (extractBarImages e)

    step hy (AltRep e es xs) = d `mappend` step hy' xs
      where (d,hy') = interAltRep fn hy (extractBarImages e) 
                                        (map extractBarImages es)



--------------------------------------------------------------------------------

infHyphenSpec :: LineStk -> HyphenSpec
infHyphenSpec stk = step (1,stk) where
  step (n,[])               = (n,LINE_BREAK) : step (n+1,[])
  step (n,x:xs) | x > 1     = (n,CONT)       : step (n+1,x-1:xs)
                | otherwise = (n,LINE_BREAK) : step (n+1,xs)


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




type InterimPrefix = (DocS,StartSymbol)
type InterimSuffix = (EndSymbol,Hyphen)

data InterimDoc = InterimDoc { _iprefix    :: InterimPrefix
                             , _interbody  :: Doc
                             , _isuffix    :: InterimSuffix
                             }
                | DocEmpty


instance Monoid InterimDoc where
  mempty = DocEmpty 
  DocEmpty         `mappend` d                   = d
  d                `mappend` DocEmpty            = d
  InterimDoc p d s `mappend` InterimDoc p' d' s' = InterimDoc p body s'
    where  body = d <+> interimSuffix s <$> withInterimPrefix p' d'


finalFromInterim :: InterimDoc -> Doc
finalFromInterim DocEmpty                      = empty
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
withInterimPrefix (fn,START_ALT n) d = fn $ alternative n <+> d


interLinear :: (Int -> DocS) 
            -> HyphenSpec 
            -> [BarImage] 
            -> (InterimDoc,HyphenSpec)
interLinear df = interimBars df (START_NONE,END_SGL)

interRepeat :: (Int -> DocS) 
            -> HyphenSpec 
            -> [BarImage] 
            -> (InterimDoc,HyphenSpec)
interRepeat df = interimBars df (START_REP,END_REP)


-- This doesn't account that the last END_REP 
-- should produce a double bar rather than :|
--
interAltRep :: (Int -> DocS) 
            -> HyphenSpec 
            -> [BarImage] 
            -> [[BarImage]]
            -> (InterimDoc,HyphenSpec)
interAltRep df hy bs bss = (oneconcat body alts, hy'') 
  where
    (body,hy')          = interimBars df (START_NONE,END_SGL) hy bs
    (alts,hy'')         = caboose_stmap stf stfinal hy' (zip [1..] bss)

    stf     h (n,alt)   = interimBars df (START_ALT n, END_REP) h alt
    stfinal h (n,alt)   = interimBars df (START_ALT n, END_DBL) h alt

    oneconcat d []      = d
    oneconcat d (a:as)  = oneconcat (d `mappend` a) as 



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

overlayPhrases :: [PhraseImage] -> PhraseOverlayImage
overlayPhrases []                        = PhraseOverlayImage [] []
overlayPhrases ((PhraseImage n1 bars):xs) = 
    PhraseOverlayImage (n1:ns) $ foldl' overlayMerge bars xs
  where
    ns = map phrase_image_name xs

overlayMerge  :: [BarOverlayImage] -> PhraseImage -> [BarOverlayImage]
overlayMerge bs1 (PhraseImage _ bs2) = step bs1 bs2 where
  step (x:xs) (y:ys) = overlayAbc x y : step xs ys
  step xs     []     = xs 
  step []     ys     = ys 


overlayAbc :: BarOverlayImage -> BarOverlayImage -> BarOverlayImage
overlayAbc v1 v2 = v1 <+> overlay <> lineCont <$> v2 
