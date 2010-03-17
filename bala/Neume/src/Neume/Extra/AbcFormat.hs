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

    simpleOutput
  , barNumber

  , overlayPhrases

  , abcScore
  , abcScore'

  ) where

import Neume.Core.SyntaxDoc
import Neume.Core.SyntaxStaff
import Neume.Core.Utils hiding ( viewl ) 

import Neume.Extra.AbcDoc

-- package: joinlist
import Data.JoinList ( JoinList, singleton, join, null, viewl, ViewL(..) )

import Text.PrettyPrint.Leijen          -- package: wl-pprint

import Data.List ( foldl', unfoldr )
import Prelude hiding ( null )


-- | Output ABC, four bars printed on each line. 
simpleOutput :: PhraseImage -> Doc
simpleOutput = four . map (<+> singleBar) . getPhraseImage


-- tempOutput :: [OverlayBar] -> Doc
-- tempOutput = four . map ((<+> singleBar) . getOverlayBar) 


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs



barNumber :: Int -> DocS
barNumber i = (comment ("Bar " ++ show i) <$>)



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

-- User-prefix function has to be a (Doc -> Doc) because 
-- their is no null predicate on Docs.


data FourList a b c d = Cons a b c d (FourList a b c d)
                      | Nil

data StartSymbol = START_NONE           -- Straight section
                 | START_REP            -- Both repeat or alt_repeat
  deriving (Eq,Show)

data EndSymbol   = END_SGL              -- Straight section
                 | END_REP              -- Repeat
                 | END_DBL              -- Alt_repeat
  deriving (Eq,Show)


type AbcList = FourList DocS StartSymbol Doc (EndSymbol,Hyph)

type FlatElt = (DocS, StartSymbol, Doc, EndSymbol, Hyph)
type FlatRep = JoinList FlatElt


-- Inside the section: (user-prefix, body, hyphen)
--
type IntraSection = ( DocS, Doc, Hyph )


type BarNum  = Int
type HyphenSpec = (BarNum,Hyph)


data Hyph = CONT | LINE_BREAK
  deriving (Eq,Show)

-- b6_score :: Score repr [StdGlyph] => repr [StdGlyph]

abcScore' :: (a -> PhraseImage) 
          -> (Int -> DocS) 
          -> LineStk 
          -> (() -> FlatInterp a) 
          -> Doc
abcScore' rf upf ls score = 
  flatRep $ fst $ unFlatRep (score ()) rf upf (hyphen ls)


-- Should this be an 'interpretation' function instead?

-- newtype P_Env a = P_Env { unP_Env :: Int -> Doc }

-- needs to be stateful...

-- newtype AbcStd a = AbcStd { unAbcStd :: (Int -> DocS) -> LineStk -> (Doc,LineStk) } 

newtype FlatInterp a = FlatInterp { 
          unFlatRep :: (a -> PhraseImage) -> (Int -> DocS) 
                    -> [HyphenSpec] -> (FlatRep,[HyphenSpec]) } 


instance Score FlatInterp [StdGlyph] where
  straight a    = FlatInterp $ \rf upf ls -> 
                    let bars = getPhraseImage $ rf a
                    in fmap2a singleton $ flatStraight upf bars ls

  repeated a    = FlatInterp $ \rf upf ls -> 
                    let bars = getPhraseImage $ rf a
                    in fmap2a singleton $ flatRepeated upf bars ls

  altRepeat a b = FlatInterp $ \rf upf ls ->
                    let body = getPhraseImage $ rf a 
                        alts = map (getPhraseImage . rf) b 
                    in fmap2a singleton $ flatAltRepeat upf body alts ls

  caten ra rb   = FlatInterp $ \rf upf ls ->
                    let (d1,ls')  = (unFlatRep ra) rf upf ls 
                        (d2,ls'') = (unFlatRep rb) rf upf ls'
                    in (d1 `join` d2, ls'')

-- This is the wrong type - it satisfies preprocessing the
-- leaves (then building the score) rather than interpreting the
-- score...  
--
abcScore :: (Int -> DocS) -> LineStk -> ScoreImage a -> Doc
abcScore upf stk = flatDoc . flatten upf stk . getScoreImage


flatDoc :: AbcList -> Doc
flatDoc = initial where

  -- don't print the start_symbol
  initial Nil                       = empty
  initial (Cons upf _ doc end Nil)  = upf (doc <+> terminal end)
  initial (Cons upf _ doc end xs)   = d1 <$> middle xs
                                      where 
                                         d1 = upf (doc <+> intrasep end)
                                      
         
  middle Nil                        = empty      -- unreachable???
  middle (Cons upf st doc end Nil)  = upf (doc' <+> terminal end) 
                                       where 
                                        doc' = st `prefix` doc
                              
  middle (Cons upf st doc end xs)   = d1 <$> middle xs 
                                      where 
                                        doc' = st `prefix` doc
                                        d1   = upf (doc' <+> intrasep end)
                                      
  prefix START_NONE d               = d
  prefix START_REP  d               = lrepeat <+> d


flatRep :: FlatRep -> Doc
flatRep = initial . viewl  where

  -- don't print the start_symbol
  initial EmptyL                      = empty
  initial ((upf,_,doc,end,hy) :< xs)
      | null xs                       = upf (doc <+> terminal2 end hy)
      | otherwise                     = d1 <$> middle (viewl xs)
    where 
      d1 = upf (doc <+> intrasep2 end hy)
                                      
         
  middle EmptyL                       = empty      -- unreachable???
  middle ((upf,st,doc,end,hy) :< xs)
      | null xs                       = upf (doc' <+> terminal2 end hy) 
      | otherwise                     = d1 <$> middle (viewl xs) 
    where 
      doc' = st `prefix` doc
      d1   = upf (doc' <+> intrasep2 end hy)
                                      
  prefix START_NONE d               = d
  prefix START_REP  d               = lrepeat <+> d




-- | Both Straights and alt_repeats are promoted to "||"
-- and the hyphenation has no bearing.
--
terminal2 :: EndSymbol -> Hyph -> Doc
terminal2 END_REP _ = rrepeat
terminal2 _       _ = doubleBar


intrasep2 :: EndSymbol -> Hyph -> Doc
intrasep2 end hy = step end where
   step END_SGL = singleBar `suffix` hy
   step END_REP = rrepeat   `suffix` hy
   step END_DBL = doubleBar `suffix` hy

   suffix d CONT = d <> lineCont
   suffix d _    = d



-- | Both Straights and alt_repeats are promoted to "||"
-- and the hyphenation has no bearing.
--
terminal :: (EndSymbol,Hyph) -> Doc
terminal (END_REP,_) = rrepeat
terminal _           = doubleBar

intrasep :: (EndSymbol,Hyph) -> Doc
intrasep = step where
   step (END_SGL,hy) = singleBar `suffix` hy
   step (END_REP,hy) = rrepeat   `suffix` hy
   step (END_DBL,hy) = doubleBar `suffix` hy

   suffix d CONT = d <> lineCont
   suffix d _    = d



flatten :: (Int -> DocS) -> LineStk -> JoinList SectionImage -> AbcList
flatten pre stk xs = step (viewl xs) (hyphen stk) where
  step EmptyL                    _  = Nil

  step (Straight a :< as)    hs = Cons upf START_NONE doc (END_SGL,hy) ls_rest
    where 
      ((upf,doc,hy),rest) = intraBars pre (getPhraseImage a) hs
      ls_rest             = step (viewl as) rest

  step (Repeated a :< as)    hs = Cons upf START_REP doc (END_REP,hy) ls_rest
    where 
      ((upf,doc,hy),rest) = intraBars pre (getPhraseImage a) hs
      ls_rest             = step (viewl as) rest                  


  step (AltRepeat a b :< as) hs = Cons upf START_REP doc (END_DBL,hy) ls_rest
    where
      (isec1,hs')  = intraBars pre (getPhraseImage a) hs
      (isec2,rest) = alternatives pre (map getPhraseImage b) hs'
      (upf,doc,hy) = isec1 `sglconcat` isec2
      ls_rest      = step (viewl as) rest


flatStraight :: (Int -> DocS) 
             -> [BarImage] 
             -> [HyphenSpec] 
             -> (FlatElt,[HyphenSpec])
flatStraight df bars hys = ((upf,START_NONE,doc,END_SGL,hy),rest)
  where
    ((upf,doc,hy),rest) = intraBars df bars hys

flatRepeated :: (Int -> DocS) 
             -> [BarImage] 
             -> [HyphenSpec] 
             -> (FlatElt,[HyphenSpec])
flatRepeated df bars hys = ((upf,START_REP,doc,END_REP,hy),rest)
  where
    ((upf,doc,hy),rest) = intraBars df bars hys


flatAltRepeat :: (Int -> DocS) 
              -> [BarImage] 
              -> [[BarImage]]
              -> [HyphenSpec] 
              -> (FlatElt,[HyphenSpec])
flatAltRepeat df bars alts hys = ((upf,START_REP,doc,END_REP,hy),rest)
  where
    (body,hys')  = intraBars df bars hys
    (ends,rest)  = alternatives df alts hys'
    (upf,doc,hy) = body `sglconcat` ends
 
alternatives :: (Int -> DocS) 
             -> [[Doc]] 
             -> [HyphenSpec] 
             -> (IntraSection,[HyphenSpec])
alternatives pre alts hys = post $ stmap phi (zip [0..] alts) hys
  where
    phi (n,alt) hs = intraBars (prex n) alt hs

    post (xs,rest) = step xs where
      step [a]     = (a,rest)
      step (d:ds)  = let (dbody,rest') = step ds
                     in (intraConcat rrepeat d  dbody,rest')
      step []      = (no_intra,rest)  
  
    prex n         = \x -> pre x . (alternative n <+>)



intraBars :: (Int -> DocS) 
          -> [Doc] 
          -> [HyphenSpec] 
          -> (IntraSection,[HyphenSpec])
intraBars pre = step where
  step [b]    (h:hs)  = (section1 pre h b,hs)
  step (b:bs) (h:hs)  = let sbar          = section1 pre h b
                            (sbody,srest) = step bs hs
                        in (sbar `sglconcat` sbody, srest)
  step _      hs      = (no_intra, hs)

no_intra :: IntraSection 
no_intra = (id, empty, LINE_BREAK)


section1 :: (Int -> DocS) -> HyphenSpec -> Doc -> IntraSection
section1 pre (n,c)  d = (pre n, d, c )


intraConcat :: Doc -> IntraSection -> IntraSection -> IntraSection
intraConcat barsym (pf,b,hy) (pf',b',hy') = (pf,body,hy')
  where
    body    = b <+> barsym <> (fn hy) <$> (pf' b')
    fn CONT = lineCont
    fn _    = empty


sglconcat :: IntraSection -> IntraSection -> IntraSection
sglconcat = intraConcat singleBar


hyphen :: LineStk -> [(Int,Hyph)]
hyphen stk = unfoldr phi (1,stk) where
  phi (b,(x:xs)) | x > 1     = Just ((b,CONT),       (b+1,x-1:xs))
                 | otherwise = Just ((b,LINE_BREAK), (b+1,xs))
  phi (b,_)                  = Just ((b,LINE_BREAK), (b+1,[]))


-- Handily overlays are 'context free' 

overlayPhrases :: [PhraseImage] -> [OverlayBar]
overlayPhrases []     = []
overlayPhrases (x:xs) = foldl' overlay2 (overlay1 x) xs


overlay2  :: [OverlayBar] -> PhraseImage -> [OverlayBar]
overlay2 bs1 (PhraseImage bs2) = step bs1 bs2 where
  step (x:xs) (y:ys) = overlayAbc x y : step xs ys
  step xs     []     = xs 
  step []     ys     = map OverlayBar ys 

overlay1 :: PhraseImage -> [OverlayBar]
overlay1 = map OverlayBar . getPhraseImage

overlayAbc :: OverlayBar -> Doc -> OverlayBar
overlayAbc (OverlayBar v1) v2 = OverlayBar $ v1 <+> overlay <> lineCont <$> v2 
