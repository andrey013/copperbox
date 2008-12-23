
--------------------------------------------------------------------------------
-- |
-- Module      :  LpTab.Process
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Data types
--
--------------------------------------------------------------------------------



module LpTab.Process where

import LpTab.Datatypes 

import Bala.Base hiding (filter,groupBy,transpose)

import qualified HNotate as H
import HNotate ( ( # ) )


import Data.List
import Data.Maybe
import Data.Ratio



cross :: ([TabLexeme] -> a) -> [TabLine] -> [BarF a]
cross column = xtranspose . map divideTabLine where
    -- xtranspose :: [[BarF TabLexeme]] -> [BarF a]
    xtranspose = map Bar . map (map column) 
                         . (map transpose . transpose) 
                         . map (map getBar)  
   
   
divideTabLine :: TabLine -> [BarF TabLexeme]
divideTabLine = map rembar . filter (not . emptyBar) . groupBy barmarkp where
    barmarkp :: TabLexeme -> TabLexeme -> Bool
    barmarkp a b        = (==BarMarkT) a && (/=BarMarkT) b
    
    rembar (BarMarkT : xs) = Bar xs
    
    emptyBar []         = True
    emptyBar [BarMarkT] = True
    emptyBar _          = False
                     

-- This interprets a column as a single voice - chords are possible, 
-- but other simultaneous notes are not (this rules out bass lines...)  
monoColumn :: [TabLexeme] -> MonoEvent
monoColumn = build . work 1 where
    work _ []                     = []    
    work i (FretNumberT n :xs)    = (i,n) : work (i+1) xs
    work i (_             :xs)    = work (i+1) xs
    
    build []                      = NoME
    build [(i,n)]                 = NoteME i n
    build xs                      = ChordME xs


monoCompact :: BarF MonoEvent -> BarF (Int,MonoEvent)
monoCompact (Bar [])      = Bar []
monoCompact (Bar (x:xs))  = Bar $ step (1,x) xs where 
    step (i,a) []           = [(i,a)]
    step (i,a) (NoME  : xs) = step (i+1,a) xs
    step (i,a) (x     : xs) = (i,a) : step (1,x) xs
       
lilypondBarME :: Tuning -> BarF (Int,MonoEvent) -> BarF (LyElementF Int)
lilypondBarME tuning (Bar xs) = Bar $ fmap step xs where
    step (i, NoteME si fi)  = LyNote (tunedNote tuning si fi) si i 
    step (i, ChordME xs)    = LyChord (fmap fn xs) i where
        fn (si,fi) = (tunedNote tuning si fi, si) 
    step (i, NoME)          = LyRest i

tunedNote :: Tuning -> StringNumber -> Fret -> Pitch
tunedNote xs snum fnum = (xs !! (snum-1)) `increase` fnum

lilypondSysME :: Tuning -> TabSystem -> [BarF (LyElementF Int)]
lilypondSysME tuning (TabSystem xs) = 
    map (lilypondBarME tuning . monoCompact) $ cross monoColumn xs

std_tuning = reverse [e3,a3,d4,g4,b4,e5]

toEventList :: Tuning -> [TabSystem] -> H.EventList
toEventList tuning ts = foldl' fn H.root $ map (lilypondSysME tuning) ts 
  where
    fn tree xs = foldl' addBar tree xs
    addBar tree (Bar xs) = foldl' addElt tree xs
    addElt tree (LyNote p sn _)  = tree # H.note' p quarter (H.stringNumber sn)
    addElt tree (LyChord xs _)   = tree 
    addElt tree (LyRest _)       = tree # H.rest quarter  
