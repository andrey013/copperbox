{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Base.Structural2
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- /Structural grouping of elements/ second version
--
--------------------------------------------------------------------------------

module Bala.Base.Structural2 where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Pitch
import HNotate.Fits

import Control.Applicative hiding (empty)

import qualified Data.Foldable as F
import Data.List (transpose, intercalate)
import Data.Sequence hiding (length, null)
import qualified Data.Sequence as S -- for length and null
import Data.Traversable

--------------------------------------------------------------------------------
-- Datatypes

-- Containers

data SectionF a = Section TimeSig (Seq (PhraseF a))
  deriving (Show)

data PhraseF a = Single (MotifF a)
               | Overlay (MotifF a) (Seq (MotifF a))   
  deriving (Show)

newtype MotifF a = Motif { getMotif :: Seq a }
  deriving (Show)  

type Section  = SectionF Elt
type Phrase   = PhraseF Elt
type Motif    = MotifF Elt


-- Elements

type GraceNotes = Seq (Pitch,Duration)

data Elt = DEvt Evt Duration      -- some 'event' that has a duration 
         | Mark Mark            -- a mark that has no duration
         | Chord (Seq Pitch) Duration
         | AGrace GraceNotes Pitch Duration 
         | UGrace Pitch Duration GraceNotes
  deriving (Eq,Show)
  
data Evt = Note Pitch | Rest | Spacer
  deriving (Eq,Show)
   
data Mark = Tie      -- | ... more ?       
  deriving (Eq,Show)
  
--------------------------------------------------------------------------------
-- wrapped constructors / builders

motif :: MotifF a
motif = Motif $ empty

phrase :: MotifF a -> PhraseF a
phrase a = Single a 

section :: TimeSig -> PhraseF a -> SectionF a
section tm a = Section tm (singleton a) 


starts :: Fits a Duration => PhraseF a -> MotifF a -> PhraseF a
starts (Single a)     mo = Overlay a (singleton mo)
starts (Overlay a se) mo = Overlay a (se |> mo)

starts' :: Fits a Duration => [MotifF a] -> PhraseF a
starts' []     = Single motif
starts' (x:xs) = foldl starts (phrase x) xs



infixl 7 +-
(+-) :: MotifF elt -> elt -> MotifF elt
(+-) (Motif se) elt = Motif  (se |> elt)



rest      :: Duration -> Elt
rest d    = DEvt Rest d

spacer    :: Duration -> Elt
spacer d  = DEvt Spacer d

note      :: Pitch -> Duration -> Elt
note p d  = DEvt (Note p) d


    
--------------------------------------------------------------------------------
-- Favourite instances

-- functor
instance Functor SectionF where
  fmap f (Section tm se)        = Section tm (fmap (fmap f) se)
  
-- Should we recalulate the cache duration? (Is it actually useful?)
instance Functor PhraseF where
  fmap f (Single mo)          = Single (fmap f mo)
  fmap f (Overlay mo smo)     = Overlay (fmap f mo) (fmap (fmap f) smo) 
  
instance Functor MotifF where
  fmap f (Motif se)             = Motif (fmap f se)

-- foldable
instance F.Foldable SectionF where
  foldMap f (Section tm se)     = F.foldMap (F.foldMap f) se
  
instance F.Foldable PhraseF where
  foldMap f (Single mo)       = F.foldMap f mo
  foldMap f (Overlay mo smo)  = F.foldMap (F.foldMap f) (mo <| smo) 
  
instance F.Foldable MotifF where
  foldMap f (Motif se)          = F.foldMap f se
  
  
-- traversable   
instance Traversable PhraseF where
  traverse f (Single mo)      = Single  <$> traverse f mo
  traverse f (Overlay mo smo) = Overlay <$> traverse f mo 
                                        <*> traverse (traverse f) smo
                                          
instance Traversable MotifF where
  traverse f (Motif se)          = Motif <$> traverse f se
  
--------------------------------------------------------------------------------
-- Fits and XOne

instance Fits Elt Duration where
  measure (DEvt e d)          = d
  measure (Mark z)            = duration_zero
  measure (Chord se d)        = d 
  measure (AGrace se p d)     = d
  measure (UGrace p d se)     = d
  
  resizeTo (DEvt e _)        d = DEvt e d
  resizeTo (Mark z)          d = Mark z
  resizeTo (Chord se _)      d = Chord se d 
  resizeTo (AGrace se p _)   d = AGrace se p d
  resizeTo (UGrace p _ se)   d = UGrace p d se
  
instance XOne Elt where
  xone (DEvt e d)           = xone e
  xone (Mark z)             = False
  xone (Chord se d)         = True 
  xone (AGrace se p d)      = True
  xone (UGrace p d se)      = True

instance XOne Evt where
  xone (Note _)             = True
  xone Rest                 = False
  xone Spacer               = False
  
--------------------------------------------------------------------------------
-- drawing to ascii
-- (could now use padToSquare)

draw :: (Fits a Duration, XOne a) => SectionF a -> IO ()
draw (Section tm se) = 
    let xss       = F.foldr (\e a -> drawPhrase tm e : a) [] se
        height    = maximum $ fmap length xss
        xss'      = transpose $ fmap (toWidth . toHeight height) xss
    in mapM_ (putStrLn . intercalate "/") xss'
  where 
    toHeight :: Int -> [[Char]] -> [[Char]]
    toHeight h xs | h > length xs = xs ++ replicate (h - length xs) "" 
                  | otherwise     = xs    
    
    toWidth :: [[Char]] -> [[Char]]
    toWidth xss = let width = maximum $ fmap length xss
                  in fmap (\e -> if null e then (pad width e) else e) xss  
  

pad :: Int -> [Char] -> [Char]
pad n xs = let l = length xs in
           if n <= l then xs else xs ++ replicate (n - l) ' ' 
                               

--- need to pad...
drawPhrase :: (Fits a Duration, XOne a) => TimeSig -> PhraseF a -> [[Char]]
drawPhrase tm (Single mo)         = [drawMotif tm mo]
drawPhrase tm o@(Overlay mo smo)  = drawPad d tm mo : xs where
  d = maxPhraseDuration o
  xs = F.foldr (\e a -> drawPad d tm e : a) [] smo
  
  drawPad :: (Fits a Duration, XOne a) => Duration ->  TimeSig -> MotifF a -> [Char] 
  drawPad d tm mo = let w   = length $ spaces tm d
                        chs = drawMotif tm mo
                    in pad w chs
  
  spaces :: TimeSig -> Duration -> [Char]
  spaces (n,d) dn = xdotAve (n,d) $ singleton dn
  

drawMotif :: (Fits a Duration, XOne a) => TimeSig -> MotifF a -> [Char]
drawMotif tm (Motif se) = xdotAve tm se    

--------------------------------------------------------------------------------
-- midi

-- ss :: Section -> Seq (Seq

linearTransform :: SectionF a -> Seq (Seq a)
linearTransform = fmap joinMotifs . stranspose . sectionContents where
  joinMotifs :: Seq (MotifF a) -> Seq a
  joinMotifs = F.foldl (\a mo -> a >< motifContents mo) empty 
  
 

sectionContents :: SectionF a -> Seq (Seq (MotifF a))
sectionContents (Section _ se) = F.foldl fn empty se where
    fn a ph = a |> phraseContents ph  
  
    phraseContents :: PhraseF a -> Seq (MotifF a)
    phraseContents (Single mo)      = singleton mo
    phraseContents (Overlay mo smo) = mo <| smo

motifContents :: MotifF a -> Seq a
motifContents (Motif se) = se




--------------------------------------------------------------------------------
-- Packing - for Midi


-- Pack vertically (so we have the same number of channels) and 
-- Pack horizontally (so overlays has consistent lengths).
-- Overlays should have the same length within the (vertical) overlay,
-- adjacent horizontal overlays can have different lengths.

-- @ aaa | aaaaa  `becomes` aaa | aaaaa @ 
-- @ bb  | bbbb             bb. | bbbb. @ 
-- @ c   |                  c.. | ..... @ 

-- Also, the last bar of a motifs is spaced if the motif doesn't divide 
-- exactly into bars.

-- Midi and Ascii processing is easier if the section is 'squared' 
-- then we can use the standard @transpose@ function to get a 
-- line-by-line view rather than a column-by-column one. 
packToSquare :: Section -> Section
packToSquare s@(Section tm se) = Section tm $ fmap fn se where
    h = sectionHeight s
    fn :: Phrase -> Phrase
    fn ph = spacerOverlayPack (regularPhraseDuration tm ph) h ph

-- LilyPond and Abc (via HNotate) handle overlays themselves, so we 
-- just spacer pack the motifs inside a phrase with this transformation.
-- This stops us having \ragged overlays\.  
packToLength :: Section -> Section
packToLength s@(Section tm se) = Section tm $ fmap fn se where
    fn :: Phrase -> Phrase
    fn ph = spacerPackLength (regularPhraseDuration tm ph) ph
    
    

sectionHeight :: SectionF a -> Int
sectionHeight (Section _ se) = smaximum $ fmap phraseHeight se where

phraseHeight :: PhraseF a -> Int
phraseHeight (Single _)     = 1
phraseHeight (Overlay _ se) = 1 + S.length se

regularPhraseDuration :: Fits a Duration => TimeSig -> PhraseF a -> Duration
regularPhraseDuration tm ph = bestfit $ divModBar (maxPhraseDuration ph) tm
  where
    bestfit (n,r) | r == duration_zero  = unitDuration tm * fromIntegral n  
                  | otherwise           = unitDuration tm * fromIntegral n+1

maxPhraseDuration :: Fits a Duration => PhraseF a -> Duration
maxPhraseDuration (Single mo)       = sumMeasure mo
maxPhraseDuration (Overlay mo smo)  = 
    max (sumMeasure mo) (smaximum $ fmap sumMeasure smo)

  
-- This 'squares' a phrase - it is extended horizontally so all overlays
-- are the same length. It is also extended vertically (adding blank 
-- motifs where necessary) so each phrase has the same numer of overlays.
-- The vertical transformation simplifies later processig for Midi and ascii
-- but it is not appropriate for LilyPond and Abc (via HNotate)      
spacerOverlayPack :: Duration -> Int -> Phrase -> Phrase
spacerOverlayPack w h (Single mo) 
    | h <= 1      = Single $ spacerPack w mo
    | otherwise   = Overlay (spacerPack w mo) 
                            (sreplicate (h-1) (motif +- spacer w)) 
spacerOverlayPack w h (Overlay mo smo) = Overlay (spacerPack w mo) smo' where
    smo' = (fmap (spacerPack w) smo) >< blanks
    blanks = sreplicate (h - (S.length smo + 1)) (motif +- spacer w) 

-- This is the alternative phrase spacer formulation for HNotate (Abc and
-- LilyPond) it only spaces horizontally. No extra voice overlays are added.
spacerPackLength :: Duration -> Phrase -> Phrase
spacerPackLength w (Single mo)        = Single $ spacerPack w mo
spacerPackLength w (Overlay mo smo)   = 
    Overlay (spacerPack w mo) (fmap (spacerPack w) smo) 

                                
                            
spacerPack :: Duration -> Motif -> Motif
spacerPack d (Motif se) = work d (sumMeasure se) where
  work d l | l >= d     = Motif se      -- (>d) could throw error instead
           | otherwise  = Motif $ se |> spacer (d - l)
           



                          
                         



    