{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Bracket
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------

module Neume.Core.Bracket 
  (
    PulseLenStack
  , BarSizeStack
  , BracketConfig
  , bracketConfig

  , makeFull   
  , MDiv(..)
  , phrase

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.Syntax

import Neume.Core.Utils.HList
import qualified Neume.Core.Utils.Lahl          as L
import Neume.Core.Utils.SnocTraceT
import Neume.Core.Utils.Stream ( Stream(..) )
import qualified Neume.Core.Utils.Stream        as S

import MonadLib.Monads

import Data.Ratio


type PulseLenStack = Stream DurationMeasure
type BarSizeStack  = Stream Int

type BracketConfig = (PulseLenStack,BarSizeStack)

bracketConfig :: MeterPattern -> BracketConfig
bracketConfig xs = (S.cycle xs, S.cycle [length xs])

eighth_note :: DurationMeasure 
eighth_note = (1%8)



makeFull :: (BeamExtremity (repr gly), DMeasure  (repr gly), MDiv  repr) 
         => BracketConfig -> NoteList (repr gly) -> Full gly
makeFull bc = Full . phrase bc


data Pulse a = Space | Group (H a)

data FitMeasure r = Fits
                  | Underflow r
                  | Overflow  r
  deriving (Eq,Show)

fitMeasure :: DMeasure e 
           => e -> DurationMeasure -> FitMeasure DurationMeasure
fitMeasure e = step (dmeasure e) where
    step dm r | dm == r        = Fits
              | dm >  r        = Overflow  $ dm - r
              | otherwise      = Underflow $ r - dm
 


phrase :: (BeamExtremity (repr e), DMeasure (repr e), MDiv repr)
       => (PulseLenStack,BarSizeStack) 
       -> NoteList (repr e) 
       -> Phrase [MetricalDiv e]
phrase (pstk,bstk) = bars bstk . segment pstk . getNotes


-- with beamStart2 if you go into beamGroup mode  
-- you'll always produce a beam group...
-- 
-- err - no you won't ... (1%8) rest rest (1%4)

-- beam has a concrete start of at least 1 and a speculative list...

-- might want an optimization on the 1 element-Pulse case 


data BeamState e = BS 
      { prolog      :: H (MetricalDiv e)
      , speculation :: H (MetricalDiv e) 
      }


-- It\'s assumed: DMeasuure <= eighth
--
addInside :: (BeamExtremity (repr e), MDiv repr) 
          => repr e -> BeamState e -> BeamState e
addInside e (BS p s) 
    | rendersToNote e = BS (p `appendH` s `snocH` (mdiv e)) emptyH
    | otherwise       = BS p                                (s `snocH` (mdiv e))

inside2 :: (BeamExtremity (repr e), MDiv repr) 
        => repr e -> repr e -> BeamState e
inside2 a b | rendersToNote b = BS ((mdiv a:) . (mdiv b:)) emptyH
inside2 a b                   = BS (mdiv a:)   (mdiv b:)


traceBuffer :: (Monad m, TraceM m (MetricalDiv e)) 
            => BeamState e -> m ()
traceBuffer (BS p s) = trace (fn $ toListH p) >> trace s
  where
    fn []  = id
    fn [x] = wrapH x
    fn xs  = wrapH $ Beamed xs



class MDiv repr where
  mdiv :: repr e -> MetricalDiv e


instance MDiv Item where
  mdiv (Item e) = Atom e

instance MDiv Division where
  mdiv (Elem e)    = Atom e
  mdiv (Plet m es) = N_Plet m (map mdiv es)

beamStart2 :: (BeamExtremity e, DMeasure e) => e -> e -> Bool
beamStart2 e1 e2 = rendersToNote e1
                && dmeasure e1 <= eighth_note
                && dmeasure e1 <= eighth_note

evalTrace :: TraceT e Id a -> H e
evalTrace = snd . runId . runTraceT 

traceAtom :: (TraceM m (MetricalDiv e), MDiv repr) =>  repr e -> m ()
traceAtom = trace1 . mdiv


-- Note - consuming the the pulse with /outside/ uses so much
-- machinery that we avoid it for the the empty and one-element
-- cases... 

dividePulse :: (BeamExtremity (repr e), DMeasure (repr e), MDiv repr) 
            => Pulse (repr e) -> H (MetricalDiv e)
dividePulse Space     = emptyH
dividePulse (Group h) = case toListH h of
                          []  -> emptyH 
                          [e] -> wrapH $ mdiv e 
                          es  -> evalTrace $ outside es
  where
    outside (x:y:zs) | beamStart2 x y = inside (inside2 x y) zs
                     | otherwise      = traceAtom x >> outside (y:zs)
    outside zs                        = trace (veloH mdiv zs)

    inside buf []                     = traceBuffer buf
    inside buf (x:xs) 
        | dmeasure x > eighth_note    = traceBuffer buf >> traceAtom x >> outside xs
        | otherwise                   = inside (addInside x buf) xs

bars :: (BeamExtremity (repr e), DMeasure (repr e), MDiv repr) 
     => BarSizeStack -> [Pulse (repr e)] -> Phrase [MetricalDiv e]
bars (top ::: stk) xs = Phrase $ step (splitAt top xs) stk
  where
    step ([],[])   _         = []
    step (ps,[])   _         = [mkBar ps]
    step (ps,rest) (t ::: s) = mkBar ps : step (splitAt t rest) s

    mkBar = toListH . concatH . map dividePulse 

segment :: DMeasure (repr e) => PulseLenStack -> [repr e] -> [Pulse (repr e)]
segment (top ::: pls) notes = group notes top pls L.empty
  where  
    -- Input exhausted...
    group []     _ _              acc = 
        if L.length acc <= 0 then [] else [Group $ L.getH acc]
        
    -- Building up a pulse...
    group (e:es) a stk@(z ::: sz) acc = case fitMeasure e a of
        Fits        -> pulse acc e : group es z sz L.empty
        Underflow r -> group es r stk (acc `L.snoc` e)  
        Overflow  r -> pulse acc e : overflow r es sz

    -- Subtracting the overflow carry from the stack... 
    -- Potentially the overflow carry is larger the the next 
    -- pulse length so we might have to produce a @Space@ in
    -- the output...
    --
    overflow r inp stk = case decrement r stk of
        (Just r',stk')     -> Space : overflow r' inp stk'
        (Nothing,z ::: sz) -> group inp z sz L.empty
                           
    pulse acc e = Group $ L.getH (acc `L.snoc` e)




-- If r == 0 produce (Just 0, stk_tail)
decrement :: DurationMeasure 
          -> PulseLenStack 
          -> (Maybe DurationMeasure, PulseLenStack)
decrement r (a ::: sa) | r <  a    = (Nothing, (a - r) ::: sa)
                       | otherwise = (Just (r - a), sa)

-- This is 'dangerous' for the segmenting algorithm:
-- we have no idea how many pulses are "popped" if r is too big
-- 
stkMinus :: DurationMeasure -> PulseLenStack -> PulseLenStack
stkMinus r (a ::: sa) | r == a    = sa
                      | r > a     = stkMinus (r - a) sa
                      | otherwise = a - r ::: sa  

