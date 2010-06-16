{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Temp01 where

import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.ModularSyntax
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondTrafo
import Neume.Core.Utils.HList
import qualified Neume.Core.Utils.Lahl as L
import Neume.Core.Utils.Stream ( Stream(..) )
import qualified Neume.Core.Utils.Stream as S
import Neume.Core.Utils.TraceT

import Text.PrettyPrint.Leijen hiding ( (<$>) )

import MonadLib.Monads

import Data.Ratio

eighth_note :: DurationMeasure 
eighth_note = (1%8)


demo1 :: [Int] 
demo1 = S.take 10 $ S.cycle [1,2,3]

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
 
instance DMeasure Int where
  dmeasure i = (fromIntegral i)%1


demo2 = fitMeasure (1::Int) (2%1)

type PulseLenStack = Stream DurationMeasure
type BarSizeStack  = Stream Int

makeBars :: [Pulse e] -> BarSizeStack -> Phrase [MetricalDiv e]
makeBars es = Phrase . step es
  where
    step [] _           = []
    step es (z ::: sz)  = let (tip,rest) = splitAt z es 
                          in makeDivs tip : step rest sz

makeDivs :: [Pulse e] -> [MetricalDiv e]
makeDivs []     = []
makeDivs (x:xs) = step x xs emptyH
  where
    step Space     []     acc = toListH acc
    step Space     (y:ys) acc = step y ys acc
--    step (Group h) (y:ys) acc = step y ys (acc `appendH` (


-- with beamStart2 if you go into beamGroup mode  
-- you'll always produce a beam group...
-- 
-- err - no you won't ... (1%8) rest rest (1%4)

-- beam has a concrete start of at least 1 and a speculative list...

-- might want an optimization on the 1 element-Pulse case 


data BeamState e = BS { prolog :: H e, speculation :: H e }

{-
speculate :: BeamState e -> e -> BeamState e
speculate (BS p s) e = BS p (s `snocH` e)

addConcrete :: BeamState e -> e -> BeamState e
addConcrete (BS p s) e = 
-}

-- It\'s assumed: DMeasuure <= eighth
--
addInside :: BeamExtremity e => e -> BeamState e -> BeamState e
addInside e (BS p s) 
    | rendersToNote e = BS (p `appendH` s `snocH` e) emptyH
    | otherwise       = BS p                         (s `snocH` e)

inside2 :: BeamExtremity e => e -> e -> BeamState e
inside2 a b | rendersToNote b = BS ((a:) . (b:)) emptyH
inside2 a b                   = BS (a:)   (b:)


traceBuffer :: (Monad m, TraceM m (MetricalDiv e)) => BeamState e -> m ()
traceBuffer (BS p s) = 
    (trace $ beamOrOne p) >> mapM_ traceAtom (toListH s)
  where
    beamOrOne f = zom emptyH (wrapH . Atom) (wrapH . Beamed . map Atom) $ f []

zom :: b -> (a -> b) -> ([a] -> b) -> [a] -> b
zom zero _    _     []  = zero
zom _    oneF _     [x] = oneF x
zom _    _    manyF xs  = manyF xs

newtype I a = I a deriving (Eq,Show)

type family NoteListElem e :: *

class ToMetricalDiv repr where
  toMetricalDiv :: e ~ NoteListElem repr => repr -> MetricalDiv e

type instance NoteListElem (Division e) = e
-- type instance NoteListElem e = e

type instance NoteListElem (I e) = e

class ToMetricalDiv2 repr where
  toMetricalDiv2 :: repr e -> MetricalDiv e


-- Having Notelists as lists of some type /e/, rather 
-- than some type-constructor over /e/ is a problem.
--
-- It means the /atom/ function is at two different types...
--
atom :: e -> MetricalDiv e
atom e = Atom e

atomZ :: Division e -> MetricalDiv e
atomZ (Elem e)    = Atom e 
atomZ (Plet m es) = N_Plet m (map atomZ es)

instance ToMetricalDiv2 I where
 toMetricalDiv2 (I e) = Atom e

instance ToMetricalDiv2 Division where
  toMetricalDiv2 (Elem e)    = Atom e
  toMetricalDiv2 (Plet m es) = N_Plet m (map atomZ es)

beamStart2 :: (BeamExtremity e, DMeasure e) => e -> e -> Bool
beamStart2 e1 e2 = rendersToNote e1
                && dmeasure e1 <= eighth_note
                && dmeasure e1 <= eighth_note

evalTrace :: TraceT e Id a -> H e
evalTrace = snd . runId . runTraceT 

traceAtom :: TraceM m (MetricalDiv e) =>  e -> m ()
traceAtom = trace1 . Atom

dividePulse :: (BeamExtremity e, DMeasure e) => Pulse e -> H (MetricalDiv e)
dividePulse Space     = emptyH
dividePulse (Group h) = zom emptyH (wrapH . Atom) (evalTrace . outside) (h [])
  where
    outside (x:y:zs) | beamStart2 x y = inside (inside2 x y) zs
                     | otherwise      = traceAtom x >> outside (y:zs)
    outside zs                        = trace (veloH Atom zs)

    inside buf []                     = traceBuffer buf
    inside buf (x:xs) 
        | dmeasure x > eighth_note    = traceBuffer buf >> traceAtom x >> outside xs
        | otherwise                   = inside (addInside x buf) xs



dividePulse1 :: forall e. (BeamExtremity e, DMeasure e) 
            => Pulse e -> H (MetricalDiv e)
dividePulse1 Space     = emptyH
dividePulse1 (Group h) = outside emptyH (toListH h)
  where
    outside :: H (MetricalDiv e) -> [e] -> H (MetricalDiv e)
    outside acc []     = acc
    outside acc (e:es) 
        | dmeasure e > eighth_note = outside (acc `snoc1` e) es
        | not (rendersToNote e)    = outside (acc `snoc1` e) es
        | otherwise                = inside acc [e] es

    inside acc reva []     = acc `appendH` unwind reva
    inside acc reva (e:es) 
        | dmeasure e > eighth_note = acc `appendH` unwind (e:reva)
        | otherwise                = inside acc (e:reva) es

    -- unwind works on a reversed list
    unwind :: [e] -> H (MetricalDiv e) 
    unwind []     = emptyH
    unwind (e:es) | not (rendersToNote e) = unwind es `snoc1` e
                  | otherwise             = mkbeam $ reverse (e:es)

    snoc1 :: H (MetricalDiv e) -> e -> H (MetricalDiv e)    
    snoc1 f e = f `snocH` Atom e

    mkbeam :: [e] -> H (MetricalDiv e)
    mkbeam []  = emptyH
    mkbeam [e] = wrapH $ Atom e
    mkbeam es  = wrapH $ Beamed (map Atom es)



segment :: DMeasure e => [e] -> PulseLenStack -> [Pulse e]
segment notes (top ::: pls) = group notes top pls L.empty
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


demo3 = S.take 5 $ stkMinus 5 $ S.cycle [4,5,5,5,5,5,5]