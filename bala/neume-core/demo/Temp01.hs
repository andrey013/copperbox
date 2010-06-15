

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

import Text.PrettyPrint.Leijen hiding ( (<$>) )

import Data.Ratio



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




type PulseLenStack = Stream DurationMeasure


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