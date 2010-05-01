{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Tactus.Base
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Manipulating meter patterns
--
--------------------------------------------------------------------------------


module Tactus.Base
  (
    MeterPattern
  , Alg                        -- opaque

  , dim
  , one
  , (+++)
  , aug
  , div2
  
  , runAlg

  ) where

import Tactus.Utils

import Data.Ratio

type MeterPattern = [Rational]

type InterpDuration a = Rational -> a

-- an algebra is parameterized with an interpretation function...

newtype Alg a = Alg { 
    getAlg :: InterpDuration a -> MeterPattern -> (H a, MeterPattern) }



dim :: Alg a
dim = Alg $ \ f u -> step f u 
  where
    step _ []     = (id,[])
    step f (r:rs) = let (n,d) = viewRational r 
                    in (replicateH (fromIntegral n) (f $ 1%d), rs)


-- 
one :: Alg a
one = Alg $ \ f u -> step f u
  where
    step _ []     = (id,[])
    step f (n:ns) = (consH $ f n, ns) 



(+++) :: Alg a -> Alg a -> Alg a
(+++) af ag = Alg $ \ f ns -> let (h1,ns')  = (getAlg af) f ns 
                                  (h2,ns'') = (getAlg ag) f ns'
                              in (h1 . h2, ns'')


-- Clever, is the (necessary) type restriction on the first 
-- parameter too restrictive though?
--
crushWith :: Alg Rational -> ([Rational] -> Rational) -> Alg a
crushWith alg sf = Alg $ \ f u -> let (h,ns') = (getAlg alg) id u
                                  in (consH $ f $ summarize sf h, ns') 

  


summarize :: ([a] -> a) -> H a -> a
summarize f hf = f $ hf [] 

 
aug :: Int -> Alg a
aug i | i < 1 = error "aug - must always consume some input"
aug i         = (step i one) `crushWith` sum
  where
    step 1 alg = alg
    step n alg = step (n-1) (alg +++ one)


-- This has scope for improvement as rationals are now used...
--
div2 :: (Rational -> (Rational,Rational)) -> Alg a
div2 df = Alg $ \ f u -> step f u
  where
    step _ []     = (id,[])
    step f (n:ns) = let (a,b) = df n in  (consH (f a) . consH (f b), ns) 




runAlg :: (Rational -> a) -> MeterPattern -> Alg a -> [a]
runAlg tc ns alg = fromH $ fst $ (getAlg alg) tc (cycle ns)

