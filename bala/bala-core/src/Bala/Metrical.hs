{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Metrical
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


module Bala.Metrical 
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

type MeterPattern = [Int]

newtype Alg a = Alg { 
    getAlg :: (Int -> a) -> MeterPattern -> (H a, MeterPattern) }

dim :: Alg a
dim = Alg $ \ f u -> step f u 
  where
    step _ []     = (id,[])
    step f (n:ns) = (replicateH n (f 1), ns) 


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
crushWith :: Alg Int -> ([Int] -> Int) -> Alg a
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

    
div2 :: (Int -> (Int,Int)) -> Alg a
div2 df = Alg $ \ f u -> step f u
  where
    step _ []     = (id,[])
    step f (n:ns) = let (a,b) = df n in  (consH (f a) . consH (f b), ns) 




runAlg :: (Int -> a) -> MeterPattern -> Alg a -> [a]
runAlg tc ns alg = fromH $ fst $ (getAlg alg) tc (cycle ns)



--------------------------------------------------------------------------------


type H a = [a] -> [a]


fromH :: H a -> [a]
fromH = ($ [])

consH :: a -> H a
consH a = (a:)

replicateH :: Int -> a -> H a
replicateH n a = step n id where
  step i f | i <= 0    = f
           | otherwise = step (i-1) (f .  (a:))  
