{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- TODO check parallelMusic in LilyPond...


module Demo1 where


import M2.Bracket
import M2.Duration
import M2.LilyPondDoc
import M2.LilyPondOutput
import M2.Pitch
import M2.Segment
import M2.Syntax
import M2.Unfold

import Data.Ratio

instance NumMeasured Int where
  type Measurement Int = Int
  nmeasure i = i

demo1 = segStep [2,2::Int,3] (4::Int) 


demo2 :: [[Int]]
demo2 = segment [2,2,4,2,1,3] [4,4,4,4,4,4]

demo2' :: ([Int], (CB Int,[Int]))
demo2' = segStep [4,2,1,3] 4 -- [4,4,4,4,4,4,4,4]

demo3 :: [[Int]]
demo3 = segment [1,2,3,4,5,6] [4,4,4,4,4,4]




asplitAt :: Int -> [a] -> ([a],[a])
asplitAt i xs = post $ aunfoldr phi (i,xs) where
  phi (i,a:as) | i > 0  = AYield a (i-1,as)
  phi s                 = ADone s   

  post (xs,(_,ys)) = (xs,ys)

demoU1 = asplitAt 4 [1..10]

asplitAt2 :: Int -> [a] -> ([a],[a])
asplitAt2 i xs = post $ aunfoldMap phi i xs where
  phi a i | i > 0     = AYield a (i-1)
          | otherwise = ADone 0

  post (a,b,_) = (a,b)

demoU2 = asplitAt2 4 [1..10]


asplitAt3 :: Int -> [a] -> ([a],[a])
asplitAt3 = apoUnfoldMap phi where
  phi a i | i > 0     = Just (a,i-1)
          | otherwise = Nothing 


demoU3 = asplitAt3 4 [1..10]

