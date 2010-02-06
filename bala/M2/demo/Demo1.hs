{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- TODO check parallelMusic in LilyPond...


module Demo1 where


import M2.Bracket
import M2.Duration
import M2.LilyPondDoc
import M2.LilyPondOutput
import M2.OneList
import M2.Pitch
import M2.Segment
import M2.Syntax
import M2.Unfold

import Data.Ratio

instance NumMeasured Int where
  type Measurement Int = DurationMeasure
  nmeasure i = (fromIntegral i)%1

test01 :: ([OneMany Int], [Int])
test01 = beamSegment [4,4] [2,2,4,4]

test02 :: ([OneMany Int], [Int])
test02 = beamSegment [] [2,2,4,4]

test03 :: ([OneMany Int], [Int])
test03 = beamSegment [4,4,4] [2,2,4,4]

test04 :: ([OneMany Int], [Int])
test04 = beamSegment [4,4,4,4] [2,2,4,4,2,2]



demo1 = segStep [2,2,3::Int] (4::DurationMeasure) 


sdemo1 = beam1 (4%1) [2,1,1,4,4::Int]

demo2 :: [[Int]]
demo2 = segment [2,2,2,4,2,1,3] [4,4,4,4,4,4::DurationMeasure]

-- demo2' :: ([Int], (CB Int,[Int]))
demo2' = segStep [4,2,1,3::Int] (4::DurationMeasure) -- [4,4,4,4,4,4,4,4]

demo3 :: [[Int]]
demo3 = segment [1,2,3,4,5,6] [4,4,4,4,4,4]




asplitAt :: Int -> [a] -> ([a],[a])
asplitAt i xs = post $ aUnfoldr phi (i,xs) where
  phi (i,a:as) | i > 0  = AYield a (i-1,as)
  phi s                 = ADone s   

  post (xs,(_,ys)) = (xs,ys)

demoU1 = asplitAt 4 [1..10]

asplitAt2 :: Int -> [a] -> ([a],[a])
asplitAt2 i xs = post $ aUnfoldMap phi i xs where
  phi a i | i > 0     = AYield a (i-1)
          | otherwise = ADone 0

  post (a,b,_) = (a,b)

demoU2 = asplitAt2 4 [1..10]


asplitAt3 :: Int -> [a] -> ([a],[a])
asplitAt3 = apoUnfoldMap phi where
  phi a i | i > 0     = Just (a,i-1)
          | otherwise = Nothing 


demoU3 = asplitAt3 4 [1..10]


filterU1 :: (a -> Bool) -> [a] -> [a]
filterU1 p = skipUnfoldr phi where
  phi []                 = SDone
  phi (a:as) | p a       = SYield a as
             | otherwise = SSkip as


demoF1 = filterU1 even [1..20]

 
-- Don't need the state for this one 
filterU2 :: (a -> Bool) -> [a] -> [a]
filterU2 p = fst . skipUnfoldMap phi ()  where
  phi a st  | p a       = SYield a st
            | otherwise = SSkip st


demoF2 = filterU2 even [1..20]


--------------------------------------------------------------------------------


demoMul = multiUnfold [f1,f2] 0 where
   f1 n | n == 5     = Done
        | otherwise = Yield 'a' (n+1)

   f2 n | n == 10    = Done
        | otherwise = Yield 'b' (n+1)


demoInt01 :: [Int]
demoInt01 = interlockUnfold phi [2,4,10] 0 where
  phi n i | i == n    = Done     -- Done with st might be better so (allowing reset)
          | otherwise = Yield i (i+1)

