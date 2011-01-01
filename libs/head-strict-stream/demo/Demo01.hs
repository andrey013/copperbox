{-# OPTIONS -Wall #-}


module Demo01 where

import Data.HeadStrictStream

import Prelude hiding ( head, repeat, take, const, map, iterate, drop
                      , dropWhile, filter, zip, zipWith, unzip )


demo01 :: Int
demo01 = head $ repeat 3

demo02 :: [Int]
demo02 = take 10 $ const 1

demo03 :: Stream Char
demo03 = map (\_ -> '*') $ const (1::Int)

demo04 :: Stream Int
demo04 = cons 999 $ repeat 1

demo05 :: Stream Int
demo05 = intersperse 999 $ repeat 1

demo06 :: Stream Int
demo06 = interleave (repeat 999) (repeat 0)

demo07 :: Stream Int
demo07 = iterate (+1) 0

demo08 :: Stream Int
demo08 = drop 5 $ iterate (+1) 0

demo09 :: Stream Int
demo09 = dropWhile (<5) $ iterate (+1) 0

demo10 :: Stream Int
demo10 = filter odd $ iterate (+1) 0

demo11 :: Stream (Int,Char)
demo11 = zip (iterate (+1) 0) (repeat 'a')

demo12 :: Stream Int
demo12 = zipWith (+) (iterate (+1) 0)  (iterate (+1) 0)

demo13 :: (Stream Int, Stream Int)
demo13 = branch (iterate (+1) 0)

demo14 :: (Stream Int, Stream Char)
demo14 = unzip $ zip (iterate (+1) 0) (repeat 'a')


demo15 :: (Stream Int, Stream Int)
demo15 = partition even $ iterate (+1) 0


demo16 :: Stream Int
demo16 = (iterate (+1) 0) + 1
