{-# OPTIONS -Wall #-}


module Demo01 where

import Data.HeadStrictStream

import Prelude hiding ( head, repeat, take, const, map, iterate, drop )


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