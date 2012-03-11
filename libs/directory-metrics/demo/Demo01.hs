{-# OPTIONS -Wall #-}

module Demo01 where

import DirectoryMetrics.Main
import DirectoryMetrics.WindowsParser
import DirectoryMetrics.ParserCombinators
import DirectoryMetrics.HierSyntax

import Control.Applicative

demo01 = main2 [] ["input/haskell-libs.txt"] []


demo02 = fmap head <$> toplevelsFromFile "input/broken.txt"
demo02a = fmap third <$> toplevelsFromFile "input/broken.txt"
  where
    third (a:b:c:_) = c
    third xs        = error $ show $ length xs

demo02b = fmap second <$> toplevelsFromFile "input/broken.txt"
  where
    second (a:b:_) = b
    second xs        = error $ show $ length xs

-- Should succeed...
test01 = runParserEither int2 "123456 7"
  where
    int2 = (,) <$> int <*> (space *> int)

-- Should fail...
test02 = runParserEither int2 "1234567"
  where
    int2 = (,) <$> int <*> (space *> int)

-- Should fail
test03 = runParserEither dt "12/01/2012 09:37"
  where
    dt = (,) <$> dateTime <*> int
