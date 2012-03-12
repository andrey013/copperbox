{-# OPTIONS -Wall #-}

module Demo01 where

import DirectoryMetrics.Main
import DirectoryMetrics.HierSyntax
import DirectoryMetrics.ParserCombinators
import DirectoryMetrics.Pretty
import DirectoryMetrics.SizeMetrics
import DirectoryMetrics.StructureMetrics
import DirectoryMetrics.TreeView
import DirectoryMetrics.WindowsParser

import Control.Applicative
import Data.Monoid

demo01 = main2 [] ["input/metrics.txt"] []
demo02 = main2 [] ["input/haskell-libs.txt"] []

demo03 = fmap head <$> toplevelsFromFile "input/metrics.txt"

demo04 :: IO ()
demo04 = do 
    ans <- toplevelsFromFile "input/metrics.txt"
    case ans of
      Left err -> error $ show err
      Right xs -> putStr $ "... " ++ (show $ sk xs)
  where
    sk = sizeMetricsF . streamFlat

demo05 = fmap sk <$> toplevelsFromFile "input/haskell-libs.txt"
  where
    sk = map ppBothMetrics . bothMetricsF . streamFlat

demo05x = toplevelsFromFile "input/haskell-libs.txt" >>= 
          either (error "here") sk
  where
    sk = drawDirectories . streamFlat

demo06 = fmap sk <$> toplevelsFromFile "input/metrics.txt"
  where
    sk = map ppBothMetrics . bothMetricsF . streamFlat
    

demo07 :: IO ()
demo07 = do 
    ans <- toplevelsFromFile "input/haskell-libs.txt"
    case ans of
      Left err -> error $ show err
      Right xs -> mapM_ (putStrLn . show) $ sk xs
  where
    sk = map ppBothMetrics . bothMetricsF . streamFlat





-- Should succeed...
temp01 = runParserEither int2 "123456 7"
  where
    int2 = (,) <$> int <*> (space *> int)

temp02 = fmap head <$> toplevelsFromFile "input/broken.txt"
temp02a = fmap third <$> toplevelsFromFile "input/broken.txt"
  where
    third (a:b:c:_) = c
    third xs        = error $ show $ length xs

temp02b = fmap second <$> toplevelsFromFile "input/broken.txt"
  where
    second (a:b:_) = b
    second xs        = error $ show $ length xs

-- Should fail...
temp03 = runParserEither int2 "1234567"
  where
    int2 = (,) <$> int <*> (space *> int)

-- Should fail
temp04 = runParserEither dt "12/01/2012 09:37"
  where
    dt = (,) <$> dateTime <*> int
