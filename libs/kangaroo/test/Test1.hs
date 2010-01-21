{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Test1
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pickling
--
--------------------------------------------------------------------------------

module Test1 where

import qualified Picklers as P

import Data.ParserCombinators.Kangaroo


main :: IO ()
main = test01

test01 :: IO ()
test01 = do 
  P.writePickle "test01.bin" (P.cstring "Hello world")
  runKangaroo cstring "test01.bin" >>= print


test02 :: IO ()
test02 = do 
    P.writePickle "test02.bin" (P.cstring "AabcdeBCDEFG")
    --
    putStrLn "Dalpunto - should see 'a' "
    runKangaroo pDalpunto "test02.bin" >>= print
    -- 
    putStrLn "Alfermata - should see 'd' "
    runKangaroo pAlfermata "test02.bin" >>= print
    -- 
    putStrLn "Alfine - should see 'B' "
    runKangaroo pAlfine "test02.bin" >>= print
  where
    pDalpunto  = intraP Dalpunto
    pAlfermata = intraP Alfermata
    pAlfine    = intraP Alfine

    intraP coda =  do 
      ch  <- char
      str <- intraparse "region1" coda 1 5 (count 3 char)
      end <- char
      return ((ch,str),end)


test03 :: IO ()
test03 = do 
    P.writePickle "test03.bin" (P.string "ABCDEabcde")
    --
    runKangaroo pRestrict2  "test03.bin" >>= (putStr . show)
  where
    pRestrict2  = do 
      showHexAll
      one <- restrict "upper" Alfine 5 $ runOn char
      two <- restrict "lower" Alfine 5 $ runOn char
      return (one,two)
