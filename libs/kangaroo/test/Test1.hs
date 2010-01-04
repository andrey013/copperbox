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
    P.writePickle "test03.bin" (P.cstring "A.....abcde")
    --
    putStrLn "Dalpunto - should see '.' "
    runKangaroo pDalpunto "test03.bin" >>= (putStr . show)

-- These should cause an error...
    -- 
--    putStrLn "Alfermata - should see 'd' "
--    runKangaroo pAlfermata "test03.bin" >>= print
    -- 
--    putStrLn "Alfine - should see 'B' "
--    runKangaroo pAlfine "test03.bin" >>= print
  where
    pDalpunto  = intraP Dalpunto
--    pAlfermata = intraP Alfermata
--    pAlfine    = intraP Alfine

    intraP coda =  do 
      ch  <- char
      str <- advance "region1" coda 2 (count 3 char)
      end <- char
      return ((ch,str),end)


test04 :: IO ()
test04 = do 
    P.writePickle "test04.bin" (P.cstring "12345678901234567890*....")
    runKangaroo p1 "test04.bin" >>= print
  where
    p1 = do 
     a <- word8ByteString 20  
     b <- char
     return (a,b)

   