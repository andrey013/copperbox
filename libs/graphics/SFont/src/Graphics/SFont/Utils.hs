{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.Utils
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils
--
--------------------------------------------------------------------------------


module Graphics.SFont.Utils where

import Graphics.SFont.PrimitiveDatatypes
import Graphics.SFont.ExtraSyntax


import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( isPrint )
import Data.Int
import Data.List ( foldl' ) 
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Word



class Meaning a where meaning :: a -> String


-- Model C land booleans...

class IntegralBool a where
    boolValue :: a -> Bool
    
    isTrue  :: a -> Bool
    isTrue = boolValue
    
    isFalse :: a -> Bool       
    isFalse = not . boolValue
    
instance IntegralBool Word32 where
    boolValue 0 = True
    boolValue _ = False
    


section :: Int -> Int -> BS.ByteString -> BS.ByteString 
section start len inp
    | start + len <= BS.length inp  = BS.take len $ BS.drop start inp 
    | otherwise                     = error $ 
          "section - out-of-bounds, from " ++ show start ++ " for"
                                           ++ show len ++ " on length " 
                                           ++ show (BS.length inp) 


-- Should unbits have a stronger marshalling constraint than Enum?
-- It used to have a dedicated type class /Unmarshal/.

unbits :: (Bits a, Ord a, Enum b) => a -> [b]
unbits v = step v (szmax - 1) where
    step a i | a <= 0     = []
             | i <  0     = []  -- unreachable?
             | otherwise  = let c = a `clearBit` i in
                            if c == a then step c (i-1) 
                                      else (toEnum i) : step c (i-1) 
    szmax = bitSize v 
    


segment :: [Int] -> [a] -> [[a]]
segment []      xs = [xs]
segment [i]     xs = let (l,r) = splitAt (1 + fromIntegral i) xs in 
                     if null r then [l] else l:[r] 
segment (i:ix)  xs = let (l,r) = splitAt i xs in l : segment ix r  

-- regionBetween extracts the region between 2 locations 
regionBetween :: Int -> Int -> Region
regionBetween start end = (start, end - start)

putStrLnSafe :: String -> IO ()
putStrLnSafe = putStrLn . map replaceUnprint where
    replaceUnprint ch | isPrint ch  = ch
                      | otherwise   = '.'

zeroBasedIntMap :: [a] -> IntMap.IntMap a 
zeroBasedIntMap = snd . foldl' fn (0,IntMap.empty) where
  fn (i,m) e = (i+1,IntMap.insert i e m)

  
buildMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map.Map k v
buildMap g h = foldr fn Map.empty where
  fn e m = Map.insert (g e) (h e) m
  


tableLocation :: String -> TableLocs -> Maybe Region
tableLocation = Map.lookup


 
  
                   