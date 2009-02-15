{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Utils
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


module Graphics.OTFont.Utils where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParseMonad ( Region )

import Data.Bits
import qualified Data.ByteString as BS
import Data.Int
import qualified Data.Map as Map
import Data.Word

class Marshal a where marshal :: a -> Int

class Unmarshal a where unmarshal :: Int -> a  

class Meaning a where meaning :: a -> String

class BoundingBox a where 
    x_min           :: a -> Int16
    y_min           :: a -> Int16
    x_max           :: a -> Int16
    y_max           :: a -> Int16


class IntegralBool a where
    boolValue :: a -> Bool
    
    isTrue  :: a -> Bool
    isTrue = boolValue
    
    isFalse :: a -> Bool       
    isFalse = not . boolValue
    
instance IntegralBool Word32 where
    boolValue 0 = True
    boolValue _ = False
    
findTableRegion :: String -> ProtoFace -> Maybe Region
findTableRegion name (ProtoFace _ _ fm) = Map.lookup name fm

section :: Int -> Int -> BS.ByteString -> BS.ByteString 
section start len inp
    | start + len <= BS.length inp  = BS.take len $ BS.drop start inp 
    | otherwise                     = error $ 
          "section - out-of-bounds, from " ++ show start ++ " for"
                                           ++ show len ++ " on length " 
                                           ++ show (BS.length inp) 


unbits :: (Bits a, Ord a, Unmarshal b) => a -> [b]
unbits v = step v (szmax - 1) where
    step a i | a <= 0     = []
             | i <  0     = []  -- unreachable?
             | otherwise  = let c = a `clearBit` i in
                            if c == a then step c (i-1) 
                                      else (unmarshal i) : step c (i-1) 
    szmax = bitSize v 
    
                          