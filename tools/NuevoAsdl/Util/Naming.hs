
module Util.Naming where

import Data.Char



-- Naming functions  
u1 :: String -> String
u1 (a:as) | isLower a = (toUpper a) : as
                | otherwise = a:as
              
l1 :: String -> String
l1 (a:as) | isUpper a = (toLower a) : as
                | otherwise = a:as              

                
                                
haskellName :: String -> String
haskellName xs = changer (u1 xs) []
  where
    changer []         acc  = reverse acc
    changer ('_':x:xs) acc  = changer xs (toUpper x : acc)
    changer (x:xs) acc  = changer xs (x : acc)