
--------------------------------------------------------------------------------
-- |
-- Module      :  TabApp.StaffLocations 
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Scan a text file to find Staff Locations
--
--------------------------------------------------------------------------------

module TabApp.StaffLocations where

import TabApp.Datatypes


candidateChars :: [Char]
candidateChars = "-|0123456789"

-- LineScore - line_length x coverage_percentage
type LineScore = (Int,Float)

-- A score is both the line length and the coverage percentage 
-- of candidateChars.
-- A very short line containing just a number would score very good
-- coverage but probably wouldn't be part of the tab.  
score :: String -> LineScore
score line = (l,cover) where
    l       = length line
    c       = length $ filter (`elem` candidateChars) line
    cover   = if l == 0 then 0 else (fromIntegral c) / (fromIntegral l) * 100.0

scoreTab :: String -> [LineScore]
scoreTab = fmap score . lines

number :: [a] -> [(Int,a)]
number xs = zipWith (flip (,)) xs [0..]
 