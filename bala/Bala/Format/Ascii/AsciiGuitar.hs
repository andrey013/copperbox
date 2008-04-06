--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Ascii.AsciiGuitar
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print ascii guitar chords
--
--------------------------------------------------------------------------------


module Bala.Format.Ascii.AsciiGuitar where

import Data.List

--------------------------------------------------------------------------------
-- ascii art fretboard 
--------------------------------------------------------------------------------


cycleOf i ss = let i' = i * length ss in take i' $ cycle ss

nutline = replicate  11 '='

labelledFretline i = show i ++ "." ++ fretline

fretline :: String 
fretline = '+' :  cycleOf 5 "-+"

openline :: String
openline = '|' : cycleOf 5 " |"

fret []       = ""
fret (' ':xs) = '|' : ' ' : fret xs
fret ('o':xs) = '@' : ' ' : fret xs 

spaces :: [Char] -> String
spaces = intersperse ' '

fretl = spaces . map rewrite
  where 
    rewrite 'o' = '@'
    rewrite '.' = '|'
    rewrite ch  = ch

topl = spaces . map rewrite
  where 
    rewrite '.' = ' '
    rewrite ch  = ch
        
        
chord :: Maybe Int -> String -> [String] -> String
chord mi ss xs = 
    unlines $ reverse $ foldl (\xs a -> pre fretline : pre (fretl a)  : xs)  initial xs
  where 
    initial = [maybe nutline labelledFretline mi, pre $ topl ss]
    pre ss = (' ':' ':ss)
                         
  

demm = chord (Just 8) "x....." [".o.o.o", "....o.", "..o..."]



  
  