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

import Bala.Base.Base

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

{-

  E.|--F--+-----+--G--+
  B.|--C--+-----+--D--+
  G.|-----+--A--+-----+
  D.|-----+--E--+--F--+
  A.|-----+--B--+--C--+
  E.|--F--+-----+--G--+

-}  


-- Fretboard

fretboard :: [Pitch] -> Int -> String
fretboard ps i = unlines $ map mkline ps
  where 
    mkline p = take i $ fbstring p $ []

fbstring :: Pitch -> ShowS
fbstring p = affi (pitchName p) . dotS . barS . fn p
  where 
    fn p = let p' = p `addSemi` 1
           in str1 (mkfun p') . fn p'
    
    mkfun p = let lbl = pitchName p in
              if (unaltered lbl) then (Just $ affi lbl) else Nothing
                  

    str1 :: (Maybe ShowS) -> ShowS
    str1 Nothing  = showString "-----+"
    str1 (Just f) = showString "--" . f . showString "--+"


standard_tuning = [e5,b4,g4,d4,a3,e3]     

drawStdFretboard :: Int -> IO ()
drawStdFretboard = putStr . fretboard standard_tuning

 

 