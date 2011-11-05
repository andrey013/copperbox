{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.PDSS.PDDoc
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Gen monad
--
--------------------------------------------------------------------------------


module Sound.PDSS.PDDoc
  ( 


    rec_canvas
  , rec_text


  ) where 


import Sound.PDSS.Utils.FormatCombinators

chunkA :: Doc
chunkA = char '#' <> char 'A'

chunkN :: Doc
chunkN = char '#' <> char 'N'

chunkX :: Doc
chunkX = char '#' <> char 'X'

-- Print records

rec_canvas :: Int -> Int -> Int -> Int -> Int -> Doc
rec_canvas  x y w h  font_size = 
    chunkN <+> text "canvas" 
           <+> (hsep $ map int [x,y,w,h,font_size]) <> char ';'


rec_text :: Int -> Int -> String -> Doc
rec_text x y ss = 
    chunkX <+> text "text" <+> int x <+> int y <+> text ss <> char ';'


