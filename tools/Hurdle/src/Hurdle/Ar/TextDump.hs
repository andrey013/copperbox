{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Ar.TextDump
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  
--
-- Pretty print as text...
--
--------------------------------------------------------------------------------

module Hurdle.Ar.TextDump where

import Hurdle.Base.Utils ( applyfs )
import Hurdle.Ar.Datatypes

import Data.Char
import Numeric
import Text.PrettyPrint.HughesPJ


printArchive :: ArArchive -> IO ()
printArchive = putStr . archiveText

archiveText :: ArArchive -> String
archiveText = renderStyle (Style PageMode 80 1.5) . ppArchive

ppArchive :: ArArchive -> Doc
ppArchive a = 
        text              (ar_magic a)
    $+$ (vcat $ map ppArchiveObject $ ar_objects a)



ppArchiveObject :: ArchiveObject -> Doc
ppArchiveObject = ppArHeader . ar_header

ppArHeader :: ArHeader -> Doc
ppArHeader a = 
    tableProlog "Header" (24,6) (applyfs fields a) 
  where
    ppf    = ppField 4 24   
    fields = 
       [ ppf 16 "name"                  (text . arh_name)
       , ppf 12 "date"                  (text . arh_date)
       , ppf 6  "user id"               (int  . arh_user_id)
       , ppf 6  "group id"              (int  . arh_group_id)
       , ppf 8  "mode"                  (text . arh_mode)
       , ppf 10 "size"                  (int . arh_size)
       , ppf 2  "trailer"               (tup2 . arh_trailer)
       ]
    tup2 (x:y:xs) = ppHex 2 (ord x) <+> ppHex 2 (ord y) <+> text xs
    tup2 xs       = text xs




--------------------------------------------------------------------------------
-- Helpers

tableProlog :: String -> (Int,Int) -> [Doc] -> Doc
tableProlog s (m,n) ds =
        columnSep 
    $+$ text s 
    $+$ columnSep
    $+$ columnHeadings m n
    $+$ columnSep
    $+$ vcat ds
  where
    columnHeadings fsz vsz = 
      text "size" <+> text (pad fsz ' ' "field") <+> text (pad vsz ' ' "value")


columnSep :: Doc
columnSep = text $ replicate 60 '-' 


ppField :: Int -> Int -> Int -> String -> (a -> Doc) -> a -> Doc
ppField n1 n2 sz field_name f a = text sz'  <+> text field_name' <+> f a
  where
    sz'         = pad n1 ' ' (show sz)
    field_name' = pad n2 ' ' field_name

ppHex :: Integral a => Int -> a -> Doc
ppHex n i = text "0x" <> (text $ pad n '0' $ showHex i "")
  

pad :: Int -> Char -> String -> String
pad n ch s | length s < n = replicate (n - length s) ch ++ s
           | otherwise    = s

listDoc :: [Doc] -> Doc
listDoc = brackets . hcat . punctuate comma

