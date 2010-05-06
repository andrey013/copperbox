{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

module ABC_B6 where

import qualified Neume.Core.AbcFormat       as ABC
import Neume.Core.SpellingMap
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils.Common
import Neume.Core.Utils.Pretty

import qualified Neume.Extra.AbcDoc          as ABC
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd

main :: IO ()
main = 
  writeDoc "bulgarian6_abc.abc" abc_score                       >>
  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps"    >>
  return ()


abc_score :: Doc
abc_score =  ABC.tunenum   1 
         <$> ABC.title     "Bulgarian 6 (ABC)"
         <$> ABC.meter     "2/4"
         <$> ABC.key       "Amaj"
         <$> tune1
  where
    tune1   = ABC.renderABC ofmt rwspec b6_score
    
    ofmt    = ABC.ABC_std_format_config  [4,4,4,4] ABC.barNumber
    rwspec  = ABC.ABC_std_rewrite_config a_major (1%16) two_four_time

 
b6_score :: Score (TRepeat :. TRepeat :. Z) (NoteList StdGlyph)
b6_score = fmap simpleNoteList $ 
    Repeat ("a",bars1'4) $ Repeat ("b",bars5'8) $ Nil


a_major     :: AbcSpellingMap
a_major     = makeAbcSpellingMap 3




bars1'4 :: [StdGlyph]
bars1'4 =  
  [ a_  4 `rap` sn, b_ 4 `rap` sn, cs_ 5 `rap` sn, cs_ 5 `rap` sn
  , cs_ 5 `rap` sn, a_ 4 `rap` sn, cs_ 5 `rap` sn, cs_ 5 `rap` sn
  
  -- bar 2
  , cs_ 5 `rap` sn, a_ 4 `rap` sn, b_ 4 `rap` sn, cs_ 5 `rap` sn 
  , b_  4 `rap` sn, a_ 4 `rap` sn, a_ 4 `rap` sn, snr
  
  -- bar 3
  , e_  5 `rap` sn, d_ 5 `rap` sn, cs_ 5 `rap` sn, b_  4 `rap` sn
  , cs_ 5 `rap` sn, a_ 4 `rap` sn, b_  4 `rap` sn, cs_ 5 `rap` sn

  -- bar 4
  , a_ 4 `rap` sn, b_ 4 `rap` sn, b_ 4 `rap` sn, a_ 4 `rap` sn
  , a_ 4 `rap` en, enr
  ]


bars5'8 :: [StdGlyph]
bars5'8 = 
  [ cs_ 5 `rap` en, b_ 4 `rap` sn, a_  4 `rap` sn
  , b_  4 `rap` en, a_ 4 `rap` sn, gs_ 4 `rap` sn

  -- bar 6
  , fs_ 4 `rap` sn, e_ 4 `rap` sn, fs_ 4 `rap` sn, gs_ 4 `rap` sn
  , a_  4 `rap` en, b_ 4 `rap` en

  -- bar 7
  , cs_ 5 `rap` en, b_ 4 `rap` sn, a_  4 `rap` sn
  , b_  4 `rap` en, a_ 4 `rap` sn, gs_ 4 `rap` sn

  -- bar 8
  , fs_ 4 `rap` sn, e_ 4 `rap` sn, fs_ 4 `rap` en
  , fs_ 4 `rap` en, enr
 
  ]




