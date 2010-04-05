

module Overlay1 where

import qualified Neume.Core.AbcOutput        as ABC
import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.NoteList
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import qualified Neume.Extra.AbcDoc          as ABC
import qualified Neume.Extra.AbcFormat       as ABC
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondFormat
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd


main :: IO ()
main = do 
  writeDoc "overlay1.ly"      ly_score
  writeDoc "overlay1_abc.abc" abc_score
  system   "lilypond overlay1.ly"
  system   "abcm2ps overlay1_abc.abc -O overlay1_abc.ps" 
  return ()



ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key c_nat "major" 
                        <$> (time 4 4)
                        <$> tune1)
  where
    tune1   = renderLyRelative_overlay2 wn ofmt rwspec rwspec ov_score
    
    ofmt    = Ly_Std_Format_Config barNumber
    rwspec  = Ly_Relative_Rewrite_Config middle_c four_four_time


abc_score :: Doc
abc_score =  ABC.tunenum        1 
         <$> ABC.title          "Overlays"
         <$> ABC.meter          "4/4"
         <$> ABC.key            "Cmaj"
         <$> ABC.unitDuration   en
         <$> tune1
  where
    tune1   = ABC.renderABC_overlay2 ofmt rwspec rwspec ov_score
    
    ofmt    = ABC.ABC_Std_Format_Config  [4,4,4,4] ABC.barNumber
    rwspec  = ABC.ABC_Std_Rewrite_Config c_major (1%8) four_four_time



ov_score :: Score (NoteList StdGlyph, NoteList StdGlyph)
ov_score = map (fmap (both simpleNoteList)) $ 
    [ Repeated (("aU", ubars1'4), ("aL", lbars1'4)) ]


c_major   :: SpellingMap
c_major   = makeSpellingMap 0


ubars1'4 :: [StdGlyph]
ubars1'4 =  
  [ a 4 () en, c 5 () en, c 5 () en, a 4 () en
  , c 5 () qn, a 4 () qn
  
  -- bar 2
  , c 5 () en, a 4 () en, c 5 () en, a 4 () en
  , c 5 () qn, a 4 () qn
  
  -- bar 3
  , a 4 () en, c 5 () en, c 5 () en, a 4 () en
  , c 5 () qn, a 4 () qn
  
  -- bar 4
  , a 4 () en, c 5 () en, c 5 () en, a 4 () en
  , a 4 () hn

  ]

lbars1'4 :: [StdGlyph]
lbars1'4 =  
  [ c 4 () hn, c 4 () hn
  
  -- bar 2
  , c 4 () hn, c 4 () hn
  
  -- bar 3
  , c 4 () hn, c 4 () hn

  
  -- bar 4
  , c 4 () qn, c 4 () qn, c 4 () hn

  ]



