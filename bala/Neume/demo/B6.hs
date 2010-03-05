

module B6 where

import qualified Neume.Core.AbcDoc           as ABC
import qualified Neume.Core.AbcOutput        as ABC
import Neume.Core.Bracket
import Neume.Core.Datatypes
import Neume.Core.Doc
import Neume.Core.Duration
import Neume.Core.LilyPondDoc
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxStaff
import Neume.Core.Utils

import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd

main :: IO ()
main = do 
  writeDoc "bulgarian6.ly"      ly_score
  writeDoc "bulgarian6_abc.abc" abc_score
  system   "lilypond bulgarian6.ly"
  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps" 
  return ()


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key a_nat "major" 
        <$> (time 2 4)
        <$> tune)
  where
    tune =  simpleOutput $ renderPhrase pitch      
                         $ rewritePitchRel    middle_c 
                         $ rewriteDurationOpt xs

    xs   = phrase two_four_time' $ simpleNoteList bars1'4

abc_score :: Doc
abc_score =  ABC.tunenum   1 
         <$> ABC.title     "Bulgarian 6"
         <$> ABC.meter     "2/4"
         <$> ABC.key       "Amaj"
         <$> (tune1 <> tune2) 
  where
    tune1 = renderToABC bars1'4
    tune2 = renderToABC bars5'8
      
renderToABC :: [StdGlyph] -> Doc
renderToABC  = ABC.simpleOutput . ABC.renderPhrase 
                                . ABC.rewritePitch amaj
                                . ABC.rewriteDuration (1%16)
                                . phrase two_four_time'
                                . simpleNoteList
  where
    amaj :: SpellingMap
    amaj = makeSpellingMap 3


two_four_time' :: MeterPattern
two_four_time' = makeMeterPattern 2 4



bars1'4 :: [StdGlyph]
bars1'4 =  
  [ a 4 () sn,  b 4 () sn, cs 5 () sn, cs 5 () sn
  , cs 5 () sn, a 4 () sn, cs 5 () sn, cs 5 () sn
  
  -- bar 2
  , cs 5 () sn, a 4 () sn, b 4 () sn, cs 5 () sn 
  , b 4 () sn, a 4 () sn,  a 4 () sn, snr
  
  -- bar 3
  , e 5 () sn,  d 5 () sn, cs 5 () sn, b 4 () sn
  , cs 5 () sn, a 4 () sn, b 4 () sn, cs 5 () sn

  -- bar 4
  , a 4 () sn, b 4 () sn, b 4 () sn, a 4 () sn
  , a 4 () en, enr
  ]

bars5'8 :: [StdGlyph]
bars5'8 = 
  [ cs 5 () en, b 4 () sn, a 4 () sn
  , b 4  () en, a 4 () sn, gs 4 () sn

  -- bar 6
  ]


