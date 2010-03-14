

module B6 where

import qualified Neume.Core.AbcOutput        as ABC
import Neume.Core.Bracket
import Neume.Core.Datatypes
import Neume.Core.Duration
import Neume.Core.LilyPondBasic
import Neume.Core.LilyPondOutput
import Neume.Core.Pitch
import Neume.Core.SyntaxDoc
import Neume.Core.SyntaxStaff
import Neume.Core.Utils.Common
import Neume.Core.Utils.Pretty

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
  writeDoc "bulgarian6.ly"      ly_score
  writeDoc "bulgarian6_abc.abc" abc_score
  system   "lilypond bulgarian6.ly"
  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps" 
  return ()


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key a_nat "major" 
                       <$> (time 2 4)
                       <$> tune1)
  where
    tune1 = lilypondScore strip $ makeBulgarian6 lyPhrase


-- Note - this is not correct (yet) - relative pitch transformation
-- is in the wrong place...
--
lyPhrase :: [StdGlyph] -> Phrase LY
lyPhrase = renderPhrase pitch . rewritePitchRel middle_c
                              . rewriteDurationOpt
                              . phrase two_four_time
                              . simpleNoteList


abc_score :: Doc
abc_score =  ABC.tunenum   1 
         <$> ABC.title     "Bulgarian 6"
         <$> ABC.meter     "2/4"
         <$> ABC.key       "Amaj"
         <$> tune1
  where
    tune1 = ABC.abcScore ABC.barNumber [4,4,4,4] $ makeBulgarian6 abcPhrase 


makeBulgarian6 :: ([StdGlyph] -> Phrase a) -> Score a
makeBulgarian6 fn = mkScore [ repeated $ fn bars1'4 
                            , repeated $ fn bars5'8
                            ]


abcPhrase :: [StdGlyph] -> Phrase ABC
abcPhrase = ABC.renderPhrase . ABC.rewritePitch a_major
                             . ABC.rewriteDuration (1%16)
                             . phrase two_four_time
                             . simpleNoteList
 

a_major     :: SpellingMap
a_major     = makeSpellingMap 3

mkScore :: [Section a] -> Score a
mkScore = Score

repeated :: Phrase a -> Section a
repeated = Repeated


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
  , fs 4 () sn, e 4 () sn, fs 4 () sn, gs 4 () sn
  , a 4 () en, b 4 () en

  -- bar 7
  , cs 5 () en, b 4 () sn, a 4 () sn
  , b 4 () en, a 4 () sn, gs 4 () sn

  -- bar 8
  , fs 4 () sn, e 4 () sn, fs 4 () en
  , fs 4 () en, enr
 
  ]



