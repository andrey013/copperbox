

module ABC_B6 where

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
import Neume.Extra.LilyPondDoc               hiding  ( score )
import Neume.Extra.LilyPondFormat
import Neume.Extra.NamedElements

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd

main :: IO ()
main = do 
  writeDoc "bulgarian6_abc.abc" abc_score
  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps" 
  return ()


abc_score :: Doc
abc_score =  ABC.tunenum   1 
         <$> ABC.title     "Bulgarian 6"
         <$> ABC.meter     "2/4"
         <$> ABC.key       "Amaj"
         <$> tune1
  where
    tune1 = ABC.abcScore strip [4,4,4,4] bulgarian_6
    



bulgarian_6 :: Score ABC
bulgarian_6 = score [ repeated $ abcPhrase bars1'4 
                    , repeated $ abcPhrase bars5'8]
      
renderToABC :: [StdGlyph] -> Doc
renderToABC  = ABC.simpleOutput . ABC.renderPhrase 
                                . ABC.rewritePitch a_major
                                . ABC.rewriteDuration (1%16)
                                . phrase two_four_time
                                . simpleNoteList

a_major     :: SpellingMap
a_major     = makeSpellingMap 3

score :: [Section ABC] -> Score ABC 
score = Score

repeated :: Phrase ABC -> Section ABC
repeated = Repeated


abcPhrase :: [StdGlyph] -> Phrase ABC
abcPhrase = ABC.renderPhrase . ABC.rewritePitch a_major
                             . ABC.rewriteDuration (1%16)
                             . phrase two_four_time
                             . simpleNoteList
 



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


