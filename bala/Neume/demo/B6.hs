

module B6 where

import qualified Neume.AbcDoc           as ABC
import qualified Neume.AbcOutput        as ABC
import Neume.Bracket
import Neume.Datatypes
import Neume.Doc
import Neume.Duration
import Neume.LilyPondDoc
import Neume.LilyPondOutput
import Neume.NamedElements
import Neume.Pitch
import Neume.SyntaxStaff
import Neume.Utils

import Text.PrettyPrint.Leijen

import Data.Ratio

main :: IO ()
main = do 
  writeDoc "bulgarian6.ly"      ly_score
  writeDoc "bulgarian6_abc.abc" abc_score


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key a_nat "major" 
        <$> (time $ timeSignature two_four_time)
        <$> tune)
  where
    tune =  simpleOutput $ renderPhrase       
                         $ rewritePitchRel    middle_c 
                         $ rewriteDurationOpt xs

    xs   = phrase twoFourTime $ simpleNoteList bars1'4

abc_score :: Doc
abc_score =  ABC.tunenum   1 
         <$> ABC.title     "Bulgarian 6"
         <$> ABC.meter     "2/4"
         <$> ABC.key       "Amaj"
         <$> tune
  where
    tune = ABC.simpleOutput $ ABC.renderPhrase     
                            $ ABC.rewritePitch     amaj 
                            $ ABC.rewriteDuration  (1%16) xs 

    xs   = phrase twoFourTime $ simpleNoteList bars1'4
    amaj = makeSpellingMap 3



twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4



bars1'4 :: [Glyph () Pitch Duration]
bars1'4 =  
  [ a 4 () sn, b 4 () sn, cs 5 () sn, cs 5 () sn, cs 5 () sn, a 4 () sn, 
               cs 5 () sn, cs 5 () sn
  -- bar 2
  , cs 5 () sn, a 4 () sn, b 4 () sn, cs 5 () sn, b 4 () sn, a 4 () sn, 
                a 4 () sn, snr
  -- bar 3
  , e 5 () sn, d 5 () sn, cs 5 () sn, b 4 () sn, cs 5 () sn, a 4 () sn, 
               b 4 () sn, cs 5 () sn
  -- bar 4
  , a 4 () sn, b 4 () sn, b 4 () sn, a 4 () sn, a 4 () en, enr
  ]



