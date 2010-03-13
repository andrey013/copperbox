

module Overlay1 where

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
  writeDoc "overlay1.ly"      ly_score
--  writeDoc "overlay1_abc.abc" abc_score
  system   "lilypond overlay1.ly"
  system   "abcm2ps overlay1_abc.abc -O overlay1_abc.ps" 
  return ()


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key c_nat "major" 
        <$> (time 2 4)
        <$> tune)
  where
    tune =  simpleOutput $ renderPhrase pitch      
                         $ rewritePitchRel    middle_c 
                         $ rewriteDurationOpt xs

    xs   = phrase four_four_time $ simpleNoteList ubars1'4

renderToLy :: [StdGlyph] -> Doc
renderToLy = simpleOutput . renderPhrase pitch 
                          . rewritePitchRel middle_c
                          . rewriteDurationOpt
                          . phrase four_four_time
                          . simpleNoteList

{-
abc_score :: Doc
abc_score =  ABC.tunenum        1 
         <$> ABC.title          "Overlays"
         <$> ABC.meter          "4/4"
         <$> ABC.key            "Cmaj"
         <$> ABC.unitDuration   en
         <$> ABC.tempOutput tune1_abc 
  where
    tune1 = renderToABC ubars1'4
-}
      
renderToABC :: [StdGlyph] -> Doc
renderToABC  = 
  ABC.simpleOutput . makeSimplePhraseAbc c_major (1%8) four_four_time
   


tune1_abc :: [OverlayBar]
tune1_abc = ABC.overlayPhrases [upper_abc,lower_abc]
  where
    mk        = makeSimplePhraseAbc c_major (1%8) four_four_time
    upper_abc = mk ubars1'4
    lower_abc = mk lbars1'4


c_major :: SpellingMap
c_major = makeSpellingMap 0

makeSimplePhraseAbc :: SpellingMap 
                    -> DurationMeasure 
                    -> MeterPattern 
                    -> [StdGlyph]
                    -> Phrase ABC
makeSimplePhraseAbc spelling unit_drn meter_pattern = 
   ABC.renderPhrase . ABC.rewritePitch    spelling
                    . ABC.rewriteDuration unit_drn
                    . phrase              meter_pattern
                    . simpleNoteList


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



