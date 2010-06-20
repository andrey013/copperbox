{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}

module Bulgarian6 where

import qualified Neume.Core.AbcOutput       as ABC
import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondPretty
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondTrafo
import Neume.Core.Pitch
import Neume.Core.Utils.Pretty ( writeDoc )
import Neume.Core.SpellingMap
import Neume.Core.Syntax

import qualified Neume.Extra.AbcDoc          as ABC
import Neume.Extra.Common
import Neume.Extra.LilyPondDoc
import Neume.Extra.LilyPondScoreOutput
import Neume.Extra.NamedElements
import Neume.Extra.ScoreSyntax

import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd



main :: IO ()
main = 
  writeDoc "bulgarian6.ly"      lilypond_full                   >>
--  writeDoc "bulgarian6_abc.abc" abc_score                       >>
  system   "lilypond bulgarian6.ly"                             >>
--  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps"    >>
  return ()

{-
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
-}

lilypond_full :: Doc
lilypond_full =  version "2.12.2"
             <$> variableDef "melody" melody_body
             <$> scoreExpr (variableUse "melody")
  where 
    melody_body = relative middle_c $ vsep [ key a_nat "major" 
                                           , time 2 4
                                           , score_doc_ly ]

-- NOTE - relative-duration-trafo is shape sensitive, although
-- it would be nice, there isn't really an opportunity to fuse 
-- it with the relative-pitch-trafo.
--

score_doc_ly :: Doc
score_doc_ly = inlineScore barNumber 1 b6_score_ly

b6_score_ly :: Score (TRepeat :. TRepeat :. Z) PhraseImage
b6_score_ly = 
    fmap render_step $ fmap runRelDurTrafo 
                     $ fst $ stmap runRelPitchTrafo middle_c b6_score
  where 
    render_step = runRender (renderGlyph pitch strip)


b6_score :: Score (TRepeat :. TRepeat :. Z) 
                  (Full (Glyph () Pitch Duration))
b6_score = fmap (makeFull (bracketConfig [1%4,1%4]) . simpleNoteList) $ 
    Repeat bars1'4 $ Repeat bars5'8 $ Nil


simpleNoteList :: [e] -> SimpleNoteList e
simpleNoteList = NoteList . map Item



a_major     :: AbcSpellingMap
a_major     = makeAbcSpellingMap 3

rap :: a -> (a -> b) -> b
rap a f = f a

bars1'4 :: [Glyph () Pitch Duration]
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


bars5'8 :: [Glyph () Pitch Duration]
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



