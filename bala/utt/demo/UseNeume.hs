{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}


module UseNeume where

import UTT.Base hiding ( relative )
import UTT.Neume

import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.Pitch
import Neume.Core.SpellingMap
import Neume.Core.SyntaxGlyph
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxScore
import Neume.Core.Utils
import Neume.Extra.LilyPondDoc
import Neume.Extra.NamedElements


import Text.PrettyPrint.Leijen

import System.Cmd


main :: IO ()
main = 
  writeDoc "triads.ly"      ly_score        >>
  system   "lilypond triads.ly"             >>
  return ()

-- Note using absolute score - Neume has something wrong with 
-- chord rendering in relative mode...
--

ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> variableDef "melody" (nestBraces $ key c_nat "major" 
                                            <$> time 4 4
                                            <$> tune1)
        <$> scoreExpr (variableUse "melody")

  where
    tune1    = renderLyAbsolute ofmt rwspec triad_score
    
    ofmt     = Ly_std_format_config       strip
    rwspec   = Ly_absolute_rewrite_config (-3) four_four_time strip


triad_score :: Score (TLinear :. Z) (NoteList StdGlyph)
triad_score = fmap simpleNoteList $ 
    Linear ("a", triads_list) $ Nil
 
triads_list :: [StdGlyph]
triads_list = map chord3 $ upwards $ render1 key_c_maj progression


key_c_maj :: SpellingMap
key_c_maj = makeSpellingMap 0 


render1 :: SpellingMap -> [Triad] -> [Pitch3]
render1 sm = map (pitches sm)


progression :: [Triad]
progression = take 4 $ iterate (`act` mediant) (Triad 0 Pos)

chord3 :: Pitch3 -> Glyph () Pitch Duration
chord3 (P3 root trd fth) = chord [note root, note trd, note fth] dWhole False
  where
    note pch        = Note () pch

