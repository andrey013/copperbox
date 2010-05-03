{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS -Wall #-}


module UseNeume where

import UTT.Base hiding ( relative )
import UTT.Neume
import UTT.TCBase

import Neume.Core.Duration
import Neume.Core.LilyPondFormat
import Neume.Core.Pitch
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


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key c_nat "major" 
                       <$> time 4 4
                       <$> tune1)
  where
    tune1    = renderLyRelative ofmt rwspec triad_score
    
    ofmt     = Ly_std_format_config       strip
    rwspec   = Ly_relative_rewrite_config middle_c four_four_time strip


triad_score :: Score (TLinear :. Z) (NoteList StdGlyph)
triad_score = fmap simpleNoteList $ 
    Linear ("a", triads_list) $ Nil
 
triads_list :: [StdGlyph]
triads_list = []


triad :: (Pitch,Pitch,Pitch) -> Glyph () Pitch Duration
triad (root,trd,fth) = chord [note root, note trd, note fth] dWhole False
  where
    note pch = Note () pch

