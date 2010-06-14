{-# OPTIONS -Wall #-}

module B6 where

import qualified Neume.Core.AbcOutput       as ABC
import qualified Neume.Core.AbcTrafo        as ABC
import Neume.Core.Bracket
import Neume.Core.Duration
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondPretty ( pitch )
import Neume.Core.LilyPondTrafo
import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.SpellingMap
import Neume.Core.ModularSyntax


import Text.PrettyPrint.Leijen

import Data.Ratio
-- import System.Cmd

type StdGlyph = Glyph () Pitch Duration

{-

main :: IO ()
main = 
  writeDoc "bulgarian6.ly"      ly_score                        >>
  writeDoc "bulgarian6_abc.abc" abc_score                       >>
  system   "lilypond bulgarian6.ly"                             >>
  system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps"    >>
  return ()


ly_score :: Doc
ly_score =  version "2.12.2" 
        <$> scoreExpr (relative middle_c $ key a_nat "major" 
                       <$> time 2 4
                       <$> tune1)
  where
    tune1    = renderLyRelative ofmt rwspec b6_score
    
    ofmt     = Ly_std_format_config       strip
    rwspec   = Ly_relative_rewrite_config middle_c two_four_time strip

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
    Repeat ("a", bars1'4) $ Repeat ("b", bars5'8) $ Nil
-}

ly_img :: PhraseImage
ly_img  = runRender (renderGlyph pitch (\_ a -> a)) ly_score

ly_score :: Full (Glyph () Pitch (Maybe Duration))
ly_score = runRelDurTrafo $ fst $ runRelPitchTrafo middle_c b6_score

abc_img :: PhraseImage
abc_img = ABC.runRender ABC.renderGlyph abc_score 

abc_score :: Full (Glyph () Pitch AbcMultiplier)
abc_score = ABC.runDurMultTrafo (1%16) $ ABC.runPitchSpellTrafo a_major b6_score

b6_score :: Full StdGlyph
b6_score = Full $ phrase two_four_time $ NoteList "b6" $ map S $ bars1_4 ++ bars5_8


two_four_time :: MeterPattern
two_four_time = makeMeterPattern 2 4


a_major     :: AbcSpellingMap
a_major     = makeAbcSpellingMap 3


bars1_4 :: [StdGlyph]
bars1_4 =  
  [ sn (A,4,nat),   sn (B,4,nat), sn (C,5,sharp), sn (C,5,sharp)
  , sn (C,5,sharp), sn (A,4,nat), sn (C,5,sharp), sn (C,5,sharp)
  
  -- bar 2
  , sn (C,5,sharp), sn (A,4,nat), sn (B,4,nat),   sn (C,5,sharp) 
  , sn (B,4,nat),   sn (A,4,nat), sn (A,4,nat),   sn_rest
  
  -- bar 3
  , sn (E,5,nat),   sn (D,5,nat), sn (C,5,sharp), sn (B,4,nat)
  , sn (C,5,sharp), sn (A,4,nat), sn (B,4,nat),   sn (C,5,sharp)

  -- bar 4
  , sn (A,4,nat),   sn (B,4,nat), sn (B,4,nat),   sn (A,4,nat)
  , en (A,4,nat),   en_rest
  ]


bars5_8 :: [StdGlyph]
bars5_8 = 
  [ en (C,5,sharp), sn (B,4,nat), sn (A,4,nat)
  , en (B,4,nat),   sn (A,4,nat), sn (G,4,sharp)

  -- bar 6
  , sn (F,4,sharp), sn (E,4,nat), sn (F,4,sharp), sn (G,4,sharp)
  , en (A,4,nat),   en (B,4,nat)

  -- bar 7
  , en (C,5,sharp), sn (B,4,nat), sn (A,4,nat)
  , en (B,4,nat),   sn (A,4,nat), sn (G,4,sharp)

  -- bar 8
  , sn (F,4,sharp), sn (E,4,nat), en (F,4,sharp)
  , en (F,4,sharp), en_rest
 
  ]

nat, sharp :: Bool
nat = False
sharp = True

mkNote :: Duration -> (PitchLetter,Octave,Bool) -> StdGlyph
mkNote d (l,o,True)  = GlyNote (Note () (Pitch l (Just Sharp) o)) d NoTie
mkNote d (l,o,False) = GlyNote (Note () (Pitch l Nothing      o)) d NoTie

sn :: (PitchLetter,Octave,Bool) -> StdGlyph
sn = mkNote dSixteenth

en :: (PitchLetter,Octave,Bool) -> StdGlyph
en = mkNote dEighth
 

en_rest :: StdGlyph
en_rest = Rest dEighth

sn_rest :: StdGlyph
sn_rest = Rest dSixteenth

