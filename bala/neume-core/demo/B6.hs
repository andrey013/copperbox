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
import Neume.Core.Syntax


import Text.PrettyPrint.Leijen

import Data.Ratio
import System.Cmd

type StdGlyph = Glyph () Pitch Duration



main :: IO ()
main = 
     writeFile "bulgarian6_abc.abc" (renderEighty abc_all)
  >> system   "abcm2ps bulgarian6_abc.abc -O bulgarian6_abc.ps"
  >> writeFile "bulgarian6.ly"      (renderEighty ly_all)
  >> system   "lilypond bulgarian6.ly"
  >> return ()


renderEighty :: Doc -> String
renderEighty = ($ "") . displayS . renderPretty 0.8 80

putDocEighty :: Doc -> IO ()
putDocEighty = putStr . renderEighty

ly_all :: Doc 
ly_all = lyDoc $ fn ly_img
  where
    fn    = encloseSep empty term sepa . getPhraseData
    term  = string " |"
    sepa  = string " |"

lyDoc :: Doc -> Doc
lyDoc body = prolog <$> body <$> epilog
  where
    prolog = vsep $ map text
               [ "\\version \"2.12.2\""
               , "\\score {"
               , "  \\relative c' {"
               , "    \\key a \\major"
               , "    \\time 2/4"
               , "    \\repeat volta 2 {"
               ]
    epilog = vsep $ map text [ "    }", "  }", "}"]


abc_all :: Doc
abc_all = abc_prolog <$> fn abc_img
  where
    fn    = encloseSep empty term sepa . getPhraseData
    term  = string " :|"
    sepa  = string " |"
   

abc_prolog :: Doc
abc_prolog = vsep $ map text $
    [ "X:1", "T:Bulgarian 6 (ABC)", "M:2/4", "K:Amaj" ] 

ly_img :: PhraseImage
ly_img  = runRender (renderGlyph pitch (\_ a -> a)) ly_score

ly_score :: Full (Glyph () Pitch (Maybe Duration))
ly_score = runRelDurTrafo $ fst $ runRelPitchTrafo middle_c b6_score

abc_img :: PhraseImage
abc_img = ABC.runRender ABC.renderGlyph abc_score 

abc_score :: Full (Glyph () Pitch AbcMultiplier)
abc_score = 
    ABC.runDurMultTrafo (1%16) $ ABC.runPitchSpellTrafo a_major b6_score

b6_score :: Full StdGlyph
b6_score = 
    makeFull (bracketConfig two_four_time) $ NoteList $ map Elem bars1_4

two_four_time :: MeterPattern
two_four_time = makeMeterPattern 2 4


a_major     :: AbcSpellingMap
a_major     = makeAbcSpellingMap 3



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


