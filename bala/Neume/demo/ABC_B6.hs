{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

module ABC_B6 where

import qualified Neume.Core.AbcOutput        as ABC
import Neume.Core.Bracket
import Neume.Core.Datatypes
import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.SyntaxScore
import Neume.Core.SyntaxStaff
import Neume.Core.Utils.Common
import Neume.Core.Utils.Pretty

import qualified Neume.Extra.AbcDoc          as ABC
import qualified Neume.Extra.AbcFormat       as ABC
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
    tune1 = ABC.abcScore abcPhrase strip [4,4,4,4] b6_score
    


b6_score :: (Score repr, [StdGlyph] ~ ScoreBase repr) => () -> repr
b6_score () = repeated bars1'4 `caten` repeated bars5'8
      

a_major     :: SpellingMap
a_major     = makeSpellingMap 3



abcPhrase :: [StdGlyph] -> PhraseImage
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


