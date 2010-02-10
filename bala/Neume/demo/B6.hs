

module B6 where

import qualified Neume.AbcOutput as ABC
import Neume.Bracket
import Neume.Datatypes
import Neume.Duration
import Neume.LilyPondOutput
import Neume.NamedElements
import Neume.Pitch
import Neume.Utils

import Text.PrettyPrint.Leijen

import Data.Ratio


demo1 = simpleOutput $ renderPhrase lyGlyph
                     $ rewritePitch middle_c 
                     $ rewriteDuration xs
  where
    xs = phrase twoFourTime $ bars1'4

demo2 = ABC.simpleOutput $ ABC.renderPhrase ABC.abcGlyph
                         $ ABC.rewritePitch amaj 
                         $ ABC.rewriteDuration (1%16) xs 
  where
    xs   = phrase twoFourTime bars1'4
    amaj = makeSpellingMap 3



twoFourTime :: MeterPattern
twoFourTime = makeMeterPattern 2 4



bars1'4 :: [PDGlyph]
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



