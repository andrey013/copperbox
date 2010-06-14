{-# OPTIONS -Wall #-}


module Temp01 where

import Neume.Core.Bracket
import Neume.Core.ModularSyntax
import Neume.Core.LilyPondOutput
import Neume.Core.LilyPondTrafo
import qualified Neume.Core.AbcOutput as ABC
import qualified Neume.Core.AbcTrafo  as ABC

import Text.PrettyPrint.Leijen hiding ( (<$>) )



d01 :: MetricalDiv Char
d01 = Beamed [ Atom 'C', Atom 'D', Atom 'E' ]

d02 :: MetricalDiv Int
d02 = fmap (const 10) d01

full01 :: Full Char
full01 = Full $ Phrase [[d01]]

demo1 :: PhraseImage
demo1 = runRender char full01


demo2 :: PhraseImage 
demo2 = ABC.runRender char full01
