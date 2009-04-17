
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import Mullein.AbcConvert
import qualified Mullein.AbcSyntax as Abc
import qualified Mullein.AbcOutput as Abc
import Mullein.ScoreSyntax
import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.LabelSet
import Mullein.NamedElements ( sixteenth, a_major, c4' )
import Mullein.ScoreNames
import Mullein.Utils

import qualified Mullein.LilyPondConvert as Ly
import qualified Mullein.LilyPondOutput as Ly




import Text.PrettyPrint.Leijen 

     

main = putDoc $ Abc.outputPart bulgarian6

ly = putDoc $ Ly.outputPart bulg6 where
  bulg6 = Ly.convertToLy Ly.relPitch c4' part1_8


bulgarian6 :: Part Abc.Element
bulgarian6 = convertToAbc lset sixteenth part1_8 where
  lset = maybe (error "lset missing") id $ makeLabelSet a_major

part1_8 :: Part Element 
part1_8 = Part $ [Repeated m1_4, Repeated m5_8]
  
m1_4 :: Motif Element
m1_4 = bracket twoFourTime (primary bars1_4)

m5_8 :: Motif Element
m5_8 = bracket twoFourTime (primary bars5_8)

twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4


bars1_4 :: NoteList
bars1_4 = notelist $ 
           [ a4 & du16, b4, cis5, cis5, cis5, a4, cis5, cis5
           , cis5, a4, b4, cis5, b4, a4, a4, rest        
           , e5, d5, cis5, b4, cis5, a4, b4, cis5
           , a4, b4, b4, a4 , a4 & du8, rest    
           ]

bars5_8 = notelist $ 
           [ c5 & du8, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4, gis4, a4 & du8, b4
           , cis5, b4 & du16, a4, b4 & du8, a4 & du16, gis4
           , fis4, e4, fis4 & du8, rest
           ]




    