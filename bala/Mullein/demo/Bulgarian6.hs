
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import Mullein.Gen.AbcConvert
import qualified Mullein.Gen.AbcSyntax as Abc
import qualified Mullein.Gen.AbcOutput as Abc
import Mullein.Gen.Syntax
import Mullein.Bracket
import Mullein.Core
import Mullein.CoreTypes
import Mullein.NamedElements ( sixteenth, a_major, c4' )
import Mullein.ScoreNames
import Mullein.Utils

import qualified Mullein.Gen.LilyPondConvert as Ly
import qualified Mullein.Gen.LilyPondOutput as Ly




import Text.PrettyPrint.Leijen 

main = putDoc $ Abc.outputAbc (repeat 4) bulgarian6

ly = putDoc $ Ly.outputLy bulg6 where
  bulg6 = Ly.convertToLy Ly.relPitch c4' s1_4


bulgarian6 :: Abc.Motif
bulgarian6 = convertToAbc lset sixteenth s1_4 where
  lset =  maybe (error "missing LabelSet") id  $ labelSetOf a_major
  
  
s1_4 = bracket twoFourTime bars1_4

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


{-
bars1_4 :: NoteList
bars1_4 = 
    root # note a4 du16   # note b4 du16 # note cis5 du16 # note cis5 du16 
         # note cis5 du16 # note a4 du16 # note cis5 du16 # note cis5 du16
        
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
         # note b4 du16   # note a4 du16 # note a4 du16   # rest du16
        
         # note e5 du16   # note d5 du16 # note cis5 du16 # note b4 du16
         # note cis5 du16 # note a4 du16 # note b4 du16   # note cis5 du16
        
         # note a4 du16   # note b4 du16 # note b4 du16   # note a4 du16
         # note a4 du8    # rest du8       

        
-} 


    