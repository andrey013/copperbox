
-- This tune is `Bulgarian (?) 6` from the Exotic ABC songbook

-- ghci ...
-- :set -i../src

module Bulgarian6 where

import Mullein.Gen.AbcConvert
import qualified Mullein.Gen.AbcSyntax as Abc
import qualified Mullein.Gen.AbcOutput as Abc
import Mullein.Gen.Syntax
import Mullein.Core
import Mullein.CoreTypes
import Mullein.NamedElements
import Mullein.Section
import Mullein.Utils

import qualified Mullein.Gen.LilyPondConvert as Ly
import qualified Mullein.Gen.LilyPondOutput as Ly




import Text.PrettyPrint.Leijen 

main = putDoc $ Abc.outputAbc (repeat 4) bulgarian6

ly = putDoc $ Ly.outputLy b6 where
  b6 = Ly.convertToLy s1_4


bulgarian6 :: Abc.Section
bulgarian6 = convertToAbc lset sixteenth s1_4 where
  lset =  maybe (error "missing LabelSet") id  $ labelSetOf a_major
  
  
s1_4 = section twoFourTime bars1_4

twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4


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

        
 


    