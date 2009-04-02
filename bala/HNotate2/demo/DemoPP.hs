

module DemoPP where

import qualified HNotate.AbcForm as Abc

import qualified HNotate.DocAbc as DocAbc
import HNotate.Data
import HNotate.Duration
import HNotate.Metrical
import HNotate.NamedElements
import HNotate.NoteList
import HNotate.Pitch
import HNotate.Staff
import HNotate.Utils

import qualified Data.Foldable as F
import Data.Ratio 
import Text.PrettyPrint.Leijen -- ( pretty )


test1 = Abc.outputAbc $ Abc.abcStaff (labelSetOf' a_major) sixteenth
    $ lineTreeToStaffRep 0 (repeat $ 2%4) (repeat [2%8,2%8]) bars1_4
test2 = pretty $ lineTreeToStaffRep 0 (repeat $ 4%4) (repeat [2%8,2%8,2%8,2%8]) example7


test3 = print . map fn . map (onset 0 (repeat $ 2%4)) $ collapseTree bars1_4
  where
    fn (i,xs) = int i <$> F.foldl' (\a e -> a <$> pretty e) empty xs

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

example7 :: NoteList
example7 =  
  root # poly [ root # note c4 du4 # note c4 du4 # note c4 du4 # note c4 du4
              , root # note d5 du4 # note d6 du4 # poly [ root # note e5 du4 ]
              ]
                       
{-
d1 = tune (xField 1 <$> composerField "Unknown" 
                    <$> meterField 2 4 <$> printEnv) <$>
     tune (xField 2 <$> meterField 4 4 <$> printEnv)
-}

