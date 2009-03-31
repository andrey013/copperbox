

module DemoPP where


import HNotate.DocAbc
import HNotate.DocBase
import HNotate.Duration
import HNotate.Fits
import HNotate.Metrical
import HNotate.NoteList
import HNotate.Pitch
import HNotate.Utils



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

         

d1 = tune (xField 1 <$> composerField "Unknown" 
                    <$> meterField 2 4 <$> printEnv) <$>
     tune (xField 2 <$> meterField 4 4 <$> printEnv)


