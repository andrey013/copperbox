

module DemoPP where

import HNotate.DocAbc
import HNotate.DocBase


d1 = tune (xField 1 <$> meterField 2 4 <$> printEnv) <$>
     tune (xField 2 <$> meterField 4 4 <$> printEnv)


