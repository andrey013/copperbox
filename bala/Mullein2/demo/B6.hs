

module B6 where

import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondOutput
import Mullein.NamedElements
import Mullein.Utils

import Text.PrettyPrint.Leijen

b1'4 = bracket (snd twoFourTime) bars1'4

demo1 = vsep $ map oBarOverlay b1'4

twoFourTime :: MetricalSpec
twoFourTime = metricalSpec 2 4




bars1'4 :: [Element]
bars1'4 =  
  [ a 4 sn [], b 4 sn [], cs 5 sn [], cs 5 sn [], cs 5 sn [], a 4 sn [], 
               cs 5 sn [], cs 5 sn []
  -- bar 2
  , cs 5 sn [], a 4 sn [], b 4 sn [], cs 5 sn [], b 4 sn [], a 4 sn [], 
                a 4 sn [], snr
  -- bar 3
  , e 5 sn [], d 5 sn [], cs 5 sn [], b 4 sn [], cs 5 sn [], a 4 sn [], 
               b 4 sn [], cs 5 sn []
  -- bar 4
  , a 4 sn [], b 4 sn [], b 4 sn [], a 4 sn [], a 4 en [], enr
  ]



