

module DemoMeter where

import Bala.Metrical


hijaz_mp :: MeterPattern 
hijaz_mp = [2,2,3]


-----

one_d_d :: Alg Int
one_d_d = one +++ dim +++ dim

demo1 = runAlg id hijaz_mp one_d_d

demo2 = runAlg id hijaz_mp (aug 3)

demo3 :: [Double]
demo3 = runAlg fromIntegral hijaz_mp (aug 3)

demo4 = runAlg id hijaz_mp (one +++ one +++ div2 (\_ -> (1,2)))


