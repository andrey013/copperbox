

module B6 where

import qualified Mullein.AbcOutput as ABC
import Mullein.Bracket
import Mullein.Core
import Mullein.Duration
import Mullein.LilyPondOutput
import Mullein.NamedElements
import Mullein.Pitch
import Mullein.Utils

import Text.PrettyPrint.Leijen



demo1 = vsep $ map oBarOverlay $ runRewriteDuration xs
  where
    xs = bracket twoFourTime $ map (pitchMap changeOctave) bars1'4

demo2 = vsep $ map ABC.oBarOverlay xs where
  
  xs = bracket twoFourTime $ map (spell amaj) bars1'4
  amaj = spellingMap 3
  
  -- ...

twoFourTime :: MeterPattern
twoFourTime = meterPattern 2 4


changeOctave :: Pitch -> Pitch
changeOctave (Pitch l a o) = Pitch l a (o-4)


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



