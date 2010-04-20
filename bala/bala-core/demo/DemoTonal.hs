
module DemoTonal where

import Bala.Core.Tonal
import Bala.Core.TonalNamed

import Data.AdditiveGroup
import Data.AffineSpace


-- To think about - non palindrome scales 
-- (melodic minor, Raga Behag, ...)

type Scale = Pitch -> [Pitch]

makeScale :: [Interval] -> Scale
makeScale = flip $ scanl (.+^)

majorScale :: Scale
majorScale = makeScale [ major2, major2, minor2, major2, major2, major2, minor2 ]


melodicMinor :: Scale
melodicMinor = makeScale (asc ++ desc)
  where
    asc  = [ major2, minor2, major2, major2, major2, major2, minor2 ]
    desc = map negateV [ major2, major2, minor2, major2, major2, minor2, major2 ]

cmaj = majorScale c5

cmminor = melodicMinor c5
