

module Demo1 where

import Data.Stream
import Data.Stream.StreamCalculus

import Prelude hiding ( take, const, zipWith )

equivUpto n s z = and $ take n $ zipWith (==) s z


d01 = take 10 $ blam (const 4)
d01'1 = take 10 $ blam (cX)