{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- TODO check parallelMusic in LilyPond...


module Demo1 where

import Neume.Bracket
import Neume.Datatypes
import Neume.Doc
import Neume.Duration
import Neume.LilyPondDoc
import Neume.LilyPondOutput
import Neume.OneList
import Neume.Pitch
import Neume.SyntaxDoc
import Neume.SyntaxMarkup
import Neume.SyntaxStaff

{-
import M2.Bracket
import M2.Duration
import M2.LilyPondDoc
import M2.LilyPondOutput
import M2.Pitch
import M2.Syntax

import Data.Ratio

instance NumMeasured Int where
  type Measurement Int = DurationMeasure
  nmeasure i = (fromIntegral i)%1

test01 :: ([OneMany Int], [Int])
test01 = beamSegment [4,4] [2,2,4,4]

test02 :: ([OneMany Int], [Int])
test02 = beamSegment [] [2,2,4,4]

test03 :: ([OneMany Int], [Int])
test03 = beamSegment [4,4,4] [2,2,4,4]

test04 :: ([OneMany Int], [Int])
test04 = beamSegment [4,4,4,4] [2,2,4,4,2,2]
-}




