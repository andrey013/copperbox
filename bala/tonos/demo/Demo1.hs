{-# OPTIONS -Wall #-}

-- > :set -i../src:../../Neume/src

module Demo1 where

import Tonos.Base
import Tonos.Neume
import Tonos.Pitch
-- import Tonos.Utils

import qualified Neume.Core.Pitch as Neume

demo1 :: Neume.Pitch
demo1 = neumePitch $ middle_c


demo2, demo3, demo4 :: Int
demo2 = distance C D   -- 2
demo3 = distance C B   -- 1
demo4 = distance B C   -- 1

demo5 :: Bool
demo5 = distance F G == distance G F