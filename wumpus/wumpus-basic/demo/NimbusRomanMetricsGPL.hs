{-# OPTIONS -Wall #-}

-- LICENSE: GPL
-- This file is derived from the metrics to the NimbusRomNo9L-Regu
-- font in the GhostScript distribution.


-- Note Space has no bounding box...



module NimbusRomanMetricsGPL where

import Wumpus.Basic.Text.Datatypes

import Wumpus.Core                              -- package: wumpus-core

import qualified Data.IntMap as IntMap

nimbus_metrics :: CharMetricsTable AfmUnit
nimbus_metrics = 
    CharMetricsTable { glyph_max_height = nimbus_max_height
                     , default_adv_vec  = wx 722
                     , char_adv_vecs    = IntMap.fromList nimbus_char_map }


nimbus_max_height :: AfmUnit
nimbus_max_height = 942 + negate (-281)

nimbus_bbox :: CharBoundingBox AfmUnit
nimbus_bbox = charBoundingBox (-168) (-281) 1031 924

bb :: AfmUnit -> AfmUnit -> AfmUnit -> AfmUnit -> CharBoundingBox AfmUnit
bb = charBoundingBox

wx :: AfmUnit -> Vec2 AfmUnit
wx = hvec


nimbus_char_map :: [(CodePoint, Vec2 AfmUnit)]
nimbus_char_map  = 
  [ (  32, wx 250 )             -- space - note no BBox!
  , (  65, wx 722 )             -- A
  , (  66, wx 667 )             -- B
  , (  67, wx 667 )             -- C
  , (  68, wx 722 )             -- D
  , (  69, wx 611 )             -- E
  , (  70, wx 556 )             -- F
  , (  71, wx 722 )             -- G
  , (  72, wx 722 )             -- H
  , (  73, wx 333 )             -- I
  , (  74, wx 389 )             -- J
  , (  75, wx 722 )             -- K
  , (  76, wx 611 )             -- L
  , (  77, wx 889 )             -- M
  , (  78, wx 722 )             -- N
  , (  79, wx 722 )             -- O
  , (  80, wx 556 )             -- P
  , (  81, wx 722 )             -- Q
  , (  82, wx 667 )             -- R
  , (  83, wx 556 )             -- S
  , (  84, wx 611 )             -- T
  , (  85, wx 722 )             -- U
  , (  86, wx 722 )             -- V
  , (  87, wx 944 )             -- W
  , (  88, wx 722 )             -- X
  , (  89, wx 722 )             -- Y
  , (  90, wx 611 )             -- Z

  , (  97, wx 444 )             -- a
  , (  98, wx 500 )             -- b
  , (  99, wx 444 )             -- c
  , ( 100, wx 500 )             -- d
  , ( 101, wx 444 )             -- e
  , ( 102, wx 333 )             -- f
  , ( 103, wx 500 )             -- g
  , ( 104, wx 500 )             -- h
  , ( 105, wx 278 )             -- i
  , ( 106, wx 278 )             -- j
  , ( 107, wx 500 )             -- k
  , ( 108, wx 278 )             -- l
  , ( 109, wx 778 )             -- m
  , ( 110, wx 500 )             -- n
  , ( 111, wx 500 )             -- o
  , ( 112, wx 500 )             -- p
  , ( 113, wx 500 )             -- q
  , ( 114, wx 333 )             -- r
  , ( 115, wx 389 )             -- s
  , ( 116, wx 278 )             -- t
  , ( 117, wx 500 )             -- u
  , ( 118, wx 500 )             -- v
  , ( 119, wx 722 )             -- w
  , ( 120, wx 500 )             -- x
  , ( 121, wx 500 )             -- y
  , ( 122, wx 444 )             -- z 
  ]