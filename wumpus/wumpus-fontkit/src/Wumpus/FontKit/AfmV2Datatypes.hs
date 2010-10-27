{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.FontKit.AfmV2Datatypes
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM concrete syntax for Version 2.0.
-- 
--------------------------------------------------------------------------------

module Wumpus.FontKit.AfmV2Datatypes
  ( 
    AfmNumber
  , AfmName
  , AfmKey
  , FontBBox
  , CharBBox
  , WidthVector
  , AfmFile(..)
  , GlobalInfo
  , CharacterMetrics(..)

  ) where


import Wumpus.Core

import Data.Map

-- Note - for robustness there is some merit in building a simple
-- map between field names and strings representing unparsed 
-- answers.
--
-- This can handle permutations and extra fields defined by 
-- /users/.
--


type AfmNumber      = Double
type AfmKey         = String
type AfmName        = String
type CharBBox       = BoundingBox AfmNumber
type FontBBox       = BoundingBox Int
type WidthVector    = Vec2 AfmNumber

data AfmFile = AfmFile
      { version_number          :: String
      , global_info             :: GlobalInfo
      , char_metrics_count      :: Int
      , char_metrics            :: [CharacterMetrics]
      }
  deriving (Eq,Show)




type GlobalInfo = Map AfmKey String


data CharacterMetrics = CharacterMetrics
      { char_code               :: Int
      , width_vector            :: WidthVector
      , char_name               :: AfmName
      , char_bbox               :: CharBBox
      }
  deriving (Eq,Show)


