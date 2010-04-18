{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Extra.Tab
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Glyphs annotated with /string number/ for printing tabs.
-- 
--------------------------------------------------------------------------------

module Neume.Extra.Tab
  (



  -- * Tab string number
  -- $tab_string_anno
    TabGlyph
  , TabGlyph'
  , StringNumber
  , stringNumber

  , ppStringNumber

  ) where

import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.SyntaxGlyph

import Text.PrettyPrint.Leijen



--------------------------------------------------------------------------------
-- Tab Glyphs with string number


-- $tab_string_anno
-- Note form is \<pitch\>\<duration\>/\/\<finger_number\> 
-- e.g. @c4\\2@
--
-- Chord form is /</\<pitch\>/\/\<finger_number> .../>/\<duration\> 
-- e.g. @\<c\\1 e\\2 g\\4>2@

-- Notes are annotated with string number

newtype StringNumber = StringNumber Int
  deriving (Eq,Ord)

stringNumber :: Int -> StringNumber
stringNumber = StringNumber . abs

instance Show StringNumber where
  showsPrec p (StringNumber i) = showsPrec p i


ppStringNumber :: StringNumber -> Doc
ppStringNumber (StringNumber i) = char '\\' <> int i

 

-- NOTE - these types/instances are due for removal.

type TabGlyph  = Glyph StringNumber Pitch Duration
type TabGlyph' = Glyph StringNumber Pitch (Maybe Duration)

