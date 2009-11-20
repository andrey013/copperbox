{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.TextEncoding
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Extended character handling...
-- 
--------------------------------------------------------------------------------

module Wumpus.Core.TextEncoding
  ( 

    EncodedText(..)    
  , TextPart(..)

  , textLength

  , iso_8859_1
  ) where


newtype EncodedText = EncodedText { getEncodedText :: [TextPart] }
  deriving (Eq,Show)

data TextPart = LeftToRightText String
              | SingleGlyphText String
  deriving (Eq,Show)


textLength :: EncodedText -> Int
textLength = foldr add 0 . getEncodedText where 
    add (LeftToRightText ss) n = n + length ss
    add _                    n = n + 1


-- | Output to PostScript as @ /egrave glyphshow @

-- Output to SVG as an escaped decimal, e.g. @ &#232; @
--
-- Note, HTML entity names do not seem to be supported in SVG,
-- @ &egrave; @ does not work in FireFox or Chrome.



iso_8859_1 :: String
iso_8859_1 = "ISO-8859-1"


