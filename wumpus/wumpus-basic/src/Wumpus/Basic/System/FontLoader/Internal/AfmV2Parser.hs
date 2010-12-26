{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.System.FontLoader.Internal.AfmV2Parser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser for Version 2.0.
--
-- Note - AFM Version 2.0 used by GhostScript and Version 3.0+
-- have numerous differences. 
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.System.FontLoader.Internal.AfmV2Parser
  ( 

    parseAfmV2File
  , afmV2Parser
  
  ) where

import Wumpus.Basic.System.FontLoader.Internal.AfmParserBase
import Wumpus.Basic.System.FontLoader.Internal.Base
import Wumpus.Basic.Utils.ParserCombinators

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative




--------------------------------------------------------------------------------
-- parser

parseAfmV2File :: FilePath -> IO (Either FontLoadErr AfmFile)
parseAfmV2File filepath = runParserEither p <$> readFile filepath
  where
    p = afmFileParser charMetricsV2

afmV2Parser :: CharParser AfmFile
afmV2Parser = afmFileParser charMetricsV2


charMetricsV2 :: CharParser AfmGlyphMetrics
charMetricsV2 = AfmGlyphMetrics <$>
        metric "C" (-1) cint
    <*> widthVector
    <*> metric "N" "" name1
    <*  charBBox
    <*  many (symbol "L" *> ligature_body <* semi)
    <*  newlineOrEOF
  where
    ligature_body = ((,) <$> name <*> name)
    
widthVector :: CharParser (Vec2 AfmUnit)
widthVector =  (symbol "WX" *> ((\w -> vec w 0) <$> number) <* semi)
           <|> (symbol "W"  *> (vec <$> number <*> number)  <* semi)


--------------------------------------------------------------------------------



