{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.FontLoader.AfmV4Dot1Parser
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- AFM file parser for Version 4.1.
--
-- Adobe distributes font metrics for the /Core 14/ fonts as
-- AFM Version 4.1 files.  
--
--------------------------------------------------------------------------------

module Wumpus.Basic.FontLoader.AfmV4Dot1Parser
  ( 
    
    parseAfmV4Dot1File
  
  ) where

import Wumpus.Basic.FontLoader.AfmParserBase
import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.Utils.ParserCombinators

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

import qualified Data.Map as Map


parseAfmV4Dot1File :: FilePath -> IO (Either ParseError AfmFile)
parseAfmV4Dot1File filepath = runParserEither afmFile <$> readFile filepath



afmFile :: CharParser AfmFile
afmFile = 
    (\info xs -> AfmFile (getEncodingScheme info)
                         (getFontBBox info)
                         (getCapHeight info)
                         xs ) 
      <$> (versionNumber    *> globalInfo) 
      <*> (startCharMetrics *> many charMetrics)



globalInfo :: CharParser GlobalInfo
globalInfo = (foldr (\(k,v) a -> Map.insert k v a) Map.empty) 
               <$> manyTill (keyStringPair <* lexeme newline) (peek startCharMetrics)



charMetrics :: CharParser AfmGlyphMetrics
charMetrics = AfmGlyphMetrics <$>
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

