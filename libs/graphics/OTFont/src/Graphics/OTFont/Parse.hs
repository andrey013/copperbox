{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Parse
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Read an otf file 
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Parse where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParserExtras

import Control.Applicative
import qualified Data.Map as Map

  

protoFace :: ParserM r ProtoFace
protoFace = do 
    ot@(OffsetTable _ i _ _ _)  <- offsetTable
    dirs                        <- count (fromIntegral i) tableDirectory
    return $ ProtoFace ot dirs (mkTableRegions dirs) 
    
     
offsetTable :: ParserM r OffsetTable 
offsetTable = (\p a b c d -> OffsetTable (checkPrefix p) a b c d)
    <$> (count 4 char) <*> word16be <*> word16be <*> word16be <*> word16be
  where
    otto, o1oo :: String 
    otto = "OTTO"
    o1oo = ['\0','\1','\0','\0']
    
    checkPrefix s | s == otto || s == o1oo  = s
                  | otherwise               = error $ "offsetTable bad prefix "  
                                                          ++ s
tableDirectory :: ParserM r TableDirectory 
tableDirectory = TableDirectory 
    <$> text 4 <*> word32be <*> word32be <*> word32be

mkTableRegions :: [TableDirectory] -> TableRegions
mkTableRegions ts = foldr fn Map.empty ts
  where
    fn (TableDirectory name _ o l) fm = 
        Map.insert name (fromIntegral o, fromIntegral l) fm
   
    
 
        

