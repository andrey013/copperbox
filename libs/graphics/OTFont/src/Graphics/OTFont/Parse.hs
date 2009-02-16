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
import Graphics.OTFont.ParseMonad

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Data.Array.IO
import Data.Array.Unboxed ( bounds )
import qualified Data.Map as Map
import Data.Word
import System.IO 


type ParseError  = String
type Parser r a = ParserT (ErrorT ParseError Identity) r a

runParser :: Parser r r -> ByteSequence -> (Either ParseError r)
runParser m arr = runIdentity $ runErrorT $
    runCST m (\c _st _env -> return c) st arr where
        st = let (i,j) = bounds arr in position i j


runParserFile :: FilePath -> Parser r r -> IO (Either ParseError r)
runParserFile path p = withBinaryFile path ReadMode $ \h -> do
    sz_i  <- hFileSize h
    let abound = mkBounds sz_i
    marr   <- newArray_ abound 
    sz_r  <- hGetArray h marr (fromIntegral sz_i)
    arr   <- freezeByteSequence marr
    if sz_r == (fromIntegral sz_i) 
        then return $ runParser p arr
        else error $ "Problem with hGetArray" 

  where
    mkBounds :: Integral a => a -> (Int,Int)
    mkBounds i = (0, fromIntegral $ i - 1)
    
    freezeByteSequence :: IOUArray Int Word8 -> IO ByteSequence
    freezeByteSequence = freeze

--------------------------------------------------------------------------------    

protoFace :: Parser r ProtoFace
protoFace = do 
    ot@(OffsetTable _ i _ _ _)  <- offsetTable
    dirs                        <- count (fromIntegral i) tableDirectory
    return $ ProtoFace ot dirs (mkTableRegions dirs) 
    
     
offsetTable :: Parser r OffsetTable 
offsetTable = (\p a b c d -> OffsetTable (checkPrefix p) a b c d)
    <$> (count 4 char) <*> word16be <*> word16be <*> word16be <*> word16be
  where
    otto, o1oo :: String 
    otto = "OTTO"
    o1oo = ['\0','\1','\0','\0']
    
    checkPrefix s | s == otto || s == o1oo  = s
                  | otherwise               = error $ "offsetTable bad prefix "  
                                                          ++ s
tableDirectory :: Parser r TableDirectory 
tableDirectory = TableDirectory 
    <$> text 4 <*> word32be <*> word32be <*> word32be

mkTableRegions :: [TableDirectory] -> TableRegions
mkTableRegions ts = foldr fn Map.empty ts
  where
    fn (TableDirectory name _ o l) fm = 
        Map.insert name (fromIntegral o, fromIntegral l) fm
   
    
 
        

