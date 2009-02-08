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
import Graphics.OTFont.Utils

import Text.ZParse

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Word

laxFont :: Monad m => BinaryParserT m LaxFont
laxFont = do 
    inp                         <- input      -- cache the original bytestring
    ot@(OffsetTable _ i _ _ _)  <- offsetTable
    dirs                        <- count (fromIntegral i) tableDirectory
    let m = extractTableStreams dirs inp
    return (LaxFont ot dirs m) 
    
     
offsetTable :: Monad m => BinaryParserT m OffsetTable 
offsetTable = OffsetTable 
    <$> (o1oo <|> otto) <*> word16be <*> word16be <*> word16be <*> word16be
  where
    otto :: Monad m => BinaryParserT m String 
    otto = chars "OTTO"
    
    o1oo :: Monad m => BinaryParserT m String 
    o1oo = chars ['\0','\1','\0','\0']
    
    
tableDirectory :: Monad m => BinaryParserT m TableDirectory 
tableDirectory = TableDirectory 
    <$> text 4 <*> word32be <*> word32be <*> word32be

extractTableStreams :: [TableDirectory] -> BS.ByteString -> TableStreams
extractTableStreams ts inp = foldr fn Map.empty ts
  where
    fn (TableDirectory name _ o l) fm = Map.insert name (section o l) fm
   
    section i j 
        | i + j <= len = BS.take (fromIntegral j) $ BS.drop (fromIntegral i) inp 
        | otherwise    = error $ "extractTableStreams - section error"
    
    len                = fromIntegral $ BS.length inp  
        

--------------------------------------------------------------------------------
-- NameTable

nameTable :: Monad m => BinaryParserT m NameTable 
nameTable  = do 
    nf  <- word16be
    nc  <- word16be
    so  <- word16be
    nr  <- count (fromIntegral nc) nameRecord
    sd  <- flush
    return $ NameTable nf nc so nr sd

nameRecord :: Monad m => BinaryParserT m NameRecord 
nameRecord = NameRecord <$>
   word16be <*> word16be <*> word16be <*> word16be <*> word16be <*> word16be 



--------------------------------------------------------------------------------
-- 

ushort :: Monad m => BinaryParserT m Word16
ushort = word16be
 


