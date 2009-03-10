{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ZBitmap.ReadBmp
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Bmp file parser.
--
--------------------------------------------------------------------------------


module Graphics.ZBitmap.ReadBmp (
  readBmp,
  readBmpHeader,
  readBmpHeaderAndPalette,
  runParser,
  evalParser,
  
) where


import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils ( cstyle2Darray, sectionSizes)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.Array
import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( chr )
import Data.Word

import System.IO

type ParseLog = String

data ParseState = PSt
      {  remaining :: BS.ByteString
      ,  pos       :: Int 
      }

newtype ParseErr = ParseErr String
  deriving (Show) 

instance Error ParseErr where
  noMsg = ParseErr ""
  strMsg s = ParseErr s   
                          
newtype Parser a = Parser { 
    _get_parser :: ErrorT ParseErr (WriterT ParseLog (State ParseState)) a }
  deriving ( Functor, Monad, MonadError ParseErr, MonadState ParseState,
             MonadWriter ParseLog ) 

instance Applicative Parser where
  pure = return
  (<*>) = ap
  
runParser :: Parser a -> BS.ByteString  -> (Either String a, ParseLog)   
runParser parser bs0 = case runP parser bs0 of
    (Left (ParseErr err),msg) -> (Left err,msg)
    (Right a            ,msg) -> (Right a,msg)  
  where 
    runP :: Parser a -> BS.ByteString  -> (Either ParseErr a, ParseLog)
    -- should we use runState and check input is exhausted? 
    runP (Parser p) bs = evalState (runWriterT $ runErrorT $ p) (initState bs)
    
    initState bs = PSt { remaining = bs, pos = 0 }


evalParser :: Parser a -> BS.ByteString  -> Either String a
evalParser parser = fst . runParser parser 


readFromFile :: FilePath -> Parser a -> String -> IO a  
readFromFile name p err_msg = do 
    h <- openBinaryFile name ReadMode
    bs <- BS.hGetContents h
    let (ans,msg) = runParser p bs    
    case ans of 
      Left err -> hClose h >> putStrLn err >> putStrLn msg 
                           >> error err_msg
      Right mf -> hClose h >> return mf
    
readBmp :: FilePath -> IO BmpBitmap  
readBmp name = readFromFile name bmpFile "readBmp failed"

readBmpHeader :: FilePath -> IO BmpHeader
readBmpHeader name = readFromFile name header "readBmpHeader failed"

readBmpHeaderAndPalette :: FilePath -> IO (BmpHeader,Maybe Palette)
readBmpHeaderAndPalette name = 
    readFromFile name headerPlusPalette "readBmpHeaderAndPalette failed"
      
      
--------------------------------------------------------------------------------

 
    
bmpFile :: Parser BmpBitmap  
bmpFile = do
    hdr                   <- header
    let (_,pali,pxli)     = sectionSizes hdr
    let height            = (fromIntegral . bmp_height . dib_header) hdr
    let cmprz             = (compression_type . dib_header) hdr
    o_p                   <- optionalPalette pali
    body                  <- imageData cmprz pxli height
    return $ BmpBitmap hdr o_p body
  where
    -- i is the palette's on disk size - divide by 4 for number of colours
    optionalPalette 0 = pure Nothing
    optionalPalette i = Just <$> palette (i `div` 4)

headerPlusPalette :: Parser (BmpHeader,Maybe Palette)  
headerPlusPalette = do
    hdr                   <- header
    let (_,pali,_)        = sectionSizes hdr
    o_p                   <- optionalPalette pali
    return $ (hdr, o_p)
  where
    -- i is the palette's on disk size - divide by 4 for number of colours
    optionalPalette 0 = pure Nothing
    optionalPalette i = Just <$> palette (i `div` 4)


header :: Parser BmpHeader
header = BmpHeader
    <$> bmpmagic <*> word32le <*> reservedData <*> word32le <*> dibheader
  where
    bmpmagic :: Parser MagicNumber
    bmpmagic = magicNumber <$> char <*> char 

    reservedData :: Parser ReservedData
    reservedData = (,) <$> word16le <*> word16le

    

      
      
dibheader :: Parser BmpDibHeader
dibheader = BmpDibHeader
      <$> readHeaderSize    <*> word32le        <*> word32le 
      <*> readColourPlanes  <*> bitsPerPx       <*> compression
      <*> word32le          <*> word32le        <*> word32le
      <*> word32le          <*> readImpColours
  where
    readHeaderSize        = headerSize        <$> word32le
    readColourPlanes      = colourPlanes      <$> word16le
    readImpColours        = importantColours  <$> word32le 
    

palette :: Int -> Parser Palette
palette n = buildPalette <$> count n paletteColour

paletteColour :: Parser RgbColour
paletteColour = (\b g r _ -> (r,g,b)) <$>
    word8 <*> word8 <*> word8 <*> word8

buildPalette :: [RgbColour] -> Palette                                 
buildPalette cs = Palette sz $ listArray (0,sz-1) cs where
    sz                    = length cs
    
    
-- /sz/ should always be a multiple of /height/
imageData :: BmpCompression -> Int -> Int -> Parser (Maybe PixelData)
imageData Bi_RGB sz height 
    | sz `mod` height == 0  = let width = sz `div` height in do
                                  ws <- count sz word8
                                  return $ Just $ 
                                      cstyle2Darray width height ws
    | otherwise             = reportFail $ "Image dimensions incorrect - "
          ++ "image height (" ++ show height ++ ") must be a multiple of "
          ++ show sz

imageData _     _   _       = reportFail $ "cannot decode compressed bmp files" 


    
bitsPerPx :: Parser BmpBitsPerPixel
bitsPerPx = unmarshalBmpBitsPerPixel <$> word16le
    
compression :: Parser BmpCompression
compression = unmarshalBmpCompression <$> word32le

--------------------------------------------------------------------------------
-- Helpers


reportFail :: String -> Parser a
reportFail s = do 
    posn <- gets pos
    throwError $ strMsg $ posStr posn ++ s
  where
    posStr p = "readError - position " ++ show p ++ " "
    
    
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
   
word8 :: Parser Word8
word8 = do
    bs <- gets remaining
    case BS.uncons bs of
        Nothing    -> reportFail "Unexpected eof" 
        Just (a,b) -> do { i <- gets pos 
                         ; modify (\s -> s { remaining=b, pos=i+1 } )
                         ; return a } 
                         
char :: Parser Char
char = chr . fromIntegral <$> word8


word16le   :: Parser Word16
word16le   = w16le <$> word8 <*> word8

word32le   :: Parser Word32
word32le   = w32le <$> word8 <*> word8 <*> word8 <*> word8


w16le :: Word8 -> Word8 -> Word16
w16le a b = a' + b' where
    a' = fromIntegral a
    b' = b `iShiftL` 8
             
w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = a' + b' + c' + d' where
    a' = fromIntegral a
    b' = b `iShiftL` 8
    c' = c `iShiftL` 16
    d' = d `iShiftL` 24
               

iShiftL :: (Bits b, Integral b) => Word8 -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x

          