{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBitmap.ReadBmp
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


module ZBitmap.ReadBmp (
  readBmp
  
) where

import ZBitmap.Datatypes
import ZBitmap.Utils ( paddingMeasure, paletteSize, bmpRowWidth )

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.Bits
import qualified Data.ByteString as BS
import Data.Char ( chr )
import Data.Word

import System.IO

type ParseLog = String

data ParseState = PSt { remaining :: BS.ByteString,
                        pos       :: Int }

newtype ParseErr = ParseErr String
  deriving (Show) 

instance Error ParseErr where
  noMsg = ParseErr ""
  strMsg s = ParseErr s   
                          
newtype Parser a = Parser { 
    getParser :: ErrorT ParseErr (WriterT ParseLog (State ParseState)) a }
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
    runP p bs = evalState (runWriterT $ runErrorT $ getParser p) (initState bs)
    
    initState bs = PSt { remaining = bs, pos = 0 }


evalParser :: Parser a -> BS.ByteString  -> Either String a
evalParser parser = fst . runParser parser 

    
readBmp :: FilePath -> IO BMPfile  
readBmp name = do 
    h <- openBinaryFile name ReadMode
    bs <- BS.hGetContents h
    let (ans,msg) = runParser bmpFile bs    
    case ans of 
      Left err -> hClose h >> putStrLn err >> putStrLn msg 
                           >> error "readBmp failed"
      Right mf -> hClose h >> return mf
      
--------------------------------------------------------------------------------


reportFail :: String -> Parser a
reportFail s = do 
    posn <- gets pos
    throwError $ strMsg $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   
    
bmpFile :: Parser BMPfile  
bmpFile = do
    hdr     <- header
    dib     <- dibheader
    o_ps    <- palette $ _bits_per_pixel dib
    body    <- bmpBody dib
    return $ BMPfile hdr dib o_ps body

header :: Parser BMPheader
header = (\sz off -> BMPheader sz off)
  <$> (ignore "magic1" char *> ignore "magic2" char *> getWord32le)          
  <*> (ignore "reserved1" getWord16le *> 
       ignore "reserved2" getWord16le *> getWord32le) 


dibheader :: Parser V3Dibheader
dibheader = V3Dibheader 
    <$> getWord32le  <*> getWord32le  <*> getWord32le 
    <*> getWord16le  <*> bitsPerPixel <*> compression
    <*> getWord32le  <*> getWord32le  <*> getWord32le
    <*> getWord32le  <*> getWord32le

palette :: BmpBitsPerPixel -> Parser (Maybe PaletteSpec)
palette bpp = case paletteSize bpp of
    0 -> return Nothing
    n -> do bs <- getByteString (4 * n)   -- 4 bytes per colour
            return $ Just bs


bmpBody :: V3Dibheader -> Parser DibImageData
bmpBody dib 
    | isBi_RGB24 dib = getByteString bs_size  
    | otherwise      = return BS.empty
  where 
    isBi_RGB24 d = _compression d == Bi_RGB  
    bs_size      = h * getByteCount (bmpRowWidth w bpp)
    (w,h,bpp)    = (_bmp_width dib, _bmp_height dib, _bits_per_pixel dib)
    
bitsPerPixel :: Parser BmpBitsPerPixel
bitsPerPixel = unmarshalBmpBitsPerPixel <$> getWord16le
    
compression :: Parser BmpCompression
compression = unmarshalBmpCompression <$> getWord32le

{-

imageData24 :: Word32 -> Word32 -> Parser (Array (Word32,Word32) RGBcolour)
imageData24 rows cols = do 
    xss <- iter rows (fst <$> dataLine cols)
    return $ build (concat $ reverse xss)
  where
    build xs = listArray arange xs
    arange = ((0,0), (rows-1,cols-1)) 
-}    
    
    
dataLine :: Word32 -> Parser ([RGBcolour],[Word8])
dataLine w = (,) <$> payload w <*> padding w
  where
    payload i = iter i rgbColour
    padding i = iter (paddingMeasure i) getWord8

          

rgbColour :: Parser RGBcolour
rgbColour = RGBcolour <$> getWord8 <*> getWord8 <*> getWord8



--------------------------------------------------------------------------------
-- Helpers


ignore :: Show a => String -> Parser a -> Parser ()
ignore name p = p >>= logfield name . show
  

logfield :: Show a => String -> a -> Parser ()
logfield name val = tell $ name  ++ " = " ++ show val ++ "\n"


getByteString :: Integral i => i -> Parser BS.ByteString
getByteString count = do 
    bs <- gets remaining
    let rlen = BS.length bs
    if i > rlen 
      then getFailure rlen
      else let (front,rest) = BS.splitAt i bs in do 
                p <- gets pos 
                modify (\s -> s { remaining=rest, pos=p+i } )
                return front
  where
    i = fromIntegral count
    
    getFailure rlen = do 
      p <- gets pos
      reportFail $ "getByteString failed at " ++ show p ++ " trying to get "
                   ++ show i ++ " bytes, with only " ++ show rlen 
                   ++ " remaining.\n" 
      

iter :: Integral i => i -> Parser a -> Parser [a]
iter i p | i > 0     = (:) <$> p <*> iter (i-1) p
         | otherwise = return []
         
         
eof :: Parser Bool
eof = do
     bs <- gets remaining
     return $ BS.null bs 
   
getWord8 :: Parser Word8
getWord8 = do
    bs <- gets remaining
    case BS.uncons bs of
        Nothing    -> reportFail "Unexpected eof" 
        Just (a,b) -> do { i <- gets pos 
                         ; modify (\s -> s { remaining=b, pos=i+1 } )
                         ; return a } 
                         
char :: Parser Char
char = chr . fromIntegral <$> getWord8

         
assertChar :: Char -> Parser Char
assertChar ch = char >>= fn
  where fn a | a == ch    = return ch
             | otherwise  = reportFail $ "assertChar failed"

getWord16le   :: Parser Word16
getWord16le   = w16le <$> getWord8 <*> getWord8

getWord32le   :: Parser Word32
getWord32le   = w32le <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8


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
               

iShiftL :: (Bits b, Integral a, Integral b) => a -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x

          