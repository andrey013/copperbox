{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZBmp.ReadBmp
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


module ZBmp1.ReadBmp where

import ZBmp1.Datatypes
import ZBmp1.Utils ( byteWidth )

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.Array.IArray ( Array, listArray )
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char ( chr )
import Data.List ( transpose )
import Data.Word

import System.IO

type ParseLog = String

data ParseState = PSt { remaining :: B.ByteString,
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
  
runParser :: Parser a -> B.ByteString  -> (Either String a, ParseLog)   
runParser parser bs0 = case runP parser bs0 of
    (Left (ParseErr err),msg) -> (Left err,msg)
    (Right a            ,msg) -> (Right a,msg)  
  where 
    runP :: Parser a -> B.ByteString  -> (Either ParseErr a, ParseLog)
    -- should we use runState and check input is exhausted? 
    runP p bs = evalState (runWriterT $ runErrorT $ getParser p) (initState bs)
    
    initState bs = PSt { remaining = bs, pos = 0 }


evalParser :: Parser a -> B.ByteString  -> Either String a
evalParser parser = fst . runParser parser 

    
readBmp :: FilePath -> IO BMPfile  
readBmp name = do 
    h <- openBinaryFile name ReadMode
    bs <- B.hGetContents h
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
    pals    <- paletteSpec (_bits_per_pxl dib)
    body    <- bmpBody dib
    return $ BMPfile hdr dib pals body

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

bmpBody :: V3Dibheader -> Parser BMPbody
bmpBody dib 
    | isMono dib  = Mono <$> imageDataMono (_dib_width dib) (_dib_height dib) 
    | otherwise   = return UnrecognizedFormat
  where 
    isMono d = _bits_per_pxl d == B1_Monochrome && _compression d == Bi_RGB  

bitsPerPixel :: Parser BitsPerPixel
bitsPerPixel = unmarshalBitsPerPixel <$> getWord16le
    
compression :: Parser Compression
compression = unmarshalCompression <$> getWord32le

paletteSpec :: BitsPerPixel -> Parser PaletteSpec
paletteSpec B1_Monochrome     = Palette 2   <$> paletteData 2
paletteSpec B4_Colour16       = Palette 16  <$> paletteData 16
paletteSpec B8_Colour256      = Palette 256 <$> paletteData 256
paletteSpec _                 = return NoPalette   


paletteData :: Word32 -> Parser PaletteData
paletteData num_colours = do 
    xs <- iter size getWord8
    return $ build xs
  where
    build xs = listArray (0,fromIntegral $ size-1) xs
    size  = 4 * num_colours 


imageDataMono :: Word32 -> Word32 -> Parser ImageData
imageDataMono w h = do
    xs <- iter size getWord8
    return $ build xs
  where
    build xs = listArray (0,fromIntegral $ size-1) xs
    size  = h * byteWidth w 
         


{-
imageData24 :: Word32 -> Word32 -> Parser (Array (Word32,Word32) RGBcolour)
imageData24 w h = do 
    xss <- iter h (fst <$> dataLine w)
    return $ build (concat $ transpose xss)
  where
    build xs = listArray arange xs
    arange = ((0,0), (w-1,h-1)) 
    
    
    
dataLine :: Word32 -> Parser ([RGBcolour],[Word8])
dataLine w = (,) <$> payload w <*> padding w
  where
    payload i = iter i rgbColour
    padding i = iter (paddingMeasure i) getWord8

 -}          

rgbColour :: Parser RGBcolour
rgbColour = RGBcolour <$> getWord8 <*> getWord8 <*> getWord8



--------------------------------------------------------------------------------
-- Helpers


ignore :: Show a => String -> Parser a -> Parser ()
ignore name p = p >>= logfield name . show
  

logfield :: Show a => String -> a -> Parser ()
logfield name val = tell $ name  ++ " = " ++ show val ++ "\n"


iter :: Integral i => i -> Parser a -> Parser [a]
iter i p | i > 0     = (:) <$> p <*> iter (i-1) p
         | otherwise = return []
         
         
eof :: Parser Bool
eof = do
     bs <- gets remaining
     return $ B.null bs 
   
getWord8 :: Parser Word8
getWord8 = do
    bs <- gets remaining
    case B.uncons bs of
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

          