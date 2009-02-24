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
  runParser,
  evalParser,
  
) where


import Graphics.ZBitmap.InternalSyntax
import Graphics.ZBitmap.Utils ( paletteSize )

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

    
readBmp :: FilePath -> IO BmpBitmap  
readBmp name = do 
    h <- openBinaryFile name ReadMode
    bs <- BS.hGetContents h
    let (ans,msg) = runParser bmpFile bs    
    case ans of 
      Left err -> hClose h >> putStrLn err >> putStrLn msg 
                           >> error "readBmp failed"
      Right mf -> hClose h >> return mf
      
--------------------------------------------------------------------------------

 
    
bmpFile :: Parser BmpBitmap  
bmpFile = do
    hdr                 <- header
    (sz,bpp,cmprz,dib)  <- dibheader
    o_ps                <- palette bpp
    let bs_size = fromIntegral sz -- might need to subtract the palette_spec size
    body                <- imageData cmprz bs_size
    return $ BmpBitmap hdr dib o_ps body

header :: Parser BmpHeader
header = (\c1 c2 sz r1 r2 off -> makeBmpHeaderLong c1 c2 sz r1 r2 off)
    <$> char <*> char     <*> word32le 
             <*> word16le <*> word16le <*> word32le 


type DataSize = Word32
type DibAns = (DataSize, BmpBitsPerPixel, BmpCompression, BmpDibHeader)

dibheader :: Parser DibAns
dibheader = 
    (\w h cp bpp cm sz hr vr pd cs -> 
        (sz, bpp, cm, makeBmpDibHeaderLong w h cp bpp cm sz hr vr pd cs)) 
      <$> (word32le  *> word32le) <*> word32le 
                    <*> word16le  <*> bitsPerPixel <*> compression
                    <*> word32le  <*> word32le     <*> word32le
                    <*> word32le  <*> word32le

palette :: BmpBitsPerPixel -> Parser (Maybe Palette)
palette bpp = case paletteSize bpp of
    0 -> return Nothing
    n -> do cs <- count n paletteColour       -- 4 bytes per colour
            return $ Just $ makePalette cs

paletteColour :: Parser PaletteColour
paletteColour = (\b g r _ -> PaletteColour r g b) <$>
    word8 <*> word8 <*> word8 <*> word8

makePalette :: [PaletteColour] -> Palette                                 
makePalette cs = Palette sz $ listArray (0,sz) cs where
    sz                    = length cs - 1
    
    
-- Return an empty ByteString if the image is compressed.
-- The image header might still be important.
imageData :: BmpCompression -> Int -> Parser BmpDibImageData
imageData Bi_RGB sz = getByteString sz  
imageData _      _  = return BS.empty

    
bitsPerPixel :: Parser BmpBitsPerPixel
bitsPerPixel = unmarshalBmpBitsPerPixel <$> word16le
    
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
          
          
getByteString :: Integral i => i -> Parser BS.ByteString
getByteString n = do 
    bs <- gets remaining
    let rlen = BS.length bs
    if i > rlen 
      then getFailure rlen
      else let (front,rest) = BS.splitAt i bs in do 
                p <- gets pos 
                modify (\s -> s { remaining=rest, pos=p+i } )
                return front
  where
    i = fromIntegral n
    
    getFailure rlen = reportFail $ 
        "getByteString failed trying to get " ++ show i ++ 
        " bytes, with only " ++ show rlen ++ " remaining.\n" 
      
   
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

          