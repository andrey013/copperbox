{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Random access parse monad 
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.ParseMonad where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import System.IO

type ParseErr = String

type ImageData = IOUArray Int Word8   -- Is Int big enough for index?

type St    = Int            -- 'file' position
type Env   = ImageData
  

newtype Parser a = Parser { 
          getParser :: Env -> St -> IO (St,Either ParseErr a) }
          
instance Functor Parser where
    fmap f (Parser x) = Parser $ 
         \env st  ->  x env st `bindIO` \(st',a) -> return (st',fmap f a)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)

returnIO :: a -> IO a
returnIO = return


instance Monad Parser where
  return a = Parser $ \_ st -> returnIO (st,Right a)
  (Parser x) >>= f = Parser $ 
     \env st -> x env st `bindIO` \(st',ans) ->
                                     case ans of 
                                        Left err -> returnIO (st',Left err)
                                        Right a  -> getParser (f a) env st'



instance Applicative Parser where
  pure = return
  (<*>) = ap


getSt :: Parser St
getSt = Parser $ \_ st -> return (st, Right st)

putSt :: St -> Parser ()
putSt st = Parser $ \_ _ -> return (st, Right ())

modifySt :: (St -> St) -> Parser ()
modifySt f = Parser $ \_ st -> return (f st, Right ())

askEnv :: Parser Env
askEnv = Parser $ \env st -> return (st, Right env)


throwErr :: String -> Parser a
throwErr msg = Parser $ \_ st -> return (st,Left msg)

liftIOAction :: IO a -> Parser a
liftIOAction ma = Parser $ \_ st -> ma >>= \a -> return (st,Right a) 


runParser :: Parser a -> FilePath -> IO (Either ParseErr a)
runParser p filename = withBinaryFile filename ReadMode $ \ handle -> do 
    sz'     <- hFileSize handle
    let sz = fromIntegral sz'
    arr     <- newArray_ (0,sz-1)
    _rsz    <- hGetArray handle arr  (fromIntegral sz)
    (_,ans) <- runP p arr
    return ans   
  where 
    runP :: Parser a -> ImageData -> IO (St, Either ParseErr a) 
    runP (Parser x) arr = x arr 0


--------------------------------------------------------------------------------
-- 


   
getWord8 :: Parser Word8
getWord8 = do
    ix   <- getSt
    arr  <- askEnv
    a    <- liftIOAction $ readArray arr ix
    putSt $ ix+1
    return a


--------------------------------------------------------------------------------
-- helpers

-- | applicative cons
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


jumpto :: Int -> Parser ()
jumpto = putSt


reportFail :: String -> Parser a
reportFail s = do 
    posn <- getSt
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = getWord8 >>= 
    (\x -> if p x then return x else reportFail $ "satisfy...")

try :: Parser a -> Parser a
try p = Parser $ \env st -> (getParser p) env st >>= \ ans -> 
                    case ans of
                      (_,Left err) -> return (st,Left err)
                      okay         -> return okay

opt :: Parser a -> Parser (Maybe a)
opt p = Parser $ \env st -> (getParser p) env st >>= \ ans -> 
                    case ans of
                      (_,   Left _)  -> return (st, Right Nothing)
                      (st', Right a) -> return (st', Right $ Just a)

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = do 
   ans <- opt end
   case ans of
     Just _ -> return []
     Nothing -> p <:> manyTill p end 


eof :: Parser Bool
eof = do
     ix  <- getSt
     arr <- askEnv
     (_,up)  <- liftIOAction $ getBounds arr
     return $ (ix>=up) 

-- | Read a null-terminated string
cstring :: Parser String
cstring = manyTill char w8Zero


w8Zero :: Parser Word8
w8Zero = satisfy (==0)

getBytes :: Integral a => a -> Parser [Word8]
getBytes i = count (fromIntegral i) getWord8

char :: Parser Char
char = (chr . fromIntegral) <$> getWord8 

getChar8bit :: Parser Char
getChar8bit = (chr . fromIntegral) <$> getWord8 

filePosition :: Parser Int
filePosition = getSt


count :: Int -> Parser a -> Parser [a]
count i p = step i [] where
  step n xs  | n <= 0     = return (reverse xs)
             | otherwise  = p >>= \a -> step (n-1) (a:xs)
             


getInt8 :: Parser Int8
getInt8 = (fromIntegral . unwrap) <$> getWord8
  where
    unwrap :: Word8 -> Int
    unwrap i | i > 128   = (fromIntegral i) - 256
             | otherwise = fromIntegral i

getWord16be   :: Parser Word16
getWord16be   = w16be     <$> getWord8 <*> getWord8  

getWord32be   :: Parser Word32
getWord32be   = w32be     <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

getWord16le   :: Parser Word16
getWord16le   = w16le     <$> getWord8 <*> getWord8  

getWord32le   :: Parser Word32
getWord32le   = w32le     <$> getWord8 <*> getWord8 <*> getWord8 <*> getWord8

  

w16be :: Word8 -> Word8 -> Word16
w16be a b = (shiftL8 a) + fromIntegral b
     
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = (shiftL24 a) + (shiftL16 b) + (shiftL8 c) + fromIntegral d


w16le :: Word8 -> Word8 -> Word16
w16le a b = fromIntegral a + (shiftL8 b)

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = fromIntegral a + (shiftL8 b) + (shiftL16 c) + (shiftL24 d)      



shiftL8 :: (Bits b, Integral b) => Word8 -> b
shiftL8 = (`shiftL` 8) . fromIntegral


shiftL16 :: (Bits b, Integral b) => Word8 -> b
shiftL16 = (`shiftL` 16) . fromIntegral


shiftL24 :: (Bits b, Integral b) => Word8 -> b
shiftL24 = (`shiftL` 24) . fromIntegral


