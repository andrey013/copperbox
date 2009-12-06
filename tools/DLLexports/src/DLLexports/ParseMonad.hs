{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DLLexports.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A MIDI file parser. 
--
--------------------------------------------------------------------------------

module DLLexports.ParseMonad where

import Control.Applicative
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Int
import Data.Word


type ParseErr = String
  
data ParseState = PSt { remaining :: B.ByteString,
                        pos       :: Int }

newtype Parser a = Parser { getParser :: ParseState -> (ParseState,Either ParseErr a) }
          
instance Functor Parser where
    fmap f (Parser x) = Parser $ \st -> let (st',a) = x st in (st',fmap f a)


instance Monad Parser where
  return a = Parser $ \st -> (st,Right a)
  (Parser x) >>= f = Parser $ \st -> let (st',ans) = x st
                                     in case ans of 
                                        Left err -> (st',Left err)
                                        Right a  -> getParser (f a) st'





instance Applicative Parser where
  pure = return
  (<*>) = ap


getSt :: Parser ParseState
getSt = Parser $ \st -> (st, Right st)

getSt_ :: (ParseState -> a) -> Parser a
getSt_ f = Parser $ \st -> (st, Right $ f st)

putSt :: ParseState -> Parser ()
putSt st = Parser $ \_ -> (st, Right ())

modifySt :: (ParseState -> ParseState) -> Parser ()
modifySt f = Parser $ \st -> (f st, Right ())


throwErr :: String -> Parser a
throwErr msg = Parser $ \st -> (st,Left msg)




runParser :: Parser a -> B.ByteString  -> Either ParseErr a   
runParser parser bs0 = case runP parser bs0 of
    (_st, Left err) -> Left $ errorstring err ""
    (_, Right a)    -> Right a  
  where 
    runP :: Parser a -> B.ByteString  -> (ParseState, Either ParseErr a) 
    -- should we use runState and check input is exhausted? 
    runP (Parser x) bs = x (initState bs)
    
    initState bs = PSt { remaining = bs, pos = 0 }
    
    errorstring err ss = err ++ "\n\nParse result upto error...\n" ++ ss 
    


--------------------------------------------------------------------------------
-- 


--------------------------------------------------------------------------------
-- helpers


reportFail :: String -> Parser a
reportFail s = do 
    posn <- getSt_ pos
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   


eof :: Parser Bool
eof = do
     bs <- getSt_ remaining
     return $ B.null bs 


   
getWord8 :: Parser Word8
getWord8 = do
    bs <- getSt_ remaining
    case B.uncons bs of
        Nothing    -> throwErr "Unexpected eof" 
        Just (a,b) -> do { i <- getSt_ pos 
                         ; modifySt (\s -> s { remaining=b, pos=i+1 } )
                         ; return a } 

getChar8bit :: Parser Char
getChar8bit = (chr . fromIntegral) <$> getWord8 

filePosition :: Parser Int
filePosition = getSt_ pos


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

getWord24be   :: Parser Word32
getWord24be   = w32be 0   <$> getWord8 <*> getWord8 <*> getWord8


getWord8split :: Parser (Word8,Word8) 
getWord8split = f <$> getWord8 
  where
    f i = ((i .&. 0xF0) `shiftR` 4, i .&. 0x0F)

  

w16be :: Word8 -> Word8 -> Word16
w16be a b = let a' = a `iShiftL` 8
                b' = fromIntegral b 
            in a' + b'  
            
w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d = let a' = a `iShiftL` 24
                    b' = b `iShiftL` 16
                    c' = c `iShiftL` 8
                    d' = fromIntegral d
            in a' + b' + c' + d'      

w16le :: Word8 -> Word8 -> Word16
w16le a b = let a' = fromIntegral a 
                b'  = b `iShiftL` 8
            in a' + b'  

w32le :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32le a b c d = let a' = fromIntegral a
                    b' = b `iShiftL` 8
                    c' = c `iShiftL` 16
                    d' = d `iShiftL` 24
            in a' + b' + c' + d'      


iShiftL :: (Bits b, Integral a, Integral b) => a -> Int -> b
a `iShiftL` x = (fromIntegral a) `shiftL` x      



assertWord8 :: Word8 -> Parser Word8
assertWord8 i = getWord8 >>= fn
  where fn j | j == i    = return i
             | otherwise = throwErr $ "assertWord8 failed"
             
assertLength :: Integral a => a -> Parser Word32
assertLength i  = getWord32be >>= fn
  where 
    fn n | n == fromIntegral i  = return n
         | otherwise            = throwErr $
              "assertLength " ++ show i ++ " /= " ++ show n

assertString :: String -> Parser String
assertString s  = getChars (length s) >>= fn
  where 
    fn ss | ss == s   = return s
          | otherwise = throwErr $ 
               "assertString " ++ (showString s []) ++ " /= " ++ (showString ss [])
                                     

getChars :: Integral a => a -> Parser [Char]  
getChars i = map (chr . fromIntegral) <$> count (fromIntegral i) getWord8 

getBytes :: Integral a => a -> Parser [Word8]
getBytes i = count (fromIntegral i) getWord8

getVarlenText :: Parser (Word32,String)  
getVarlenText = getVarlen     >>= \i  -> 
                getChars i    >>= \cs -> return (i,cs)

getVarlenBytes :: Parser (Word32,[Word8]) 
getVarlenBytes = getVarlen    >>= \i  ->  
                 getBytes i   >>= \bs -> return (i,bs)
                 
getVarlen :: Parser Word32
getVarlen = recVarlen 0            
  where
    recVarlen acc = do
        i <- getWord8
        if (varBitHigh i == False)
          then (return $ merge acc i)
          else (recVarlen $ merge acc i)
    
    varBitHigh i = i `testBit` 7
        
    merge acc i = (acc `shiftL` 7) + ((fromIntegral i) .&. 0x7F)
    



