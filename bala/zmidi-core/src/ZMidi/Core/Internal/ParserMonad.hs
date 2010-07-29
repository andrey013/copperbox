{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZMidi.Core.Internal.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A parse monad - better error handling than Binary-Get.
--
--------------------------------------------------------------------------------

module ZMidi.Core.Internal.ParserMonad
  (

    ErrMsg
  , Pos
  , ParseErr
  , ParserM
  , runParser
  
  , word8
  , int8

  , word16be
  , word24be
  , word32be
  , char8
  , reportError

  , count
  , gencount
  , text
  , boundRepeat

  ) where

import Control.Applicative
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Int
import Data.Word


type Pos = Int

data ParserState = ParserState 
       { pos       :: !Pos
       , input     :: L.ByteString 
       }


type ErrMsg = String

type ParseErr = (Pos,ErrMsg)

newtype ParserM a = ParserM 
          { getParserM :: ParserState -> (Either ParseErr a, ParserState) }


instance Functor ParserM where
  fmap f mf = ParserM $ \s -> let (ans,s') = getParserM mf s in (fmap f ans,s')
  

instance Applicative ParserM where
  pure a    = ParserM $ \s -> (Right a, s)
  af <*> ma = ParserM $ \s -> let (ef,s')  = getParserM af s
                              in case ef of 
                                Left e  -> (Left e, s') 
                                Right f -> let (a,s'') = getParserM ma s'
                                           in (fmap f a,s'')

instance Monad ParserM where
  return a  = ParserM $ \s -> (Right a, s)
  m >>= k   = ParserM $ \s -> let (ea,s') = getParserM m s
                              in case ea of
                                Left e -> (Left e, s')
                                Right a -> (getParserM . k) a s'

runParser :: L.ByteString -> ParserM a -> Either ParseErr a
runParser bs mf = fst $ getParserM mf (ParserState { pos = 0, input = bs})


getPos :: ParserM Int
getPos = ParserM $ \s -> (Right $ pos s, s)

word8 :: ParserM Word8
word8 = ParserM $ \s@(ParserState n bs) -> case L.uncons bs of 
    Nothing      -> (Left (n,"word8 - no more data."), s)
    Just (a,bs') -> (Right a, ParserState (n+1) bs')


-- NEEDS CHECKING!
--
int8 :: ParserM Int8
int8 = fromIntegral <$> word8


word16be :: ParserM Word16
word16be = ParserM $ \s@(ParserState n bs) -> case uncons2 bs of
    Nothing -> (Left (n,"word16be - no more data."), s)
    Just (a,b,bs') -> (Right $ w16be a b, ParserState (n+2) bs')

word24be :: ParserM Word32
word24be = ParserM $ \s@(ParserState n bs) -> case uncons3 bs of
    Nothing -> (Left (n,"word24be - no more data."), s)
    Just (a,b,c,bs') -> (Right $ w24be a b c, ParserState (n+3) bs')

word32be :: ParserM Word32
word32be = ParserM $ \s@(ParserState n bs) -> case uncons4 bs of
    Nothing -> (Left (n, "word32be - no more data."), s)
    Just (a,b,c,d,bs') -> (Right $ w32be a b c d, ParserState (n+4) bs')


char8 :: ParserM Char
char8 = (chr . fromIntegral) <$> word8



reportError :: String -> ParserM a
reportError msg = ParserM $ \s -> (Left (pos s, msg), s)




count :: Int -> ParserM a -> ParserM [a]
count i p 
    | i <= 0    = pure []
    | otherwise = (:) <$> p <*> count (i-1) p


gencount :: Integral i => ParserM i -> ParserM a -> ParserM (i,[a]) 
gencount plen p = plen >>= \i -> 
    count (fromIntegral i) p >>= \ans -> return (i,ans)


text :: Int -> ParserM String
text i = count i char8


boundRepeat :: Int -> ParserM a -> ParserM [a]
boundRepeat n p = getPos >>= \start -> step (start + n)
  where
    step lim = do { a <- p
                  ; i <- getPos 
                  ; case compare i lim of
                      LT -> do { as <- step lim; return (a:as) }
                      EQ -> return [a]
                      GT -> reportError "boundRepeat - parser exceeds limit"
                  }



--------------------------------------------------------------------------------
-- helpers


uncons2 :: L.ByteString -> Maybe (Word8,Word8,L.ByteString)
uncons2 bs = L.uncons bs  >>= \(a,bs1) -> 
             L.uncons bs1 >>= \(b,bs2) -> return (a,b,bs2)


uncons3 :: L.ByteString -> Maybe (Word8,Word8,Word8,L.ByteString)
uncons3 bs = L.uncons bs  >>= \(a,bs1) -> 
             L.uncons bs1 >>= \(b,bs2) -> 
             L.uncons bs2 >>= \(c,bs3) -> return (a,b,c,bs3)

uncons4 :: L.ByteString -> Maybe (Word8,Word8,Word8,Word8,L.ByteString)
uncons4 bs = L.uncons bs  >>= \(a,bs1)  -> 
             L.uncons bs1 >>= \(b,bs2) -> 
             L.uncons bs2 >>= \(c,bs3) -> 
             L.uncons bs3 >>= \(d,bs4) -> return (a,b,c,d,bs4)


w16be :: Word8 -> Word8 -> Word16
w16be a b       = (shiftL `flip` 8  $ fromIntegral a) + fromIntegral b

w24be :: Word8 -> Word8 -> Word8 -> Word32
w24be a b c     = (shiftL `flip` 16  $ fromIntegral a) 
                + (shiftL `flip`  8  $ fromIntegral b) 
                + fromIntegral c


w32be :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
w32be a b c d   = (shiftL `flip` 24  $ fromIntegral a) 
                + (shiftL `flip` 16  $ fromIntegral b) 
                + (shiftL `flip`  8  $ fromIntegral c) 
                + fromIntegral d