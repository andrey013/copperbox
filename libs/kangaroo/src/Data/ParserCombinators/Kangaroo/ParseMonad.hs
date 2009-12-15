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
import Data.Word
import System.IO

type ParseErr = String

type ImageData = IOUArray Int Word8   -- Is Int big enough for index?

type St    = Int            -- 'file' position
type Env   = ImageData
  

newtype Kangaroo a = Kangaroo { 
          getKangaroo :: Env -> St -> IO (St,Either ParseErr a) }
          
instance Functor Kangaroo where
    fmap f (Kangaroo x) = Kangaroo $ 
         \env st  ->  x env st `bindIO` \(st',a) -> return (st',fmap f a)




bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)

returnIO :: a -> IO a
returnIO = return


instance Monad Kangaroo where
  return a = Kangaroo $ \_ st -> returnIO (st,Right a)
  (Kangaroo x) >>= f = Kangaroo $ 
     \env st -> x env st `bindIO` \(st',ans) ->
                                     case ans of 
                                        Left err -> returnIO (st',Left err)
                                        Right a  -> getKangaroo (f a) env st'



instance Applicative Kangaroo where
  pure = return
  (<*>) = ap


getSt :: Kangaroo St
getSt = Kangaroo $ \_ st -> return (st, Right st)

putSt :: St -> Kangaroo ()
putSt st = Kangaroo $ \_ _ -> return (st, Right ())

modifySt :: (St -> St) -> Kangaroo ()
modifySt f = Kangaroo $ \_ st -> return (f st, Right ())

askEnv :: Kangaroo Env
askEnv = Kangaroo $ \env st -> return (st, Right env)


throwErr :: String -> Kangaroo a
throwErr msg = Kangaroo $ \_ st -> return (st,Left msg)

liftIOAction :: IO a -> Kangaroo a
liftIOAction ma = Kangaroo $ \_ st -> ma >>= \a -> return (st,Right a) 


runKangaroo :: Kangaroo a -> FilePath -> IO (Either ParseErr a)
runKangaroo p filename = withBinaryFile filename ReadMode $ \ handle -> do 
    sz'     <- hFileSize handle
    let sz = fromIntegral sz'
    arr     <- newArray_ (0,sz-1)
    _rsz    <- hGetArray handle arr  (fromIntegral sz)
    (_,ans) <- runP p arr
    return ans   
  where 
    runP :: Kangaroo a -> ImageData -> IO (St, Either ParseErr a) 
    runP (Kangaroo x) arr = x arr 0


--------------------------------------------------------------------------------
-- 


   
word8 :: Kangaroo Word8
word8 = do
    ix   <- getSt
    arr  <- askEnv
    a    <- liftIOAction $ readArray arr ix
    putSt $ ix+1
    return a


eof :: Kangaroo Bool
eof = do
     ix  <- getSt
     arr <- askEnv
     (_,up)  <- liftIOAction $ getBounds arr
     return $ (ix>=up) 


reportFail :: String -> Kangaroo a
reportFail s = do 
    posn <- getSt
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   



