{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Monads
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A a double reader monad with writer for logging
-- WriterT (ReaderT (Reader)).
-- The inner reader is immutable - no access to 'local' 
--
--------------------------------------------------------------------------------

module HNotate.NotateMonad where

import HNotate.Document

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer



newtype NotateErr = NotateErr String
  deriving (Show) 

instance Error NotateErr where
  noMsg = NotateErr ""
  strMsg s = NotateErr s 


-- OutputReaderM is a double reader over a mutable Env
-- (i.e. supports local) and an immutable Config (no local).

-- The Config (cfg) and Env (env) are parameteric only to avoid 
-- mutually recursive modules - they will only store Config and 
-- Env from the Env module.


newtype NotateMonadT env cfg m a = NotateMonadT {  
    getNotateMonadT :: ErrorT NotateErr 
                        (WriterT String 
                          (ReaderT env 
                            (ReaderT cfg m))) a }

instance Monad m => Functor (NotateMonadT env cfg m) where
    fmap f = NotateMonadT . fmap f . getNotateMonadT 


instance Monad m => Monad (NotateMonadT env cfg m) where
  return a = NotateMonadT $ return a
  ma >>= f = NotateMonadT $ getNotateMonadT ma >>= getNotateMonadT . f

instance Monad m => MonadError NotateErr (NotateMonadT env cfg m) where
  throwError     = NotateMonadT . throwError
  catchError m f = NotateMonadT $ 
                     catchError (getNotateMonadT m) (getNotateMonadT . f)
                                                      

instance Monad m => MonadReader env (NotateMonadT env cfg m) where
  ask     = NotateMonadT $ ask 
  local f = NotateMonadT . local f . getNotateMonadT

instance Monad m => MonadWriter String (NotateMonadT env cfg m) where
  tell    = NotateMonadT . tell
  listen  = NotateMonadT . listen . getNotateMonadT
  pass    = NotateMonadT . pass . getNotateMonadT 

instance MonadTrans (NotateMonadT env cfg) where
  lift = NotateMonadT . lift . lift . lift . lift
    
instance (MonadIO m) => MonadIO (NotateMonadT env cfg m) where
    liftIO = lift . liftIO


ask_config :: Monad m => NotateMonadT env cfg m cfg   
ask_config = NotateMonadT $ lift $ lift $ lift $ ask

asks_config :: Monad m => (cfg -> a) -> NotateMonadT env cfg m a   
asks_config = NotateMonadT . lift . lift . lift . asks



runNotateMonadT :: Monad m => NotateMonadT env cfg m a 
                   -> env -> cfg 
                   -> m (Either NotateErr a,String)
runNotateMonadT m env cfg =
  runReaderT (runReaderT (runWriterT (runErrorT $ getNotateMonadT m)) env) cfg

    

-- Specialized printers 

class Witness a where textrep :: a -> String

instance Witness ODoc where textrep = wpp

wpp :: ODoc -> String 
wpp = formatted 0 70


primOutput :: Monad m => String -> NotateMonadT env cfg m ()
primOutput s = tell s >> tell "\n"
    


witness :: (Monad m, Witness a) => 
           String -> a -> NotateMonadT env cfg m a 
witness s a = primOutput s >> primOutput (textrep a) >> return a

steno :: Monad m => String -> (a -> String) -> a -> NotateMonadT env cfg m a 
steno s f a = primOutput s >> primOutput (f a) >> return a

document :: Monad m => String -> (a -> ODoc) -> a -> NotateMonadT env cfg m a 
document s f a = primOutput s >> primOutput (wpp $ f a) >> return a

textoutput :: Monad m => String -> String ->  NotateMonadT env cfg m String
textoutput title a = primOutput title >>  primOutput a >> return a

