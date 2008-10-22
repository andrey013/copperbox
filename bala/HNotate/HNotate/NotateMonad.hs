{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

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


import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import System.IO (stdout)
import Text.PrettyPrint.Leijen





-- OutputReaderM is a double reader over a mutable Env
-- (i.e. supports local) and an immutable Config (no local).

-- The Config (cfg) and Env (env) are parameteric only to avoid 
-- mutually recursive modules


newtype NotateMonadT env cfg m a = NotateMonadT {  
    getNotateMonadT :: WriterT String (ReaderT env (ReaderT cfg m)) a }

instance Monad m => Functor (NotateMonadT env cfg m) where
    fmap f = NotateMonadT . fmap f . getNotateMonadT 


instance Monad m => Monad (NotateMonadT env cfg m) where
  return a = NotateMonadT $ return a
  ma >>= f = NotateMonadT $ getNotateMonadT ma >>= getNotateMonadT . f

instance Monad m => MonadReader env (NotateMonadT env cfg m) where
  ask     = NotateMonadT $ ask 
  local f = NotateMonadT . local f . getNotateMonadT

instance Monad m => MonadWriter String (NotateMonadT env cfg m) where
  tell    = NotateMonadT . tell
  listen  = NotateMonadT . listen . getNotateMonadT
  pass    = NotateMonadT . pass . getNotateMonadT 

instance MonadTrans (NotateMonadT env cfg) where
  lift = NotateMonadT . lift . lift . lift
    
instance (MonadIO m) => MonadIO (NotateMonadT env cfg m) where
    liftIO = lift . liftIO


ask_config :: Monad m => NotateMonadT env cfg m cfg   
ask_config = NotateMonadT $ lift $ lift $ ask

asks_config :: Monad m => (cfg -> a) -> NotateMonadT env cfg m a   
asks_config = NotateMonadT . lift . lift . asks

runNotateMonadT :: Monad m => NotateMonadT env cfg m a -> env -> cfg -> m (a,String)
runNotateMonadT m env cfg =
  runReaderT (runReaderT (runWriterT $ getNotateMonadT m) env) cfg
    

-- Specialized printers 
  
output :: Monad m => Doc -> NotateMonadT env cfg m ()
output d = tell $ displayS (renderPretty 0.4 80 d) ""

writePretty :: (Monad m, Pretty a) => String -> a -> NotateMonadT env cfg m a
writePretty title = genWriteStep title pretty 

writeShow :: (Monad m, Show a) => String -> a -> NotateMonadT env cfg m a
writeShow title = genWriteStep title (string . show)

    
genWriteStep :: Monad m =>  String -> (a -> Doc) -> a -> NotateMonadT env cfg m a
genWriteStep title pp a = do 
    tell $ title ++ "\n"
    output (pp a <$> empty <$> empty)
    return a 


writePrettyM :: (Monad m, Pretty a) => String -> m a -> NotateMonadT env cfg m a
writePrettyM title = genWriteStepM title pretty

writeShowM :: (Monad m, Show a) => String -> m a -> NotateMonadT env cfg m a
writeShowM title = genWriteStepM title (string . show)

     
genWriteStepM :: Monad m => String -> (a -> Doc) -> m a -> NotateMonadT env cfg m a
genWriteStepM title pp m = do 
    tell $ title ++ "\n"
    a <- lift $ m
    output (pp a <$> empty <$> empty)
    return a
