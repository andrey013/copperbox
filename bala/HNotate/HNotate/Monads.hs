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
-- A restricted ReaderT (Reader) monad and a logging version 
-- WriterT (ReaderT (Reader)).
--
--------------------------------------------------------------------------------

module HNotate.Monads where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import System.IO (stdout)
import Text.PrettyPrint.Leijen





-- OutputReaderM is a double reader over a mutable 'env'
-- (i.e. supports local) and an immutable 'config' (no local).
newtype OutputReaderM env cfg a = OutputReaderM { 
                                      getOR :: ReaderT env (Reader cfg) a }

instance Monad (OutputReaderM env cfg) where
  return a = OutputReaderM $ return a
  ma >>= f = OutputReaderM $ getOR ma >>= getOR . f

instance MonadReader env (OutputReaderM env cfg) where
  ask     = OutputReaderM $ ask
  local f = OutputReaderM . local f . getOR

askConfig :: OutputReaderM env cfg cfg   
askConfig = OutputReaderM $ lift $ ask

asksConfig :: (cfg -> a) -> OutputReaderM env cfg a   
asksConfig = OutputReaderM . lift . asks

runOutputReader :: OutputReaderM env cfg a -> cfg -> env -> a
runOutputReader f config env = runReader (runReaderT (getOR f) env) config



-- DebugWriterT a Writer Monad specialized to output of type String
newtype DebugWriterT m a = DebugWriterT { getDWT :: WriterT String m a }

instance Monad m => Monad (DebugWriterT m) where
  return a = DebugWriterT $ return a
  ma >>= f = DebugWriterT $ getDWT ma >>= getDWT . f
  
instance MonadTrans DebugWriterT where
  lift = DebugWriterT . lift

instance Monad m => MonadWriter String (DebugWriterT m) where
  tell    = DebugWriterT . tell
  listen  = DebugWriterT . listen . getDWT
  pass    = DebugWriterT . pass . getDWT 
  
-- DebugWriterT on the Identity monad to get a non-transformer DebugWriter
type DebugWriter a = DebugWriterT Identity a

-- Debug version of the OutputReader monad
type OutputReaderDebugM env cfg a = DebugWriterT (OutputReaderM env cfg) a

-- Run functions

runDebugWriter :: DebugWriter a -> (a,String)
runDebugWriter m = runIdentity (runWriterT $ getDWT m)

runDebugWriterT :: DebugWriterT m a -> m (a,String)
runDebugWriterT m = runWriterT $ getDWT m


runOutputReader_debug :: OutputReaderDebugM env cfg a -> cfg -> env -> (a,String)
runOutputReader_debug m config env =
  runOutputReader (runWriterT $ getDWT m) config env
  
  

-- Specialized printers 

runInIO :: DebugWriter a -> IO a
runInIO f = let (a,out) = runDebugWriter f in do
  putStr out
  return a

runIOInIO :: DebugWriterT IO a -> IO a
runIOInIO m = runDebugWriterT m >>= \(a,out) -> putStrLn out >> return a

  
output :: Monad m => Doc -> DebugWriterT m ()
output d = tell $ displayS (renderPretty 0.4 80 d) ""



writePretty :: (Monad m, Pretty b) => String -> (a -> b) -> a -> DebugWriterT m b
writePretty title f = genWriteStep title pretty f

writeShow :: (Monad m, Show b) => String -> (a -> b) -> a -> DebugWriterT m b
writeShow title f = genWriteStep title (string . show) f

    
genWriteStep :: Monad m => String -> (b -> Doc) -> (a -> b) -> a -> DebugWriterT m b
genWriteStep title pp f a = do 
    tell $ title ++ "\n"
    let b = f a
    output (pp b <$> empty <$> empty)
    return b 

writePrettyM :: (Monad m, Pretty b) => 
             String -> (a -> m b) -> a -> DebugWriterT m b
writePrettyM title f = genWriteStepM title pretty f

writeShowM :: (Monad m, Show b) => 
            String -> (a -> m b) -> a -> DebugWriterT m b
writeShowM title f = genWriteStepM title (string . show) f

genWriteStepM :: Monad m => 
              String -> (b -> Doc) -> (a -> m b) -> a -> DebugWriterT m b
genWriteStepM title pp m a = do 
    tell $ title ++ "\n"
    b <- lift $ m a
    output (pp b <$> empty <$> empty)
    return b 