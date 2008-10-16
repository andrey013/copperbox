{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DebugWriter
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A specialized writer monad for debugging.
--
--------------------------------------------------------------------------------


module HNotate.DebugWriter where

import Control.Monad.Writer
import Data.Monoid
import System.IO (stdout)
import Text.PrettyPrint.Leijen


newtype DebugWriter a = DebugWriter { getDW :: Writer String a }

runDebugWriter :: DebugWriter a -> (a, String) 
runDebugWriter = runWriter . getDW

instance Monad DebugWriter where
  return a = DebugWriter $ return a
  ma >>= f = DebugWriter $ getDW ma >>= getDW . f

instance MonadWriter String DebugWriter where
  tell    = DebugWriter . tell
  listen  = DebugWriter . listen . getDW
  pass    = DebugWriter . pass . getDW 

runInIO :: DebugWriter a -> IO a
runInIO f = let (a,out) = runDebugWriter f in do
  putStr out
  return a


output :: Doc -> DebugWriter ()
output d = tell $ displayS (renderPretty 0.4 80 d) ""



writeStep :: Pretty b => String -> (a -> b) -> a -> DebugWriter b
writeStep title = genWriteStep title pretty

    
genWriteStep :: String -> (b -> Doc) -> (a -> b) -> a -> DebugWriter b
genWriteStep title pp f a = do 
    tell $ title ++ "\n"
    let b = f a
    output (pp b <$> empty <$> empty)
    return b 
    