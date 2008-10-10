

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

instance Monoid Doc where
  mempty = empty
  mappend = (</>)

type DebugWriter a = Writer String a

runInIO :: DebugWriter a -> IO a
runInIO f = let (a,out) = runWriter f in do
  putStr out -- displayIO stdout (renderPretty 0.7 80 out)
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
    