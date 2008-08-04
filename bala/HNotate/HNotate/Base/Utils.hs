
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common utility functions.
--
--------------------------------------------------------------------------------

module HNotate.Base.Utils (
  simpledoc, displaySimple,
  applyi
  ) where

import Text.PrettyPrint.Leijen

simpledoc :: Doc -> SimpleDoc
simpledoc d = renderPretty 0.8 80 (pretty d)

displaySimple :: Doc -> String
displaySimple a = displayS (simpledoc $ a <$> empty) ""


-- | Apply a function i times.              
applyi :: Int -> (a -> a) -> a -> a
applyi i f a | i <= 0    = a
             | otherwise = applyi (i-1) f (f a) 

