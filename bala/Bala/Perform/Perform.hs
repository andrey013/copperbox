{-# OPTIONS_GHC -XMultiParamTypeClasses #-}
{-# OPTIONS_GHC -XFlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Perform.Perform
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type class for `performing` a music representation
--
--------------------------------------------------------------------------------


module Bala.Perform.Perform where

data Perform evt env out = Perform {
    render :: [evt] -> env -> out,
    perform :: out  -> env -> IO ()
  }

output :: [evt] -> env -> Perform evt env out -> IO ()
output xs env (Perform {render=r,perform=p})
  = let o = r xs env in p o env



