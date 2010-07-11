{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Shapes.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Combinators...
-- 
--------------------------------------------------------------------------------

module Wumpus.Shapes.Utils
  ( 

  -- * Starlings...
    star
  , star2

  ) where

-- starlings...

star     :: (a -> r -> ans) 
         -> (r -> a) 
         -> r -> ans
star f fa x = f (fa x) x

star2    :: (a -> b -> r -> ans) 
         -> (r -> a) 
         -> (r -> b) 
         -> r -> ans
star2 f fa fb x = f (fa x) (fb x) x

