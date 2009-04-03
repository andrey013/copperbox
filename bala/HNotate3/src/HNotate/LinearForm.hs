{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.LinearForm
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Alternative note list representation
--
--------------------------------------------------------------------------------

module HNotate.LinearForm where

import Control.Applicative hiding ( empty )
import Data.Sequence

type DblSeq a = Seq (Seq a) 

root :: (env -> Seq (Seq a))
root = pure empty

infixl 6 ..., //

(...) :: (env -> DblSeq a) -> (env -> a) -> (env -> DblSeq a)
(...) f a = add_at_tip <$> f <*> a

(//) :: (env -> DblSeq a) -> (env -> a) -> (env -> DblSeq a)
(//) m a = add_with_new_tip <$> m <*> a
                    

                    
add_at_tip :: Seq (Seq a) -> a -> Seq (Seq a) 
add_at_tip sse x = step $ viewr sse where
    step EmptyR       = singleton $ singleton x
    step (ssa :> sa)  = ssa |> (sa |> x)

   
add_with_new_tip :: Seq (Seq a) -> a -> Seq (Seq a)
add_with_new_tip sse x = step $ viewr sse where
    step EmptyR       = singleton $ singleton x
    step _            = sse |> (singleton x)
    
    
    