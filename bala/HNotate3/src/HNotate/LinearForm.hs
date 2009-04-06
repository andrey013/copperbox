{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.LinearForm
-- Copyright   :  (c) Stephen Tetley 2009
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
import qualified Data.Foldable as F
import Data.List ( intersperse )
import Data.Sequence

import Text.PrettyPrint.Leijen hiding ( empty, (<$>) )
import qualified Text.PrettyPrint.Leijen as PP

type DblSeq a = Seq (Seq a) 

concat :: Seq (Seq a) -> Seq a
concat = F.foldr (><) empty

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
    
    
pplf :: Pretty a => DblSeq a -> Doc
pplf = vsep . prepunctuate (text " // ") . F.foldr (\e a -> step e:a) [] where
    step = hsep . intersperse (text "...") . F.foldr (\e a -> pretty e:a) []

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : foldr (\e a -> p <> e : a) [] ds


    