{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Output.OutputAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Intermediate data type used by OutputAbc  - not particularly 
-- necessary we could go straight to the Doc type, but used for the 
-- time being.
--
--------------------------------------------------------------------------------

module Bala.Format.Output.OutputBase where


import Text.PrettyPrint.Leijen hiding (empty)
import qualified Text.PrettyPrint.Leijen as PP

import Data.Sequence
import qualified Data.Foldable as F


emptydoc :: PP.Doc
emptydoc = PP.empty

emptyseq :: Seq a
emptyseq = empty


type Enclose a = a -> a
type Caten a = a -> a -> a

data Skeleton a = Literal a
                | Nested (Enclose a) (Skeleton a)
                | Sequence (Caten a) (Seq (Skeleton a))
                | Attr (Caten a) (Skeleton a) (Skeleton a)



instance Pretty (Skeleton Doc) where
  pretty (Literal d)      = d
  pretty (Nested f a)     = f (pretty a)
  pretty (Sequence f sz)  = combineLeft pretty f emptydoc sz
  pretty (Attr f e a)     = (pretty e) `f` (pretty a)

combineLeft f op ntrl sz = case viewl sz of
    EmptyL -> ntrl
    a :< sa -> F.foldl fn (f a) sa
  where
    fn a b = a `op` (f b)

  
literal :: a -> Skeleton a
literal = Literal

nested' f = Nested f
 
nested o c = Nested (enclose o c)

sequenceL :: Caten a -> [Skeleton a] -> Skeleton a
sequenceL f = Sequence f . fromList

sequenceS :: Caten a -> Seq (Skeleton a) -> Skeleton a
sequenceS f = Sequence f 

attr o e attr = Attr o e attr 


----
run :: Pretty a => a -> IO ()   
run = putDoc . pretty 

literalP :: Pretty a => a -> Skeleton Doc
literalP = literal . pretty 




  

