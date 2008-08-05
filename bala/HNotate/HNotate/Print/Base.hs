{-# LANGUAGE FlexibleInstances, EmptyDataDecls, 
             MultiParamTypeClasses #-}

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

module HNotate.Print.Base where

import Data.Monoid
import Text.PrettyPrint.Leijen

infixr 6 <<

-- | Higher precedence version of the application operator ($).
(<<) ::(a -> b) -> a ->  b
f << a = f a


class WDoc t where
  unwrap :: t a -> Doc
  wrap   :: Doc -> t a
  
  
type Enclose = Doc -> Doc
type Caten = Doc -> Doc -> Doc

caten :: WDoc t => Caten -> t a -> t b -> t o
caten op a b = wrap $ unwrap a `op` unwrap b

caten3 :: WDoc t => Caten -> t a -> t b -> t c -> t o
caten3 op a b c = wrap $ unwrap a `op` unwrap b `op` unwrap c

caten4 :: WDoc t => Caten -> t a -> t b -> t c -> t d -> t o
caten4 op a b c d = wrap $ unwrap a `op` unwrap b `op` unwrap c `op` unwrap d

nested :: WDoc t => Enclose -> t a -> t b
nested f e = wrap $ f (unwrap e)

promote :: WDoc t => t a -> t b
promote = wrap . unwrap


class SuffixAttr cxte cxta

infixl 7 !

( ! ) :: (WDoc t, SuffixAttr cxte cxta) => t cxte -> t cxta -> t cxte
( ! ) e a = caten (<>) e a

class PrefixAttr cxte cxta

infixl 7 !>
( !> ) :: (WDoc t, PrefixAttr cxte cxta) => t cxta -> t cxte -> t cxte
( !> ) a e = caten (flip (<>)) e a

infixl 5 +++

(+++) :: (Monoid (t se), Append t se e, WDoc t) => t se -> t e -> t se
(+++) se e = se `mappend` lifts e

class (WDoc t) => Append t se e where 
  lifts :: (t e) -> (t se)
  lifts = wrap . unwrap
    




{-
-- A type constrained add-right
class Append cxts cxta

infixl 5 +++

(+++) :: (Monoid cxts, Append cxts cxta) => t cxts -> t cxta -> t cxts
(+++) se e = addright se e



-}







{-
literalP :: Pretty a => a -> Skeleton Doc
literalP = literal . pretty
-}





