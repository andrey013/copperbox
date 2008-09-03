{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  multi-parameter typeclasses
--
-- Common utils - mainly for the phantom type wrappers over Doc.
--
--------------------------------------------------------------------------------

module HNotate.CommonUtils where


import Data.Monoid
import Data.Sequence hiding (empty)
import Text.PrettyPrint.Leijen

sepSeq op sq = case viewl sq of
    EmptyL    -> empty
    e :< se   -> trav (pretty e) (viewl se)
  where
    trav doc EmptyL    = doc
    trav doc (e :< se) = trav (doc `op` pretty e) (viewl se)
    
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
    

class Sequence a

-- The sequence append operator
infixl 4 <+<
(<+<) :: (Monoid (t se), Sequence se) => t se -> t se -> t se
(<+<) se se' = se `mappend` se'

infixl 7 *!
(*!) e oa   = maybe e (e !) oa

infixl 7 !*>
(!*>) oa e   = maybe e ((flip (!>)) e) oa




