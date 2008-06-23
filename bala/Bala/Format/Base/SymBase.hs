{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Base.SymBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  empty data declarations, multi-parameter typeclasses
--
-- Common functions and types for syntax trees represented in the 
-- final-tagless style.
--
--------------------------------------------------------------------------------

module Bala.Format.Base.SymBase where

import Text.PrettyPrint.Leijen


infixl 5 `cSnoc`

data CList ctx
class SymCList repr ctx where
  cNil :: repr (CList ctx)
  cSnoc :: repr (CList ctx) -> repr (a ctx) ->  repr (CList ctx) 


infixl 5 +++

-- | (+++) - alias for cSnoc.
(+++) :: (SymCList repr ctx) 
      => repr (CList ctx) -> repr (a ctx) ->  repr (CList ctx) 
(+++) es e = cSnoc es e

-- * Attributes
-- | Wrap an element as an attribute.
class Attr repr where
  attr :: repr (a ctx) -> repr (e ctx) -> repr (e ctx)
  
  
-- * Composition 
infixl 7 #
-- | Reverse application.
( # ) :: a -> (a -> b) -> b
x # f = f x


-- CAN WE DO WITHOUT << ?

infixr 6 << 

-- | Higher precedence version of the application operator ($).
(<<) ::(a -> b) -> a ->  b
f << a = f a 

-- * Meter fraction
-- | An alternative to Data.Rational which normalizes where possible
-- e.g. 4\/4 becomes 1\/1. For time signatures we don't want to noramlize.
data MeterFraction = Int :% Int

infixl 2 %

-- | Synonym for the infix constructor (:%).
(%) :: Int -> Int -> MeterFraction
(%) n d = n :% d
  
  
--------------------------------------------------------------------------------
-- * Pretty printing

-- | To generate output we need a pretty printing interpretation.
newtype P a = P { unP :: Doc }

-- | Print a document with a unit argument. Documents taking () as an argument
-- do not have to be explicitly typed and avoid the monomorphism restriction.
printP :: (() -> P a) -> IO ()
printP x = putDoc $ unP (x ())


instance Pretty MeterFraction where
  pretty (n :% d) = group $ int n <> char '/' <> int d

instance Show MeterFraction where
  showsPrec _ (n :% d) = shows n . showChar '/' . shows d
  
