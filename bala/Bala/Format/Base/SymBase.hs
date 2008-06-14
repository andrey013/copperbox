{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Base.SymBase
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common functions and types for syntax trees represented in the 
-- final-tagless style.
--
--------------------------------------------------------------------------------

module Bala.Format.Base.SymBase where

import Text.PrettyPrint.Leijen
import Data.Ratio


data Concatenation ctx

infixl 5 +++
class SymConcatenation ctx repr where
  (+++)  :: repr (a ctx) -> repr (b ctx) -> repr (Concatenation ctx)


-- | turn an element into an attribute
class Attr repr where
  attr :: repr (a ctx) -> repr (b ctx) -> repr (b ctx)
  
  
-- | reverse application  
infixl 7 #
(#) :: a -> (a -> b) -> b
x # f = f x

-- | high precedence application
infixr 6 << 
(<<) ::(a -> b) -> a ->  b
f << a = f a 


--------------------------------------------------------------------------------
-- Pretty printing
--------------------------------------------------------------------------------


newtype P a = P { unP :: Doc }

printP x = putDoc $ unP (x ())


pprational :: Rational -> Doc
pprational r = ppfraction (numerator r) (denominator r)

ppfraction :: (Integral a , Integral b) => a -> b -> Doc
ppfraction n d = 
  group $ (int . fromIntegral) n <> char '/' <> (int . fromIntegral) d
  
  
