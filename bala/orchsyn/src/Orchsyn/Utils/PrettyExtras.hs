{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Utils.PrettyExtras
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extra helpers for Text.PrettyPrint.HughesPJ.
--
--------------------------------------------------------------------------------

module Orchsyn.Utils.PrettyExtras
  (
    Format(..)

  , dtrunc
  , separate
  , vconcat
  , vconcatSpace

  , padStringR
  , padStringL

  , arglist

  ) where

import Numeric

import Text.PrettyPrint.HughesPJ


--------------------------------------------------------------------------------

class Format a where format :: a -> Doc

instance Format Int where
  format = int

instance Format Integer where
  format = integer

instance Format Double where
  format = double




--------------------------------------------------------------------------------
-- \"Minimizing\" Doubles

dtrunc :: Double -> Doc
dtrunc = text . truncateDouble


-- | Truncate the printed decimal representation of a Double.
-- The is prefered to 'showFFloat' from Numeric as it produces
-- shorter representations where appropriate.
-- 
-- 0.000000000 becomes 0 rather than however many digs are 
-- specified.
--  
truncateDouble :: Double -> String
truncateDouble d | abs d < 0.0001  = "0"
                 | d < 0.0         = '-' :  fn (abs tx)
                 | otherwise       = fn tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0

    fn v = let (a,b) = properFractioni v 
           in if b == 0.0 then show a else ($ "") $ showFFloat Nothing v

roundi :: RealFrac a => a -> Integer
roundi = round


properFractioni :: RealFrac a => a -> (Integer,a)
properFractioni = properFraction




separate :: Doc -> [Doc] -> Doc
separate _   []      = empty
separate sepa (a:as) = step a as
  where
    step acc []     = acc
    step acc (x:xs) = step (acc <> sepa <> x) xs


-- | Vertical concatenate a list of documents with @$+$@.
-- 
vconcat :: [Doc] -> Doc
vconcat []     = empty 
vconcat (a:as) = go a as
  where
    go ac []     = ac
    go ac (b:bs) = go (ac $+$ b) bs


-- | Vertical concatenate a list of documents with a black line
-- as separator.
-- 
vconcatSpace :: [Doc] -> Doc
vconcatSpace []     = empty 
vconcatSpace (a:as) = go a as
  where
    go ac []     = ac
    go ac (b:bs) = go (ac $+$ text "" $+$ b) bs


padStringR :: Int -> String -> Doc
padStringR i ss = text ss <> text (replicate n ' ')
  where
    n = i - length ss


padStringL :: Int -> String -> Doc
padStringL i ss = text (replicate n ' ') <> text ss
  where
    n = i - length ss



arglist :: [Doc] -> Doc
arglist = separate (text ", ")

