{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.Utils.DocExtras
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Extra functions for the Text.PrettyPrint.HughesPJ library.
--
--------------------------------------------------------------------------------

module Majalan.Core.Utils.DocExtras
  (

    dtrunc
  , truncateDouble

  , decimal
  , decimalString
 
  ) where

import Numeric
import Text.PrettyPrint.HughesPJ



-- | Truncate the printed decimal representation of a Double.
-- The is prefered to 'showFFloat' from Numeric as it produces
-- shorter representations where appropriate.
-- 
-- 0.000000000 becomes 0 rather than however many digits are 
-- specified.
--  
dtrunc :: Double -> Doc
dtrunc = text . truncateDouble

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


-- | Print a decimal with the supplied precision
-- (first argument), right pad with spaces if the output is 
-- shorter than the column width (second arg).
--
decimal :: Int -> Int -> Double -> Doc
decimal prec colw = text . decimalString prec colw

-- | Print a decimal with the supplied precision
-- (first argument), right pad with spaces if the output is 
-- shorter than the column width (second arg).
--
decimalString :: Int -> Int -> Double -> String
decimalString prec colw d = step $ ($ "") $ showFFloat (Just prec) d
  where
    step ss = ss ++ replicate (colw - length ss) ' '