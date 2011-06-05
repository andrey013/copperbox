{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.GenRoutines
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Csound gen tables.
--
--------------------------------------------------------------------------------

module ZSnd.Core.GenRoutines
  (

  
  -- * Sinusoids
    gen9
  , gen10

  -- * File access 
  , gen1


  ) where

import ZSnd.Core.CsoundScore



-- | sinusoids
-- 
gen9 :: Int -> Double -> Int -> [(Double,Double,Double)] -> ScoBuilder ()
gen9 i t sz xs = dyngen 9 i t sz ps
  where
    ps = concatMap (\(a,b,c) -> [ CsDouble a, CsDouble b, CsDouble c ]) xs


gen10 :: Int -> Double -> Int -> [Double] -> ScoBuilder ()
gen10 i t sz xs = dyngen 10 i t sz (map CsDouble xs)
       




-- | File access - potentially this should be overloaded.
--
gen1 :: Int -> Double -> Int -> String -> Double -> Int -> ScoBuilder ()
gen1 i t sz fc skip fmt = dyngen 1 i t sz [CsString fc, CsDouble skip, CsInt fmt]
             

