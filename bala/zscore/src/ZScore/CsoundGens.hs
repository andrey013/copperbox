{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.CsoundGens
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- CsoundScore
--
--------------------------------------------------------------------------------

module ZScore.CsoundGens
  (
  
  -- * Sinusoids
    gen9
  , gen10

  -- * File access 
  , gen1

  ) where

import ZScore.CsoundScore

-- | sinusoids
-- 
gen9 :: Int -> Double -> Int -> [(Double,Double,Double)] -> TableStmt
gen9 i t sz xs = 
    TableStmt { table_num       = i
              , table_atime     = t
              , table_size      = sz
              , gen_routine     = 9
              , table_args      = ps
              }
  where
    ps = concatMap (\(a,b,c) -> [ CsDouble a, CsDouble b, CsDouble c ]) xs


gen10 :: Int -> Double -> Int -> [Double] -> TableStmt
gen10 i t sz xs = 
    TableStmt { table_num       = i
              , table_atime     = t
              , table_size      = sz
              , gen_routine     = 10
              , table_args      = map CsDouble xs
              }


-- | File access
--
gen1 :: Int -> Double -> Int -> String -> Double -> Int -> TableStmt
gen1 i t sz fc skip fmt = 
    TableStmt { table_num       = i
              , table_atime     = t
              , table_size      = sz
              , gen_routine     = 1
              , table_args      = [CsString fc, CsDouble skip, CsInt fmt]
              }

