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
   

  -- * Sine cosine genarators
    gen9
  , gen10
  , gen19
  , gen11

  -- * line / expon segment generators
  , gen5
  , gen6
  , gen7
  , gen8
  , gen25
  , gen27

  -- * File access 
  , gen1
  

  


  ) where

import ZSnd.Core.CsoundScore




--------------------------------------------------------------------------------
-- sine / cosine generators


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen9 :: time * size * [(partial_num, strength, inital_phase)]
-- 
gen9 :: Double -> Int -> [(Double,Double,Double)] -> ScoBuilder ()
gen9 t sz xs = dyngen 9 t sz (double3 xs)

-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
--
-- > gen10 :: time * size * [relative_strength]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen10 :: Double -> Int -> [Double] -> ScoBuilder ()
gen10 t sz xs = dyngen 10 t sz (map CsDouble xs)

-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen19 :: time * size * [(partial_num, strength, inital_phase, dc_offset)]
--
gen19 :: Double -> Int -> [(Double,Double,Double,Double)] -> ScoBuilder ()
gen19 t sz xs = dyngen 19 t sz (double4 xs)

-- | Generate additive set of cosine partials.
-- 
-- > gen11 :: time * size * num_harmonics *
--
-- @num_harmonics@ must be positive.
--
-- Note - the corresponding Csound gen also allows optional 
-- lowest harmonic partial and multiplier.
-- 
-- ZSnd needs extending to handle this optional cases... 
--
gen11 :: Double -> Int -> Int -> ScoBuilder ()
gen11 t sz nh  = dyngen 11 t sz [ CsInt nh ]


--------------------------------------------------------------------------------
-- Line / exponential segment generators


-- | Construct exponential curve table.
-- 
-- > gen10 :: time * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @ordinate_values@ cannot be zero, use 0.001 for a near zero 
-- number.
--
gen5 :: Double -> Int -> [(Double, Int) ] -> ScoBuilder ()
gen5 t sz xs = dyngen 5 t sz (concatMap fn xs)
  where
    fn (d,i) = [CsDouble d, CsInt i]


-- | Construct a table of cubic polynomial segments.
-- 
-- > gen6 :: time * size * a -> [(n, b, n+1, c, n+2, d)]
-- 
-- @m0@ is the start maxima, sucessive curve segments are 
-- specified as ordinate_value and maxima tuples interspersed
-- with the number of stored values for each segement 
-- (n, n+1, n+2).  
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen6 :: Double -> Int -> Double 
     -> [(Int,Double, Int,Double, Int,Double)] -> ScoBuilder ()
gen6 t sz a xs = dyngen 6 t sz (CsDouble a : concatMap fn xs)
  where
    fn (n,b,n1,c,n2,d) = [ CsInt n,  CsDouble b
                         , CsInt n1, CsDouble c
                         , CsInt n2, CsDouble d 
                         ]



-- | Construct a table of straight line segments.
-- 
-- > gen7 :: time * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
--
gen7 :: Double -> Int -> [(Double, Int)] -> ScoBuilder ()
gen7 t sz xs = dyngen 7 t sz (concatMap fn xs)
  where
    fn (a,n) = [ CsDouble a, CsInt n ] 



-- | Construct a table of cubic spine segments.
-- 
-- > gen8 :: time * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
-- The sum of segment lengths is usually expected to equal @size@.
--
-- Note - potentially the segment list should be three * two-tuples.
--
gen8 :: Double -> Int -> Double -> [(Int,Double)] -> ScoBuilder ()
gen8 t sz a xs = dyngen 8 t sz (CsDouble a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsDouble b ] 


-- | Construct exponential curves in breakpoint fashion.
-- 
-- > gen25 :: time * size * table_loc * [(break_point, table_loc)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen25 :: Double -> Int -> Int -> [(Int,Int)] -> ScoBuilder ()
gen25 t sz a xs = dyngen 25 t sz (CsInt a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsInt b ] 


-- | Construct straight lines in brreakpoint fashion.
-- 
-- > gen8 :: time * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen27 :: Double -> Int -> Int -> [(Int,Int)] -> ScoBuilder ()
gen27 t sz a xs = dyngen 27 t sz (CsInt a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsInt b ] 


--------------------------------------------------------------------------------
-- File access

-- | File access.
--
-- > gen1 :: time * size * file_name * skip_time * format
-- 
-- @skip_time@ is read position start position within the file.
-- 
-- Use @format=0@ to read the format form the sound file header.
--
-- Note - to be statically typed, this function is more 
-- constrained than the Csound counterpart. Csound accounts for
-- anonymous numbered sound files, ZSyn needs all files to be 
-- named.
-- 
gen1 :: Double -> Int -> String -> Double -> Int -> ScoBuilder ()
gen1 t sz fc skip fmt = dyngen 1 t sz [CsString fc, CsDouble skip, CsInt fmt]



       



--------------------------------------------------------------------------------

-- helpers 

double3 :: [(Double,Double,Double)] -> [CsoundType]
double3 = concatMap fn 
  where
    fn (a,b,c) = [CsDouble a, CsDouble b, CsDouble c]

             
double4 :: [(Double,Double,Double,Double)] -> [CsoundType]
double4 = concatMap fn 
  where
    fn (a,b,c,d) = [CsDouble a, CsDouble b, CsDouble c, CsDouble d]
