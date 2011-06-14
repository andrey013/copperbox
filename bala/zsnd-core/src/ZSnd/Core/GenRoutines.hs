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
-- Note - some of the argument may have incorrect types - Doubles 
-- when they should be Ints, Ints when they should be Doubles.
--
--------------------------------------------------------------------------------

module ZSnd.Core.GenRoutines
  (
   

  -- * Sine cosine generators
    gen9
  , gen10
  , gen19
  , gen11

  -- * Line / exponential segment generators
  , gen5
  , gen6
  , gen7
  , gen8
  , gen25
  , gen27

  -- * File access 
  , gen1
  , gen23
  , gen28

  -- * Numeric value access
  , gen2
  , genN2
  , gen17

  -- * Window functions
  , Gen20Window(..)
  , gen20
  , genN20

  -- * Random functions
  , Gen21Dist(..)
  , gen21

  -- * Waveshaping
  , gen3
  , gen13
  , gen14
  , gen15

  -- * Amplitude scaling
  , gen4
  , gen12
  , genN12

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

-- | Read data from a sound file into a table.
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


-- | Read numeric values from a text file.
--
-- > gen23 :: time * size * file_name
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen23 :: Double -> Int -> String -> ScoBuilder ()
gen23 t sz file_path = dyngen 23 t sz [CsString file_path]


-- | Read a time-tagged trajectory from a file.
--
-- > gen28 :: time * file_name 
-- 
-- Note - size is automatically set to 0 in the generated Csound
-- score. Csound itself handles the allocation size.
-- 
gen28 :: Double -> String -> ScoBuilder ()
gen28 t file_name = dyngen 28 t 0 [CsString file_name]


--------------------------------------------------------------------------------
-- Numeric value access

-- | Transfer data from immediate p-fields into a table.
--
-- > gen2 :: time * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen2 :: Double -> Int -> [Int] -> ScoBuilder ()
gen2 t sz xs = dyngen 2 t sz (map CsInt xs)

-- | /Negative/ version of gen2.
--
-- > genN2 :: time * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN2 :: Double -> Int -> [Int] -> ScoBuilder ()
genN2 t sz xs = dyngen (-2) t sz (map CsInt xs)


-- | Generate a step table from the supplied pairs.
--
-- > gen17 :: time * size * [(ordinate,y_value)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen17 :: Double -> Int -> [(Int,Int)] -> ScoBuilder ()
gen17 t sz xs = dyngen 17 t sz (int2 xs)


--------------------------------------------------------------------------------
-- Window functions

-- | Enumeration of the window types for 'gen20'.
--
-- Note - @GAUSSIAN@ and @KAISER@ take extra arguments.
-- 
data Gen20Window = HAMMING | HANNING | BARTLETT | BLACKMAN
                 | BLACKMAN_HARRIS | GAUSSIAN Int | KAISER Int
                 | RECTANGLE | SYNC
  deriving (Eq,Ord,Show)


gen20Opts :: Gen20Window -> Int -> [CsoundValue]
gen20Opts HAMMING         mx = [CsInt 1, CsInt mx]     
gen20Opts HANNING         mx = [CsInt 2, CsInt mx]     
gen20Opts BARTLETT        mx = [CsInt 3, CsInt mx]     
gen20Opts BLACKMAN        mx = [CsInt 4, CsInt mx]
gen20Opts BLACKMAN_HARRIS mx = [CsInt 5, CsInt mx]
gen20Opts (GAUSSIAN y)    mx = [CsInt 6, CsInt mx, CsInt y]
gen20Opts (KAISER y)      mx = [CsInt 7, CsInt mx, CsInt y]
gen20Opts RECTANGLE       mx = [CsInt 8, CsInt mx]
gen20Opts SYNC            mx = [CsInt 9, CsInt mx]


-- | Generate functions of different windows.
--
-- > gen20 :: time * size * window_type * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen20 :: Double -> Int -> Gen20Window -> Int -> ScoBuilder ()
gen20 t sz wt mx = dyngen 20 t sz (gen20Opts wt mx)


-- | /Negative/ version of gen20.
--
-- The table is generated with a negative table number, which 
-- makes Csound rescale the maximum value - consult the Csound 
-- manual. e.g.:
-- 
-- > f1       0       1024    -20     2       456
--
genN20 :: Double -> Int -> Gen20Window -> Int -> ScoBuilder ()
genN20 t sz wt mx = dyngen (-20) t sz (gen20Opts wt mx)

--------------------------------------------------------------------------------
-- Random functions 

-- | Enumeration of the random distribution types for 'gen21'.
--
-- Note - @BETA@ and @WEIBULL@ take extra arguments.
-- 
data Gen21Dist = UNIFORM | LINEAR | TRIANGULAR | EXPON
               | BIEXPON | DIST_GAUSSIAN | CAUCHY | POSITIVE_CAUCHY
               | BETA Int Int | WEIBULL Int | POISSON
  deriving (Eq,Ord,Show)

gen21Opts :: Gen21Dist -> Double -> [CsoundValue]
gen21Opts UNIFORM         lv = [CsInt 1, CsDouble lv]
gen21Opts LINEAR          lv = [CsInt 2, CsDouble lv]
gen21Opts TRIANGULAR      lv = [CsInt 3, CsDouble lv]     
gen21Opts EXPON           lv = [CsInt 4, CsDouble lv]     
gen21Opts BIEXPON         lv = [CsInt 5, CsDouble lv]
gen21Opts DIST_GAUSSIAN   lv = [CsInt 6, CsDouble lv]
gen21Opts CAUCHY          lv = [CsInt 7, CsDouble lv]
gen21Opts POSITIVE_CAUCHY lv = [CsInt 8, CsDouble lv]     
gen21Opts (BETA a b)      lv = [CsInt 9, CsDouble lv, CsInt a, CsInt b]
gen21Opts (WEIBULL a)     lv = [CsInt 10, CsDouble lv, CsInt a]
gen21Opts POISSON         lv = [CsInt 11, CsDouble lv]


-- | Generate tabels of random distributions.
--
-- > gen21 :: time * size * distribution_type * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen21 :: Double -> Int -> Gen21Dist -> Double -> ScoBuilder ()
gen21 t sz dist lv = dyngen 21 t sz (gen21Opts dist lv)



--------------------------------------------------------------------------------
-- Waveshaping

-- | Generate a table by evaulating a polynomial.
--
-- > gen3 :: time * size * xval1 * xval2 * [coeffcients] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen3 :: Double -> Int -> Int -> Int -> [(Double,Double)] -> ScoBuilder ()
gen3 t sz x1 x2 xs = dyngen 3 t sz (CsInt x1 : CsInt x2 : double2 xs)


-- | Store a Chebyshev polynomial of the first kind.
--
-- > gen13 :: time * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen13 :: Double -> Int -> Int -> Int -> [Double] -> ScoBuilder ()
gen13 t sz x1 x2 xs = dyngen 13 t sz (CsInt x1 : CsInt x2 : map CsDouble xs)


-- | Store a Chebyshev polynomial of the second kind.
--
-- > gen14 :: time * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen14 :: Double -> Int -> Int -> Int -> [Double] -> ScoBuilder ()
gen14 t sz x1 x2 xs = dyngen 14 t sz (CsInt x1 : CsInt x2 : map CsDouble xs)


-- | Generate two tables of stored polynomials.
--
-- > gen15 :: time * size * xint * xamp * [(partial_strength, phase)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen15 :: Double -> Int -> Int -> Int -> [(Double,Double)] -> ScoBuilder ()
gen15 t sz x1 x2 xs = dyngen 15 t sz (CsInt x1 : CsInt x2 : double2 xs)

--------------------------------------------------------------------------------
-- Amplitude scaling

-- | Generate a log of a Bessel function of the second kind.
--
-- > gen4 :: time * size * source_table * source_mode 
-- 
-- @size@ must be a power-of-2 plus 1.
--
gen4 :: Double -> Int -> Int -> Int -> ScoBuilder ()
gen4 t sz src src_mode = dyngen 4 t sz [CsInt src, CsInt src_mode]


-- | Generate a normalizing function.
--
-- > gen12 :: time * size * xint 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen12 :: Double -> Int -> Int -> ScoBuilder ()
gen12 t sz xint = dyngen 12 t sz [CsInt xint]

-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN12 :: Double -> Int -> Int -> ScoBuilder ()
genN12 t sz xint = dyngen (-12) t sz [CsInt xint]



--------------------------------------------------------------------------------

-- helpers 


int2 :: [(Int,Int)] -> [CsoundValue]
int2 = concatMap fn 
  where
    fn (a,b) = [CsInt a, CsInt b]


double2 :: [(Double,Double)] -> [CsoundValue]
double2 = concatMap fn 
  where
    fn (a,b) = [CsDouble a, CsDouble b]


double3 :: [(Double,Double,Double)] -> [CsoundValue]
double3 = concatMap fn 
  where
    fn (a,b,c) = [CsDouble a, CsDouble b, CsDouble c]

             
double4 :: [(Double,Double,Double,Double)] -> [CsoundValue]
double4 = concatMap fn 
  where
    fn (a,b,c,d) = [CsDouble a, CsDouble b, CsDouble c, CsDouble d]
