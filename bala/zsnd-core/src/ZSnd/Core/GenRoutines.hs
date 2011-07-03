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

import ZSnd.Core.ScoreInternal



--------------------------------------------------------------------------------
-- sine / cosine generators


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen9 :: inst_field_ref * size * [(partial_num, strength, inital_phase)]
-- 
gen9 :: Int -> Int -> [(Double,Double,Double)] -> GenStmtProps
gen9 ix sz xs = GenStmtProps ix sz 9 (double3 xs)


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
--
-- > gen10 :: inst_field_ref * size * [relative_strength]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen10 :: Int -> Int -> [Double] -> GenStmtProps
gen10 ix sz xs = GenStmtProps ix sz 10 (map CsDouble xs)


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen19 :: inst_field_ref * size * [(partial, strength, initial_phase, dc_offset)]
--
gen19 :: Int -> Int -> [(Double,Double,Double,Double)] -> GenStmtProps
gen19 ix sz xs = GenStmtProps ix sz 19 (double4 xs)

-- | Generate additive set of cosine partials.
-- 
-- > gen11 :: inst_field_ref * size * num_harmonics *
--
-- @num_harmonics@ must be positive.
--
-- Note - the corresponding Csound gen also allows optional 
-- lowest harmonic partial and multiplier.
-- 
-- ZSnd needs extending to handle this optional cases... 
--
gen11 :: Int -> Int -> Int -> GenStmtProps
gen11 ix sz nh  = GenStmtProps ix sz 11 [ CsInt nh ]


--------------------------------------------------------------------------------
-- Line / exponential segment generators


-- | Construct exponential curve table.
-- 
-- > gen10 :: inst_field_ref * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @ordinate_values@ cannot be zero, use 0.001 for a near zero 
-- number.
--
gen5 :: Int -> Int -> [(Double, Int) ] -> GenStmtProps
gen5 ix sz xs = GenStmtProps ix sz 5 (concatMap fn xs)
  where
    fn (d,i) = [CsDouble d, CsInt i]


-- | Construct a table of cubic polynomial segments.
-- 
-- > gen6 :: inst_field_ref * size * a -> [(n, b, n+1, c, n+2, d)]
-- 
-- @m0@ is the start maxima, sucessive curve segments are 
-- specified as ordinate_value and maxima tuples interspersed
-- with the number of stored values for each segement 
-- (n, n+1, n+2).  
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen6 :: Int -> Int -> Double 
     -> [(Int,Double, Int,Double, Int,Double)] -> GenStmtProps
gen6 ix sz a xs = GenStmtProps ix sz 6 (CsDouble a : concatMap fn xs)
  where
    fn (n,b,n1,c,n2,d) = [ CsInt n,  CsDouble b
                         , CsInt n1, CsDouble c
                         , CsInt n2, CsDouble d 
                         ]



-- | Construct a table of straight line segments.
-- 
-- > gen7 :: inst_field_ref * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
--
gen7 :: Int -> Int -> [(Double, Int)] -> GenStmtProps
gen7 ix sz xs = GenStmtProps ix sz 7 (concatMap fn xs)
  where
    fn (a,n) = [ CsDouble a, CsInt n ] 



-- | Construct a table of cubic spine segments.
-- 
-- > gen8 :: inst_field_ref * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
-- The sum of segment lengths is usually expected to equal @size@.
--
-- Note - potentially the segment list should be three * two-tuples.
--
gen8 :: Int -> Int -> Double -> [(Int,Double)] -> GenStmtProps
gen8 ix sz a xs = GenStmtProps ix sz 8 (CsDouble a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsDouble b ] 


-- | Construct exponential curves in breakpoint fashion.
-- 
-- > gen25 :: inst_field_ref * size * table_loc * [(break_point, table_loc)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen25 :: Int -> Int -> Int -> [(Int,Int)] -> GenStmtProps
gen25 ix sz a xs = GenStmtProps ix sz 25 (CsInt a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsInt b ] 


-- | Construct straight lines in brreakpoint fashion.
-- 
-- > gen8 :: inst_field_ref * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen27 :: Int -> Int -> Int -> [(Int,Int)] -> GenStmtProps
gen27 ix sz a xs = GenStmtProps ix sz 27 (CsInt a : concatMap fn xs)
  where
    fn (n,b) = [ CsInt n, CsInt b ] 


--------------------------------------------------------------------------------
-- File access

-- | Read data from a sound file into a table.
--
-- > gen1 :: inst_field_ref * size * file_name * skip_time * format * channel
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
gen1 :: Int -> Int -> String -> Double -> Int -> Int -> GenStmtProps
gen1 ix sz fc skip fmt ch = 
    GenStmtProps ix sz 1 [CsString fc, CsDouble skip, CsInt fmt, CsInt ch]


-- | Read numeric values from a text file.
--
-- > gen23 :: inst_field_ref * size * file_name
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen23 :: Int -> Int -> String -> GenStmtProps
gen23 ix sz file_path = GenStmtProps ix sz 23 [CsString file_path]


-- | Read a time-tagged trajectory from a file.
--
-- > gen28 :: inst_field_ref * file_name 
-- 
-- Note - size is automatically set to 0 in the generated Csound
-- score. Csound itself handles the allocation size.
-- 
gen28 :: Int -> String -> GenStmtProps
gen28 ix file_name = GenStmtProps ix 0 28 [CsString file_name]


--------------------------------------------------------------------------------
-- Numeric value access

-- | Transfer data from immediate p-fields into a table.
--
-- > gen2 :: inst_field_ref * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen2 :: Int -> Int -> [Int] -> GenStmtProps
gen2 ix sz xs = GenStmtProps ix sz 2 (map CsInt xs)

-- | /Negative/ version of gen2.
--
-- > genN2 :: inst_field_ref * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN2 :: Int -> Int -> [Int] -> GenStmtProps
genN2 ix sz xs = GenStmtProps ix sz (-2) (map CsInt xs)


-- | Generate a step table from the supplied pairs.
--
-- > gen17 :: inst_field_ref * size * [(ordinate,y_value)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen17 :: Int -> Int -> [(Int,Int)] -> GenStmtProps
gen17 ix sz xs = GenStmtProps ix sz 17 (int2 xs)


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
-- > gen20 :: inst_field_ref * size * window_type * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen20 :: Int -> Int -> Gen20Window -> Int -> GenStmtProps
gen20 ix sz wt mx = GenStmtProps ix sz 20 (gen20Opts wt mx)


-- | /Negative/ version of gen20.
--
-- The table is generated with a negative table number, which 
-- makes Csound rescale the maximum value - consult the Csound 
-- manual. e.g.:
-- 
-- > f1       0       1024    -20     2       456
--
genN20 :: Int -> Int -> Gen20Window -> Int -> GenStmtProps
genN20 ix sz wt mx = GenStmtProps ix sz (-20) (gen20Opts wt mx)

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
-- > gen21 :: inst_field_ref * size * distribution_type * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen21 :: Int -> Int -> Gen21Dist -> Double -> GenStmtProps
gen21 ix sz dist lv = GenStmtProps ix sz 21 (gen21Opts dist lv)



--------------------------------------------------------------------------------
-- Waveshaping

-- | Generate a table by evaulating a polynomial.
--
-- > gen3 :: inst_field_ref * size * xval1 * xval2 * [coeffcients] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen3 :: Int -> Int -> Int -> Int -> [(Double,Double)] -> GenStmtProps
gen3 ix sz x1 x2 xs = GenStmtProps ix sz 3 (CsInt x1 : CsInt x2 : double2 xs)


-- | Store a Chebyshev polynomial of the first kind.
--
-- > gen13 :: inst_field_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen13 :: Int -> Int -> Int -> Int -> [Double] -> GenStmtProps
gen13 ix sz x1 x2 xs = 
    GenStmtProps ix sz 13 (CsInt x1 : CsInt x2 : map CsDouble xs)


-- | Store a Chebyshev polynomial of the second kind.
--
-- > gen14 :: inst_field_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen14 :: Int -> Int -> Int -> Int -> [Double] -> GenStmtProps
gen14 ix sz x1 x2 xs = 
    GenStmtProps ix sz 14 (CsInt x1 : CsInt x2 : map CsDouble xs)


-- | Generate two tables of stored polynomials.
--
-- > gen15 :: inst_field_ref * size * xint * xamp * [(partial_strength, phase)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen15 :: Int -> Int -> Int -> Int -> [(Double,Double)] -> GenStmtProps
gen15 ix sz x1 x2 xs = 
    GenStmtProps ix sz 15 (CsInt x1 : CsInt x2 : double2 xs)

--------------------------------------------------------------------------------
-- Amplitude scaling

-- | Generate a log of a Bessel function of the second kind.
--
-- > gen4 :: inst_field_ref * size * source_table * source_mode 
-- 
-- @size@ must be a power-of-2 plus 1.
--
gen4 :: Int -> Int -> Int -> Int -> GenStmtProps
gen4 ix sz src src_mode = 
    GenStmtProps ix sz 4 [CsInt src, CsInt src_mode]


-- | Generate a normalizing function.
--
-- > gen12 :: inst_field_ref * size * xint 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen12 :: Int -> Int -> Int -> GenStmtProps
gen12 ix sz xint = GenStmtProps ix sz 12 [CsInt xint]

-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN12 :: Int -> Int -> Int -> GenStmtProps
genN12 ix sz xint = GenStmtProps ix sz (-12) [CsInt xint]



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
