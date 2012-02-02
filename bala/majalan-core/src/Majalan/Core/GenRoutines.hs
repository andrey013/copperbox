{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.GenRoutines
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

module Majalan.Core.GenRoutines
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

import Majalan.Core.Score


--------------------------------------------------------------------------------
-- sine / cosine generators


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen9 :: inst_field_ref * size * [(partial_num, strength, inital_phase)]
-- 
gen9 :: Int -> Int -> [(Double,Double,Double)] -> GenStmt
gen9 ix sz xs = GenStmt ix sz 9 (double3 xs)


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
--
-- > gen10 :: inst_field_ref * size * [relative_strength]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen10 :: Int -> Int -> [Double] -> GenStmt
gen10 ix sz xs = GenStmt ix sz 10 (map D xs)


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > gen19 :: inst_field_ref * size * [(partial, strength, initial_phase, dc_offset)]
--
gen19 :: Int -> Int -> [(Double,Double,Double,Double)] -> GenStmt
gen19 ix sz xs = GenStmt ix sz 19 (double4 xs)

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
gen11 :: Int -> Int -> Int -> GenStmt
gen11 ix sz nh  = GenStmt ix sz 11 [ I nh ]


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
gen5 :: Int -> Int -> [(Double, Int) ] -> GenStmt
gen5 ix sz xs = GenStmt ix sz 5 (concatMap fn xs)
  where
    fn (d,i) = [D d, I i]


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
     -> [(Int,Double, Int,Double, Int,Double)] -> GenStmt
gen6 ix sz a xs = GenStmt ix sz 6 (D a : concatMap fn xs)
  where
    fn (n,b,n1,c,n2,d) = [ I n,  D b
                         , I n1, D c
                         , I n2, D d 
                         ]



-- | Construct a table of straight line segments.
-- 
-- > gen7 :: inst_field_ref * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
--
gen7 :: Int -> Int -> [(Double, Int)] -> GenStmt
gen7 ix sz xs = GenStmt ix sz 7 (concatMap fn xs)
  where
    fn (a,n) = [ D a, I n ] 



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
gen8 :: Int -> Int -> Double -> [(Int,Double)] -> GenStmt
gen8 ix sz a xs = GenStmt ix sz 8 (D a : concatMap fn xs)
  where
    fn (n,b) = [ I n, D b ] 


-- | Construct exponential curves in breakpoint fashion.
-- 
-- > gen25 :: inst_field_ref * size * table_loc * [(break_point, table_loc)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen25 :: Int -> Int -> Int -> [(Int,Int)] -> GenStmt
gen25 ix sz a xs = GenStmt ix sz 25 (I a : concatMap fn xs)
  where
    fn (n,b) = [ I n, I b ] 


-- | Construct straight lines in brreakpoint fashion.
-- 
-- > gen8 :: inst_field_ref * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen27 :: Int -> Int -> Int -> [(Int,Int)] -> GenStmt
gen27 ix sz a xs = GenStmt ix sz 27 (I a : concatMap fn xs)
  where
    fn (n,b) = [ I n, I b ] 


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
gen1 :: Int -> Int -> String -> Double -> Int -> Int -> GenStmt
gen1 ix sz fc skip fmt ch = 
    GenStmt ix sz 1 [S fc, D skip, I fmt, I ch]


-- | Read numeric values from a text file.
--
-- > gen23 :: inst_field_ref * size * file_name
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen23 :: Int -> Int -> String -> GenStmt
gen23 ix sz file_path = GenStmt ix sz 23 [S file_path]


-- | Read a time-tagged trajectory from a file.
--
-- > gen28 :: inst_field_ref * file_name 
-- 
-- Note - size is automatically set to 0 in the generated Csound
-- score. Csound itself handles the allocation size.
-- 
gen28 :: Int -> String -> GenStmt
gen28 ix file_name = GenStmt ix 0 28 [S file_name]


--------------------------------------------------------------------------------
-- Numeric value access

-- | Transfer data from immediate p-fields into a table.
--
-- > gen2 :: inst_field_ref * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen2 :: Int -> Int -> [Int] -> GenStmt
gen2 ix sz xs = GenStmt ix sz 2 (map I xs)

-- | /Negative/ version of gen2.
--
-- > genN2 :: inst_field_ref * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN2 :: Int -> Int -> [Int] -> GenStmt
genN2 ix sz xs = GenStmt ix sz (-2) (map I xs)


-- | Generate a step table from the supplied pairs.
--
-- > gen17 :: inst_field_ref * size * [(ordinate,y_value)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen17 :: Int -> Int -> [(Int,Int)] -> GenStmt
gen17 ix sz xs = GenStmt ix sz 17 (int2 xs)


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


gen20Opts :: Gen20Window -> Int -> [CsValue]
gen20Opts HAMMING         mx = [I 1, I mx]     
gen20Opts HANNING         mx = [I 2, I mx]     
gen20Opts BARTLETT        mx = [I 3, I mx]     
gen20Opts BLACKMAN        mx = [I 4, I mx]
gen20Opts BLACKMAN_HARRIS mx = [I 5, I mx]
gen20Opts (GAUSSIAN y)    mx = [I 6, I mx, I y]
gen20Opts (KAISER y)      mx = [I 7, I mx, I y]
gen20Opts RECTANGLE       mx = [I 8, I mx]
gen20Opts SYNC            mx = [I 9, I mx]


-- | Generate functions of different windows.
--
-- > gen20 :: inst_field_ref * size * window_type * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen20 :: Int -> Int -> Gen20Window -> Int -> GenStmt
gen20 ix sz wt mx = GenStmt ix sz 20 (gen20Opts wt mx)


-- | /Negative/ version of gen20.
--
-- The table is generated with a negative table number, which 
-- makes Csound rescale the maximum value - consult the Csound 
-- manual. e.g.:
-- 
-- > f1       0       1024    -20     2       456
--
genN20 :: Int -> Int -> Gen20Window -> Int -> GenStmt
genN20 ix sz wt mx = GenStmt ix sz (-20) (gen20Opts wt mx)

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

gen21Opts :: Gen21Dist -> Double -> [CsValue]
gen21Opts UNIFORM         lv = [I 1, D lv]
gen21Opts LINEAR          lv = [I 2, D lv]
gen21Opts TRIANGULAR      lv = [I 3, D lv]     
gen21Opts EXPON           lv = [I 4, D lv]     
gen21Opts BIEXPON         lv = [I 5, D lv]
gen21Opts DIST_GAUSSIAN   lv = [I 6, D lv]
gen21Opts CAUCHY          lv = [I 7, D lv]
gen21Opts POSITIVE_CAUCHY lv = [I 8, D lv]     
gen21Opts (BETA a b)      lv = [I 9, D lv, I a, I b]
gen21Opts (WEIBULL a)     lv = [I 10, D lv, I a]
gen21Opts POISSON         lv = [I 11, D lv]


-- | Generate tabels of random distributions.
--
-- > gen21 :: inst_field_ref * size * distribution_type * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen21 :: Int -> Int -> Gen21Dist -> Double -> GenStmt
gen21 ix sz dist lv = GenStmt ix sz 21 (gen21Opts dist lv)



--------------------------------------------------------------------------------
-- Waveshaping

-- | Generate a table by evaulating a polynomial.
--
-- > gen3 :: inst_field_ref * size * xval1 * xval2 * [coeffcients] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen3 :: Int -> Int -> Int -> Int -> [(Double,Double)] -> GenStmt
gen3 ix sz x1 x2 xs = GenStmt ix sz 3 (I x1 : I x2 : double2 xs)


-- | Store a Chebyshev polynomial of the first kind.
--
-- > gen13 :: inst_field_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen13 :: Int -> Int -> Int -> Int -> [Double] -> GenStmt
gen13 ix sz x1 x2 xs = 
    GenStmt ix sz 13 (I x1 : I x2 : map D xs)


-- | Store a Chebyshev polynomial of the second kind.
--
-- > gen14 :: inst_field_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen14 :: Int -> Int -> Int -> Int -> [Double] -> GenStmt
gen14 ix sz x1 x2 xs = 
    GenStmt ix sz 14 (I x1 : I x2 : map D xs)


-- | Generate two tables of stored polynomials.
--
-- > gen15 :: inst_field_ref * size * xint * xamp * [(partial_strength, phase)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen15 :: Int -> Int -> Int -> Int -> [(Double,Double)] -> GenStmt
gen15 ix sz x1 x2 xs = 
    GenStmt ix sz 15 (I x1 : I x2 : double2 xs)

--------------------------------------------------------------------------------
-- Amplitude scaling

-- | Generate a log of a Bessel function of the second kind.
--
-- > gen4 :: inst_field_ref * size * source_table * source_mode 
-- 
-- @size@ must be a power-of-2 plus 1.
--
gen4 :: Int -> Int -> Int -> Int -> GenStmt
gen4 ix sz src src_mode = 
    GenStmt ix sz 4 [I src, I src_mode]


-- | Generate a normalizing function.
--
-- > gen12 :: inst_field_ref * size * xint 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
gen12 :: Int -> Int -> Int -> GenStmt
gen12 ix sz xint = GenStmt ix sz 12 [I xint]

-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
genN12 :: Int -> Int -> Int -> GenStmt
genN12 ix sz xint = GenStmt ix sz (-12) [I xint]



--------------------------------------------------------------------------------

-- helpers 


int2 :: [(Int,Int)] -> [CsValue]
int2 = concatMap fn 
  where
    fn (a,b) = [I a, I b]


double2 :: [(Double,Double)] -> [CsValue]
double2 = concatMap fn 
  where
    fn (a,b) = [D a, D b]


double3 :: [(Double,Double,Double)] -> [CsValue]
double3 = concatMap fn 
  where
    fn (a,b,c) = [D a, D b, D c]

             
double4 :: [(Double,Double,Double,Double)] -> [CsValue]
double4 = concatMap fn 
  where
    fn (a,b,c,d) = [D a, D b, D c, D d]
