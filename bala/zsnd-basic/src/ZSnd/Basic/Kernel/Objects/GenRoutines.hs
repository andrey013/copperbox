{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Basic.Kernel.Objects.GenRoutines
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- ...
--
--------------------------------------------------------------------------------

module ZSnd.Basic.Kernel.Objects.GenRoutines
   (

  -- * Sine cosine generators
    fgen9
  , fgen10
  , fgen19
  , fgen11

  -- * Line / exponential segment generators
  , fgen5
  , fgen6
  , fgen7
  , fgen8
  , fgen25
  , fgen27

  -- * File access 
  , fgen1
  , fgen23
  , fgen28

  -- * Numeric value access
  , fgen2
  , fgenN2
  , fgen17


  -- * Window generation
  , fgen20_hamming
  , fgen20_hanning
  , fgen20_bartlett
  , fgen20_blackman
  , fgen20_blackman_harris
  , fgen20_gaussian
  , fgen20_kaiser
  , fgen20_rectangle
  , fgen20_sync

  -- * Random functions
  , fgen21_uniform
  , fgen21_linear
  , fgen21_triangular
  , fgen21_expon
  , fgen21_biexpon
  , fgen21_gaussian
  , fgen21_cauchy
  , fgen21_positive_cauchy
  , fgen21_beta
  , fgen21_weibull
  , fgen21_poisson

  -- * Waveshaping
  , fgen3
  , fgen13
  , fgen14
  , fgen15

  -- * Amplitude scaling
  , fgen4
  , fgen12
  , fgenN12

  )

  where



import ZSnd.Basic.Kernel.Base.BaseDefs
import ZSnd.Basic.Kernel.Base.Context
import ZSnd.Basic.Kernel.Base.WrappedPrimitive
import ZSnd.Basic.Kernel.Objects.Basis
import ZSnd.Basic.Kernel.Objects.LocEvent

import ZSnd.Core                                -- package: zsnd-core


-- | Helper.
--
mkGen :: InterpretUnit u
      => GenStmtProps ->  ULocEvent ctx u
mkGen props = promoteLoc $ \u -> 
    normalizeCtx u >>= \du -> 
    primEvent $ prim1 $ absTableGen du props


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > fgen9 :: inst_ref * size * [(partial_num, strength, inital_phase)]
-- 
fgen9 :: InterpretUnit u
      => Int -> Int -> [(Double,Double,Double)] -> ULocEvent ctx u
fgen9 ix sz ds = mkGen (gen9 ix sz ds)
     


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids. 
--
-- >  fgen10 :: inst_ref * size * [relative_strength]
--
fgen10 :: InterpretUnit u 
       => Int -> Int -> [Double] -> ULocEvent ctx u
fgen10 ix sz ds = mkGen (gen10 ix sz ds)
     

-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > fgen19 :: inst_ref * size * [(partial_num, strength, inital_phase, dc_offset)]
--
fgen19 :: InterpretUnit u 
       => Int -> Int -> [(Double,Double,Double,Double)] -> ULocEvent ctx u
fgen19 ix sz ds = mkGen (gen19 ix sz ds)



-- | Generate additive set of cosine partials.
-- 
-- > gen11 :: inst_ref * size * num_harmonics *
--
-- @num_harmonics@ must be positive.
--
-- Note - the corresponding Csound gen also allows optional 
-- lowest harmonic partial and multiplier.
-- 
-- ZSnd needs extending to handle this optional cases... 
--
fgen11 :: InterpretUnit u 
       => Int -> Int -> Int -> ULocEvent ctx u
fgen11 ix sz nh = mkGen (gen11 ix sz nh)



--------------------------------------------------------------------------------
-- Line / exponential segment generators


-- | Construct exponential curve table.
-- 
-- > fgen10 :: inst_ref * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @ordinate_values@ cannot be zero, use 0.001 for a near zero 
-- number.
--
fgen5 :: InterpretUnit u 
      => Int -> Int -> [(Double, Int) ] -> ULocEvent ctx u
fgen5 ix sz xs = mkGen (gen5 ix sz xs)


-- | Construct a table of cubic polynomial segments.
-- 
-- > fgen6 :: inst_ref * size * a -> [(n, b, n+1, c, n+2, d)]
-- 
-- @m0@ is the start maxima, sucessive curve segments are 
-- specified as ordinate_value and maxima tuples interspersed
-- with the number of stored values for each segement 
-- (n, n+1, n+2).  
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen6 :: InterpretUnit u 
      => Int -> Int -> Double -> [(Int,Double, Int,Double, Int,Double)] 
      -> ULocEvent ctx u
fgen6 ix sz a xs = mkGen (gen6 ix sz a xs)



-- | Construct a table of straight line segments.
-- 
-- > fgen7 :: inst_ref * size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
--
fgen7 :: InterpretUnit u 
      => Int -> Int -> [(Double, Int)] -> ULocEvent ctx u
fgen7 ix sz xs = mkGen (gen7 ix sz xs)



-- | Construct a table of cubic spine segments.
-- 
-- > fgen8 :: inst_ref * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
-- The sum of segment lengths is usually expected to equal @size@.
--
-- Note - potentially the segment list should be three * two-tuples.
--
fgen8 :: InterpretUnit u 
      => Int -> Int -> Double -> [(Int,Double)] -> ULocEvent ctx u
fgen8 ix sz a xs = mkGen (gen8 ix sz a xs) 


-- | Construct exponential curves in breakpoint fashion.
-- 
-- > fgen25 :: inst_ref * size * table_loc * [(break_point, table_loc)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen25 :: InterpretUnit u 
       => Int -> Int -> Int -> [(Int,Int)] -> ULocEvent ctx u
fgen25 ix sz a xs = mkGen (gen25 ix sz a xs)


-- | Construct straight lines in brreakpoint fashion.
-- 
-- > fgen8 :: inst_ref * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen27 :: InterpretUnit u 
       => Int -> Int -> Int -> [(Int,Int)] -> ULocEvent ctx u
fgen27 ix sz a xs = mkGen (gen27 ix sz a xs) 


--------------------------------------------------------------------------------
-- File access

-- | Read data from a sound file into a table.
--
-- > fgen1 :: inst_ref * size * file_name * skip_time * format * channel
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
fgen1 :: InterpretUnit u 
      => Int -> Int -> String -> Double -> Int -> Int -> ULocEvent ctx u
fgen1 ix sz fc skip fmt ch = mkGen (gen1 ix sz fc skip fmt ch) 


-- | Read numeric values from a text file.
--
-- > fgen23 :: inst_ref * size * file_name
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen23 :: InterpretUnit u 
       => Int -> Int -> String -> ULocEvent ctx u
fgen23 ix sz file_path = mkGen (gen23 ix sz file_path) 


-- | Read a time-tagged trajectory from a file.
--
-- > fgen28 :: inst_ref * size *  file_name 
-- 
-- Note - size is automatically set to 0 in the generated Csound
-- score. Csound itself handles the allocation size.
-- 
fgen28 :: InterpretUnit u 
       => Int -> String -> ULocEvent ctx u
fgen28 ix file_name = mkGen (gen28 ix file_name) 


--------------------------------------------------------------------------------
-- Waveshaping

-- | Generate a table by evaulating a polynomial.
--
-- > fgen3 :: inst_ref * size * xval1 * xval2 * [coeffcients] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen3 :: InterpretUnit u 
      => Int -> Int -> Int -> Int -> [(Double,Double)] -> ULocEvent ctx u
fgen3 ix sz x1 x2 xs = mkGen (gen3 ix sz x1 x2 xs)


-- | Store a Chebyshev polynomial of the first kind.
--
-- > fgen13 :: inst_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen13 :: InterpretUnit u 
       => Int -> Int -> Int -> Int -> [Double] -> ULocEvent ctx u
fgen13 ix sz x1 x2 xs = mkGen (gen13 ix sz x1 x2 xs)


-- | Store a Chebyshev polynomial of the second kind.
--
-- > fgen14 :: inst_ref * size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen14 :: InterpretUnit u 
       => Int -> Int -> Int -> Int -> [Double] -> ULocEvent ctx u
fgen14 ix sz x1 x2 xs = mkGen (gen14 ix sz x1 x2 xs)


-- | Generate two tables of stored polynomials.
--
-- > fgen15 :: inst_ref * size * xint * xamp * [(partial_strength, phase)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen15 :: InterpretUnit u 
       => Int -> Int -> Int -> Int -> [(Double,Double)] -> ULocEvent ctx u
fgen15 ix sz x1 x2 xs = mkGen (gen15 ix sz x1 x2 xs)


--------------------------------------------------------------------------------
-- Numeric value access

-- | Transfer data from immediate p-fields into a table.
--
-- > fgen2 :: inst_ref * size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen2 :: InterpretUnit u 
      => Int -> Int -> [Int] -> ULocEvent ctx u
fgen2 ix sz xs = mkGen (gen2 ix sz xs) 



-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgenN2 :: InterpretUnit u 
       => Int -> Int -> [Int] -> ULocEvent ctx u
fgenN2 ix sz xs = mkGen (genN2 ix sz xs) 


-- | Generate a step table from the supplied pairs.
--
-- > fgen17 :: inst_ref * size * [(ordinate,y_value)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen17 :: InterpretUnit u 
       => Int -> Int -> [(Int,Int)] -> ULocEvent ctx u
fgen17 ix sz xs = mkGen (gen17 ix sz xs) 


--------------------------------------------------------------------------------
-- Window generation

-- | Generate a Hamming window.
--
-- > fgen20_hamming :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_hamming :: InterpretUnit u 
               => Int -> Int -> Int -> ULocEvent ctx u
fgen20_hamming ix sz mx = mkGen (gen20 ix sz HAMMING mx) 


-- | Generate a Hanning window.
--
-- > fgen20_hanning :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_hanning :: InterpretUnit u 
               => Int -> Int -> Int -> ULocEvent ctx u
fgen20_hanning ix sz mx = mkGen (gen20 ix sz HANNING mx) 


-- | Generate a Bartlett window.
--
-- > fgen20_bartlett :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_bartlett :: InterpretUnit u 
                => Int -> Int -> Int -> ULocEvent ctx u
fgen20_bartlett ix sz mx = mkGen (gen20 ix sz BARTLETT mx) 

-- | Generate a Blackman window.
--
-- > fgen20_blackman :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_blackman :: InterpretUnit u 
                => Int -> Int -> Int -> ULocEvent ctx u
fgen20_blackman ix sz mx = mkGen (gen20 ix sz BLACKMAN mx) 


-- | Generate a Blackman-Harris window.
--
-- > fgen20_blackman_harris :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_blackman_harris :: InterpretUnit u 
                       => Int -> Int -> Int -> ULocEvent ctx u
fgen20_blackman_harris ix sz mx = mkGen (gen20 ix sz BLACKMAN_HARRIS mx) 


-- | Generate a Gaussian window.
--
-- > fgen20_gaussian :: inst_ref * size * maximum * openness
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_gaussian :: InterpretUnit u 
                => Int -> Int -> Int -> Int -> ULocEvent ctx u
fgen20_gaussian ix sz mx opn = mkGen (gen20 ix sz (GAUSSIAN opn) mx) 

-- | Generate a Kaiser window.
--
-- > fgen20_kaiser :: inst_ref * size * maximum * broadness
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_kaiser :: InterpretUnit u 
              => Int -> Int -> Int -> Int -> ULocEvent ctx u
fgen20_kaiser ix sz mx broad = mkGen (gen20 ix sz (KAISER broad) mx) 


-- | Generate a rectangle window.
--
-- > fgen20_rectangle :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_rectangle :: InterpretUnit u 
                 => Int -> Int -> Int -> ULocEvent ctx u
fgen20_rectangle ix sz mx = mkGen (gen20 ix sz RECTANGLE mx) 


-- | Generate a sync window.
--
-- > fgen20_sync :: inst_ref * size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_sync :: InterpretUnit u 
            => Int -> Int -> Int -> ULocEvent ctx u
fgen20_sync ix sz mx = mkGen (gen20 ix sz SYNC mx) 

--------------------------------------------------------------------------------
-- Random functions


-- | Generate a uniform random distribution.
--
-- > fgen21_uniform :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_uniform :: InterpretUnit u 
               => Int -> Int -> Double -> ULocEvent ctx u
fgen21_uniform ix sz lv = mkGen (gen21 ix sz UNIFORM lv) 


-- | Generate a uniform linear distribution.
--
-- > fgen21_linear :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_linear :: InterpretUnit u 
              => Int -> Int -> Double -> ULocEvent ctx u
fgen21_linear ix sz lv = mkGen (gen21 ix sz LINEAR lv) 


-- | Generate a triangular distribution.
--
-- > fgen21_triangular :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_triangular :: InterpretUnit u 
                  => Int -> Int -> Double -> ULocEvent ctx u
fgen21_triangular ix sz lv = mkGen (gen21 ix sz TRIANGULAR lv) 


-- | Generate an exponential distribution.
--
-- > fgen21_expon :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_expon :: InterpretUnit u 
             => Int -> Int -> Double -> ULocEvent ctx u
fgen21_expon ix sz lv = mkGen (gen21 ix sz EXPON lv) 


-- | Generate a biexponential distribution.
--
-- > fgen21_biexpon :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_biexpon :: InterpretUnit u 
               => Int -> Int -> Double -> ULocEvent ctx u
fgen21_biexpon ix sz lv = mkGen (gen21 ix sz EXPON lv) 


-- | Generate a Gaussian distribution.
--
-- > fgen21_gaussian :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_gaussian :: InterpretUnit u 
                => Int -> Int -> Double -> ULocEvent ctx u
fgen21_gaussian ix sz lv = mkGen (gen21 ix sz DIST_GAUSSIAN lv) 


-- | Generate a Cauchy distribution.
--
-- > fgen21_cauchy :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_cauchy :: InterpretUnit u 
              => Int -> Int -> Double -> ULocEvent ctx u
fgen21_cauchy ix sz lv = mkGen (gen21 ix sz CAUCHY lv) 


-- | Generate a positive Cauchy distribution.
--
-- > fgen21_positive_cauchy :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_positive_cauchy :: InterpretUnit u 
                       => Int -> Int -> Double -> ULocEvent ctx u
fgen21_positive_cauchy ix sz lv = mkGen (gen21 ix sz POSITIVE_CAUCHY lv)

-- | Generate a beta distribution.
--
-- > fgen21_beta :: inst_ref * size * level * a * b
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_beta :: InterpretUnit u 
            => Int -> Int -> Double -> Int -> Int -> ULocEvent ctx u
fgen21_beta ix sz lv a b = mkGen (gen21 ix sz (BETA a b) lv) 


-- | Generate a Weibull distribution.
--
-- > fgen21_weibull :: inst_ref * size * level * a
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_weibull :: InterpretUnit u 
               => Int -> Int -> Double -> Int -> ULocEvent ctx u
fgen21_weibull ix sz lv a = mkGen (gen21 ix sz (WEIBULL a) lv) 


-- | Generate a poisson distribution.
--
-- > fgen21_poisson :: inst_ref * size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_poisson :: InterpretUnit u 
               => Int -> Int -> Double -> ULocEvent ctx u
fgen21_poisson ix sz lv = mkGen (gen21 ix sz POISSON lv) 

--------------------------------------------------------------------------------
-- Amplitude scaling

-- | Generate a log of a Bessel function of the second kind.
--
-- > gen4 :: inst_ref * size * source_table * source_mode 
-- 
-- @size@ must be a power-of-2 plus 1.
--
fgen4 :: InterpretUnit u 
      => Int -> Int -> Int -> Int -> ULocEvent ctx u
fgen4 ix sz src src_mode = mkGen (gen4 ix sz src src_mode)


-- | Generate a normalizing function.
--
-- > fgen12 :: inst_ref * size * xint 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen12 :: InterpretUnit u 
       => Int -> Int -> Int -> ULocEvent ctx u
fgen12 ix sz xint = mkGen (gen12 ix sz xint)

-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgenN12 :: InterpretUnit u 
        => Int -> Int -> Int -> ULocEvent ctx u
fgenN12 ix sz xint = mkGen (genN12 ix sz xint)