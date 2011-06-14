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
-- All note statemets have duration, even when it is irrelevent...
--
mkGen :: (CtxTempo ctx, InterpretUnit u) 
      => (OnsetDbl -> ScoBuilder ()) ->  ULocEvent ctx u
mkGen fn = promoteLoc $ \u -> 
    askCtx >>= \ctx -> 
    primEvent $ prim1 $ NoteStmt { onset_time = normalize (tempo ctx) u
                                 , event_dur  = 0
                                 , event_gen  = (\ot _ -> fn ot) }


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > fgen9 :: size * [(partial_num, strength, inital_phase)]
-- 
fgen9 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> [(Double,Double,Double)] -> ULocEvent ctx u
fgen9 sz ds = mkGen (\ot -> gen9 ot sz ds)
     


-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids. 
--
-- >  fgen10 :: size * [relative_strength]
--
fgen10 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> [Double] -> ULocEvent ctx u
fgen10 sz ds = mkGen (\ot -> gen10 ot sz ds)
     

-- | Generate composite waveforms from weighted sums of simple 
-- sinusoids.
-- 
-- > fgen19 :: time * size * [(partial_num, strength, inital_phase, dc_offset)]
--
fgen19 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> [(Double,Double,Double,Double)] -> ULocEvent ctx u
fgen19 sz ds = mkGen (\ot -> gen19 ot sz ds)



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
fgen11 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> ULocEvent ctx u
fgen11 sz nh = mkGen (\ot -> gen11 ot sz nh)



--------------------------------------------------------------------------------
-- Line / exponential segment generators


-- | Construct exponential curve table.
-- 
-- > fgen10 :: size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @ordinate_values@ cannot be zero, use 0.001 for a near zero 
-- number.
--
fgen5 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> [(Double, Int) ] -> ULocEvent ctx u
fgen5 sz xs = mkGen (\ot -> gen5 ot sz xs)


-- | Construct a table of cubic polynomial segments.
-- 
-- > fgen6 :: size * a -> [(n, b, n+1, c, n+2, d)]
-- 
-- @m0@ is the start maxima, sucessive curve segments are 
-- specified as ordinate_value and maxima tuples interspersed
-- with the number of stored values for each segement 
-- (n, n+1, n+2).  
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen6 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> Double -> [(Int,Double, Int,Double, Int,Double)] 
      -> ULocEvent ctx u
fgen6 sz a xs = mkGen (\ot -> gen6 ot sz a xs)



-- | Construct a table of straight line segments.
-- 
-- > fgen7 :: size * [(ordinate_value, length)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
--
fgen7 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> [(Double, Int)] -> ULocEvent ctx u
fgen7 sz xs = mkGen (\ot -> gen7 ot sz xs)



-- | Construct a table of cubic spine segments.
-- 
-- > fgen8 :: time * size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
-- @segment_length@ cannot be negative, though it can be zero.
-- The sum of segment lengths is usually expected to equal @size@.
--
-- Note - potentially the segment list should be three * two-tuples.
--
fgen8 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> Double -> [(Int,Double)] -> ULocEvent ctx u
fgen8 sz a xs = mkGen (\ot -> gen8 ot sz a xs) 


-- | Construct exponential curves in breakpoint fashion.
-- 
-- > fgen25 :: time * size * table_loc * [(break_point, table_loc)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen25 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> [(Int,Int)] -> ULocEvent ctx u
fgen25 sz a xs = mkGen (\ot -> gen25 ot sz a xs)


-- | Construct straight lines in brreakpoint fashion.
-- 
-- > fgen8 :: size * ordinate_value * [(segment_length, ordinate_value)]
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen27 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> [(Int,Int)] -> ULocEvent ctx u
fgen27 sz a xs = mkGen (\ot -> gen27 ot sz a xs) 


--------------------------------------------------------------------------------
-- File access

-- | Read data from a sound file into a table.
--
-- > fgen1 :: size * file_name * skip_time * format
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
fgen1 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> String -> Double -> Int -> ULocEvent ctx u
fgen1 sz fc skip fmt = mkGen (\ot -> gen1 ot sz fc skip fmt) 


-- | Read numeric values from a text file.
--
-- > fgen23 :: size * file_name
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen23 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> String -> ULocEvent ctx u
fgen23 sz file_path = mkGen (\ot -> gen23 ot sz file_path) 


-- | Read a time-tagged trajectory from a file.
--
-- > fgen28 :: time * file_name 
-- 
-- Note - size is automatically set to 0 in the generated Csound
-- score. Csound itself handles the allocation size.
-- 
fgen28 :: (CtxTempo ctx, InterpretUnit u) 
       => String -> ULocEvent ctx u
fgen28 file_name = mkGen (\ot -> gen28 ot file_name) 


--------------------------------------------------------------------------------
-- Waveshaping

-- | Generate a table by evaulating a polynomial.
--
-- > fgen3 :: size * xval1 * xval2 * [coeffcients] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen3 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> Int -> Int -> [(Double,Double)] -> ULocEvent ctx u
fgen3 sz x1 x2 xs = mkGen (\ot -> gen3 ot sz x1 x2 xs)


-- | Store a Chebyshev polynomial of the first kind.
--
-- > fgen13 :: size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen13 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> Int -> [Double] -> ULocEvent ctx u
fgen13 sz x1 x2 xs = mkGen (\ot -> gen13 ot sz x1 x2 xs)


-- | Store a Chebyshev polynomial of the second kind.
--
-- > fgen14 :: size * xint * xamp * [partial_strength] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen14 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> Int -> [Double] -> ULocEvent ctx u
fgen14 sz x1 x2 xs = mkGen (\ot -> gen14 ot sz x1 x2 xs)


-- | Generate two tables of stored polynomials.
--
-- > fgen15 :: size * xint * xamp * [(partial_strength, phase)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen15 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> Int -> [(Double,Double)] -> ULocEvent ctx u
fgen15 sz x1 x2 xs = mkGen (\ot -> gen15 ot sz x1 x2 xs)


--------------------------------------------------------------------------------
-- Numeric value access

-- | Transfer data from immediate p-fields into a table.
--
-- > fgen2 :: size * [value] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen2 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> [Int] -> ULocEvent ctx u
fgen2 sz xs = mkGen (\ot -> gen2 ot sz xs) 



-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgenN2 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> [Int] -> ULocEvent ctx u
fgenN2 sz xs = mkGen (\ot -> genN2 ot sz xs) 


-- | Generate a step table from the supplied pairs.
--
-- > fgen17 :: size * [(ordinate,y_value)] 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen17 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> [(Int,Int)] -> ULocEvent ctx u
fgen17 sz xs = mkGen (\ot -> gen17 ot sz xs) 


--------------------------------------------------------------------------------
-- Window generation

-- | Generate a Hamming window.
--
-- > fgen20_hamming :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_hamming :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Int -> ULocEvent ctx u
fgen20_hamming sz mx = mkGen (\ot -> gen20 ot sz HAMMING mx) 


-- | Generate a Hanning window.
--
-- > fgen20_hanning :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_hanning :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Int -> ULocEvent ctx u
fgen20_hanning sz mx = mkGen (\ot -> gen20 ot sz HANNING mx) 


-- | Generate a Bartlett window.
--
-- > fgen20_bartlett :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_bartlett :: (CtxTempo ctx, InterpretUnit u) 
                => Int -> Int -> ULocEvent ctx u
fgen20_bartlett sz mx = mkGen (\ot -> gen20 ot sz BARTLETT mx) 

-- | Generate a Blackman window.
--
-- > fgen20_blackman :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_blackman :: (CtxTempo ctx, InterpretUnit u) 
                => Int -> Int -> ULocEvent ctx u
fgen20_blackman sz mx = mkGen (\ot -> gen20 ot sz BLACKMAN mx) 


-- | Generate a Blackman-Harris window.
--
-- > fgen20_blackman_harris :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_blackman_harris :: (CtxTempo ctx, InterpretUnit u) 
                       => Int -> Int -> ULocEvent ctx u
fgen20_blackman_harris sz mx = mkGen (\ot -> gen20 ot sz BLACKMAN_HARRIS mx) 


-- | Generate a Gaussian window.
--
-- > fgen20_gaussian :: size * maximum * openness
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_gaussian :: (CtxTempo ctx, InterpretUnit u) 
                => Int -> Int -> Int -> ULocEvent ctx u
fgen20_gaussian sz mx opn = mkGen (\ot -> gen20 ot sz (GAUSSIAN opn) mx) 

-- | Generate a Kaiser window.
--
-- > fgen20_kaiser :: size * maximum * broadness
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_kaiser :: (CtxTempo ctx, InterpretUnit u) 
              => Int -> Int -> Int -> ULocEvent ctx u
fgen20_kaiser sz mx broad = mkGen (\ot -> gen20 ot sz (KAISER broad) mx) 


-- | Generate a rectangle window.
--
-- > fgen20_rectangle :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_rectangle :: (CtxTempo ctx, InterpretUnit u) 
                 => Int -> Int -> ULocEvent ctx u
fgen20_rectangle sz mx = mkGen (\ot -> gen20 ot sz RECTANGLE mx) 


-- | Generate a sync window.
--
-- > fgen20_sync :: size * maximum
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen20_sync :: (CtxTempo ctx, InterpretUnit u) 
                 => Int -> Int -> ULocEvent ctx u
fgen20_sync sz mx = mkGen (\ot -> gen20 ot sz SYNC mx) 

--------------------------------------------------------------------------------
-- Random functions


-- | Generate a uniform random distribution.
--
-- > fgen21_uniform :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_uniform :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Double -> ULocEvent ctx u
fgen21_uniform sz lv = mkGen (\ot -> gen21 ot sz UNIFORM lv) 


-- | Generate a uniform linear distribution.
--
-- > fgen21_linear :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_linear :: (CtxTempo ctx, InterpretUnit u) 
              => Int -> Double -> ULocEvent ctx u
fgen21_linear sz lv = mkGen (\ot -> gen21 ot sz LINEAR lv) 


-- | Generate a triangular distribution.
--
-- > fgen21_triangular :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_triangular :: (CtxTempo ctx, InterpretUnit u) 
                  => Int -> Double -> ULocEvent ctx u
fgen21_triangular sz lv = mkGen (\ot -> gen21 ot sz TRIANGULAR lv) 


-- | Generate an exponential distribution.
--
-- > fgen21_expon :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_expon :: (CtxTempo ctx, InterpretUnit u) 
                   => Int -> Double -> ULocEvent ctx u
fgen21_expon sz lv = mkGen (\ot -> gen21 ot sz EXPON lv) 


-- | Generate a biexponential distribution.
--
-- > fgen21_biexpon :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_biexpon :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Double -> ULocEvent ctx u
fgen21_biexpon sz lv = mkGen (\ot -> gen21 ot sz EXPON lv) 


-- | Generate a Gaussian distribution.
--
-- > fgen21_gaussian :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_gaussian :: (CtxTempo ctx, InterpretUnit u) 
                => Int -> Double -> ULocEvent ctx u
fgen21_gaussian sz lv = mkGen (\ot -> gen21 ot sz DIST_GAUSSIAN lv) 


-- | Generate a Cauchy distribution.
--
-- > fgen21_cauchy :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_cauchy :: (CtxTempo ctx, InterpretUnit u) 
              => Int -> Double -> ULocEvent ctx u
fgen21_cauchy sz lv = mkGen (\ot -> gen21 ot sz CAUCHY lv) 


-- | Generate a positive Cauchy distribution.
--
-- > fgen21_positive_cauchy :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_positive_cauchy :: (CtxTempo ctx, InterpretUnit u) 
                       => Int -> Double -> ULocEvent ctx u
fgen21_positive_cauchy sz lv = mkGen (\ot -> gen21 ot sz POSITIVE_CAUCHY lv) 

-- | Generate a beta distribution.
--
-- > fgen21_beta :: size * level * a * b
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_beta :: (CtxTempo ctx, InterpretUnit u) 
              => Int -> Double -> Int -> Int -> ULocEvent ctx u
fgen21_beta sz lv a b = mkGen (\ot -> gen21 ot sz (BETA a b) lv) 


-- | Generate a Weibull distribution.
--
-- > fgen21_weibull :: size * level * a
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_weibull :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Double -> Int -> ULocEvent ctx u
fgen21_weibull sz lv a = mkGen (\ot -> gen21 ot sz (WEIBULL a) lv) 


-- | Generate a poisson distribution.
--
-- > fgen21_poisson :: size * level
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen21_poisson :: (CtxTempo ctx, InterpretUnit u) 
               => Int -> Double -> ULocEvent ctx u
fgen21_poisson sz lv = mkGen (\ot -> gen21 ot sz POISSON lv) 

--------------------------------------------------------------------------------
-- Amplitude scaling

-- | Generate a log of a Bessel function of the second kind.
--
-- > gen4 :: size * source_table * source_mode 
-- 
-- @size@ must be a power-of-2 plus 1.
--
fgen4 :: (CtxTempo ctx, InterpretUnit u) 
      => Int -> Int -> Int -> ULocEvent ctx u
fgen4 sz src src_mode = mkGen (\ot -> gen4 ot sz src src_mode)


-- | Generate a normalizing function.
--
-- > fgen12 :: size * xint 
-- 
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgen12 :: (CtxTempo ctx, InterpretUnit u) 
       => Int -> Int -> ULocEvent ctx u
fgen12 sz xint = mkGen (\ot -> gen12 ot sz xint)

-- | Negative version of 'gen12'.
--
-- @size@ must be a power of 2 or power-of-2 plus 1.
--
fgenN12 :: (CtxTempo ctx, InterpretUnit u) 
        => Int -> Int -> ULocEvent ctx u
fgenN12 sz xint = mkGen (\ot -> genN12 ot sz xint)