


module Data.Stream.StreamCalculus 
  (
    (<:)
  , (<<)
  , const

  , inverse
  , cX
  , powX
  , polynomial 
  , (<#>)
  , sinverse
  , expos
  , deriv
  , blam
  , (<.>)
  ) where

import Data.Stream

import Prelude hiding ( head, tail, const, repeat, map, zip, zipWith )

--------------------------------------------------------------------------------
-- Some of the shorthand from Data.Stream.Hinze.Stream


-- | Cons for streams
infixr 5 <:
(<:)    ::  a -> Stream a -> Stream a
a <: s  =   Cons a s


infixr 5 <<
-- | Prepend a list to a stream
(<<)            ::  [a] -> Stream a -> Stream a
[]        << s  =   s
(a : as)  << s  =   a <: (as << s)


--------------------------------------------------------------------------------

--

-- stream addition (which is a zip)
(#+) :: Num a => Stream a -> Stream a -> Stream a 
fs #+ gs = (head fs) + (head gs) <:> (tail fs) #+ (tail gs)


-- convolution product
(#*) :: Num a => Stream a -> Stream a -> Stream a 
fs #* gs = f*g <:> (ft #* gs) #+ ((const f) #* gt) 
  where
    f  = head fs
    ft = tail fs
    g  = head gs
    gt = tail gs

-- | @const@ taken to streams - @const r == [r,0,0,0,..]@
const :: Num a => a -> Stream a
const r = r <:> repeat 0


-- | Num instance for streams where multiplication is the
-- convolution product
instance (Num a) => Num (Stream a) where
   (+)          =  (#+)
   (-)          =  zipWith (-)
   (*)          =  (#*)
   negate       =  (#*) (const (-1))
   abs          =  map abs
   signum       =  map signum
   fromInteger  =  const . fromInteger




inverse :: Fractional a => Stream a -> Stream a
inverse fs = f0 <:> (negate $ const f0) #* ft #* inverse fs 
  where
    f0 = 1/head fs
    ft = tail fs


-- | Formal variable
cX :: Num a => Stream a
cX = 0 <: 1 <: repeat 0


powX :: Num a => Int -> Stream a
powX n = replicate (fromIntegral n) 0 << 1 <: repeat 0 

polynomial :: Num a => [a] -> Stream a
polynomial ns = ns << repeat 0



-- | Shuffle product 
(<#>) :: Num a => Stream a -> Stream a -> Stream a 
fs <#> gs = f*g <: (ft <#> gs) + (fs <#> gt) 
   where 
     f  = head fs
     ft = tail fs
     g  = head gs
     gt = tail gs


-- | Shuffle inverse
sinverse :: Fractional a => Stream a -> Stream a
sinverse fs = f0 <: negate ft <#> (fs' <#> fs') 
  where 
    f0  = 1/head fs
    ft  = tail fs
    fs' = sinverse fs


-- | Stream exponentation
expos :: (Fractional a, Floating a) => Stream a -> Stream a
expos fs = exp f <: ft <#> expos fs 
  where
    f  = head fs
    ft = tail fs

deriv :: Num a => Stream a -> Stream a
deriv fs = tail (cX <#> (tail fs)) 

-- (big lambda)C 
blam :: Num a => Stream a -> Stream a
blam fs = head fs <: deriv fs 




-- Don't use
-- gsum :: Num a => Stream (Stream a) -> Stream a
-- gsum ss = sum 0 (head ss) <: gsum (tail ss) where
--   sum n ss = sum (n + head ss) (tail ss)


-- | composition
(<.>) :: Num a => Stream a -> Stream a -> Stream a
fs <.> gs = f <: gt * (ft * gs) where
  f  = head fs
  ft = tail fs
  gt = tail gs 
