{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.UnitConvert
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Conversion between different unit types.
--
-- Unfortunately this is rather clunky as the primary objects in 
-- Wumpus-Basic are not usefully functors.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.UnitConvert
  (

{-
    ConvertAlg(..)
  , unit_conv
  , func_conv
  , adaptConv

  , converti
  , convertli
  , convertlti
  , convertconn
  , convertpti
-}

  ) where


{-

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Objects.BaseObjects
import Wumpus.Basic.Kernel.Objects.Connector
import Wumpus.Basic.Kernel.Objects.PosImage

import Wumpus.Core                              -- package: wumpus-core




-- It looks like this is the best we will get...


data ConvertAlg ans_in unit_in ans_out unit_out = 
     ConvertAlg { loc_input_u   :: unit_out -> unit_in
                , loc_prim_u    :: unit_in  -> unit_out
                , loc_ans       :: ans_in   -> ans_out
                }


unit_conv :: (PtSize u1, PtSize u) => ConvertAlg a u1 a u
unit_conv = 
    ConvertAlg (fromPsPoint . toPsPoint) (fromPsPoint . toPsPoint) id

-- | /Functorial/ version of 'unit_conv'.
--
func_conv :: (Functor t, PtSize u1, PtSize u) => ConvertAlg (t u1) u1 (t u) u
func_conv = 
    ConvertAlg (fromPsPoint . toPsPoint) (fromPsPoint . toPsPoint)
                                         (fmap (fromPsPoint . toPsPoint)) 
               

adaptConv :: (u -> u) -> (b -> b) -> ConvertAlg a u1 b u -> ConvertAlg a u1 b u
adaptConv uf bf (ConvertAlg f1 f2 f3) = ConvertAlg f1 (uf . f2) (bf . f3)


converti :: ConvertAlg a u1 b u -> Image u1 a -> Image u b
converti (ConvertAlg _ fr fl) img = fmap (bimap fl (fmap fr)) img



convertli :: ConvertAlg a u1 b u -> LocImage u1 a -> LocImage u b
convertli (ConvertAlg fpt fr fl) img = 
    promoteR1 $ \pt -> fmap (bimap fl (fmap fr)) $ img `at` fmap fpt pt


convertlti :: ConvertAlg a u1 b u 
             -> LocThetaImage u1 a -> LocThetaImage u b
convertlti (ConvertAlg fpt fr fl) img = promoteR2 $ \pt ang -> 
    fmap (bimap fl (fmap fr)) $ apply2R2 img (fmap fpt pt) ang 


convertconn :: ConvertAlg a u1 b u 
            -> ConnectorImage u1 a -> ConnectorImage u b
convertconn (ConvertAlg f1 f2 f3) img = promoteR2 $ \p0 p1 -> 
    fmap (bimap f3 (fmap f2)) $ apply2R2 img (fmap f1 p0) (fmap f1 p1)




convertpti :: ConvertAlg a u1 b u 
           -> PosThetaImage u1 a -> PosThetaImage u b
convertpti (ConvertAlg f1 f2 f3) img = promoteR3 $ \p0 rpos ang -> 
    fmap (bimap f3 (fmap f2)) $ apply3R3 img (fmap f1 p0) rpos ang


-}


{-

-- Cannot write a class the type synonym instance must be fully 
-- saturated.

class ImageConvert bi where
  imageCvt :: (a -> b) -> bi u1 a -> bi u b

-- Not fully staurated...
instance ImageConvert LocThetaImage where
  imageCvt fb = locThetaImageConv (ConvertAlg (fromPsPoint . toPsPoint)
                                              (fromPsPoint . toPsPoint) fb)

-}


{-
-- Doodle...
uconv :: (PtSize u1, PtSize u) => LocImage u1 a -> LocImage u a
uconv img = promoteR1 $ \pt -> fmap post $ img `at` fmap (fromPsPoint . toPsPoint) pt
  where
    post = bimap id (fmap (fromPsPoint . toPsPoint))


uconvF :: (PtSize u1, PtSize u, Functor t) => LocImage u1 (t u1) -> LocImage u (t u)
uconvF = locImageConv $ ConvertAlg (fromPsPoint . toPsPoint)
                                   (fromPsPoint . toPsPoint)
                                   (fmap (fromPsPoint . toPsPoint))


uconvF' :: (PtSize u1, PtSize u, Functor f) 
        => (u -> u) -> LocImage u1 (f u1) -> LocImage u (f u)
uconvF' f img = promoteR1 $ \pt -> 
    fmap (bimap (fmap (f . cnv)) (fmap (f . cnv))) $ img `at` fmap cnv pt


-}

