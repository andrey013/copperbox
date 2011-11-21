{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Monad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Gen monad
--
--------------------------------------------------------------------------------


module PDSS.Core.Monad
  ( 

    GenMonad
  , run

  , draw
  , draw_
  , drawl
  , drawl_
  , drawc

  ) where 


import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.ObjectBasis
import PDSS.Core.PdDoc
import PDSS.Core.Utils.FormatCombinators


import Control.Applicative hiding ( empty ) 
import Data.Monoid


-- | At least a Reader, Writer, State ...

newtype GenMonad a = GenMonad { 
    getGenMonad :: PdContext -> GenSt -> (a,GenSt,Primitive) }

instance Functor GenMonad where
  fmap f ma = GenMonad $ \r s -> let (a,s1,d1) = getGenMonad ma r s
                                 in (f a,s1,d1)


instance Applicative GenMonad where
  pure a    = GenMonad $ \_ s -> (a,s,mempty)
  mf <*> ma = GenMonad $ \r s -> let (f,s1,d1) = getGenMonad mf r s
                                     (a,s2,d2) = getGenMonad ma r s1
                                 in (f a, s2, d1 `mappend` d2)



instance Monad GenMonad where
  return  = pure
  m >>= k = GenMonad $ \r s -> let (a,s1,d1) = getGenMonad m r s
                                   (b,s2,d2) = getGenMonad (k a) r s1
                               in (b, s2, d1 `mappend` d2)


instance ContextM GenMonad where
  askCtx        = GenMonad $ \r s -> (r, s, mempty) 
  localize f ma = GenMonad $ \r s -> getGenMonad ma (f r) s




run :: (Int,Int,Int,Int) -> Int -> GenMonad a -> String
run (x,y,w,h) sz ma = 
    runDoc $ rec_ncanvas0 x y w h sz `vconcat` (unwrapPrimitive body)
  where
    body = let (_,_,d1) = getGenMonad ma standard_context zeroSt in d1




draw :: Image a -> GenMonad a
draw ma = GenMonad $ \r s -> runImage r s ma
                             
draw_ :: Image a -> GenMonad ()
draw_ ma = draw ma >> return ()


drawl :: Point -> LocImage a -> GenMonad a 
drawl pt ma = draw $ ma `at` pt


drawl_ :: Point -> LocImage a -> GenMonad ()
drawl_ pt ma = drawl pt ma >> return ()

-- | Note - ports not points.
-- 
drawc :: Port -> Port -> ConnectorImage a -> GenMonad a 
drawc p1 p2 ma = draw (connector ma p1 p2)