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
  , drawl
  , drawc

--  , bang
  
  , canvas

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


tell :: Doc -> GenMonad ()
tell d1 = GenMonad $ \_ s -> ((),s, primitive d1)


run :: (Int,Int,Int,Int) -> Int -> GenMonad a -> String
run (x,y,w,h) sz ma = 
    runDoc $ rec_canvas0 x y w h sz `vconcat` (unwrapPrimitive body)
  where
    body = let (_,_,d1) = getGenMonad ma standard_context zeroSt in d1




{-
bang :: Int -> Int -> GenMonad Bang
bang x y = do 
    tell $ rec_bang x y 15 250 50 0 noSRL 0 (-6) default_display
    next Bang
-}

canvas :: Int -> Int -> Int -> Int -> GenMonad ()
canvas x y w h = 
    getDisplayProps >>= \props -> 
    tell $ rec_cnv x y 15 w h noSRL 0 0 props


draw :: Image a -> GenMonad a
draw ma = GenMonad $ \r s -> runImage r s ma
                             

drawl :: Point -> LocImage a -> GenMonad a 
drawl pt ma = draw $ ma `at` pt


-- | Note - ports not points.
-- 
drawc :: Port -> Port -> ConnectorImage a -> GenMonad a 
drawc p1 p2 ma = draw (connector ma p1 p2)