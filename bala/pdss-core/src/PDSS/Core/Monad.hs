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
  , text
--  , bang
  
  , canvas

  ) where 


import PDSS.Core.Colour
import PDSS.Core.Context
import PDSS.Core.InternalTypes
import PDSS.Core.PDDoc
import PDSS.Core.Utils.FormatCombinators hiding ( text )
import Control.Applicative hiding ( empty ) 

type St = Int


-- | At least a Reader, Writer, State ...

newtype GenMonad a = GenMonad { getGenMonad :: PdContext -> St -> (a,St,Doc) }

instance Functor GenMonad where
  fmap f ma = GenMonad $ \r s -> let (a,s1,d1) = getGenMonad ma r s
                                 in (f a,s1,d1)


instance Applicative GenMonad where
  pure a    = GenMonad $ \r s -> (a,s,empty)
  mf <*> ma = GenMonad $ \r s -> let (f,s1,d1) = getGenMonad mf r s
                                     (a,s2,d2) = getGenMonad ma r s1
                                 in (f a, s2, d1 `vconcat` d2)



instance Monad GenMonad where
  return  = pure
  m >>= k = GenMonad $ \r s -> let (a,s1,d1) = getGenMonad m r s
                                   (b,s2,d2) = getGenMonad (k a) r s1
                               in (b, s2, d1 `vconcat` d2)

tell :: Doc -> GenMonad ()
tell d1 = GenMonad $ \_ s -> ((),s,d1)

next :: (Int -> a) -> GenMonad a
next f = GenMonad $ \_ s -> (f s, s + 1, empty)

run :: (Int,Int,Int,Int) -> Int -> GenMonad a -> String
run (x,y,w,h) sz ma = 
    runDoc (rec_canvas0 x y w h sz `vconcat` body)
  where
    body = let (_,_,d1) = getGenMonad ma standard_context 0 in d1



text :: Int -> Int -> String -> GenMonad ()
text x y ss = tell $ rec_text x y ss

{-
bang :: Int -> Int -> GenMonad Bang
bang x y = do 
    tell $ rec_bang x y 15 250 50 0 noSRL 0 (-6) default_display
    next Bang
-}

canvas :: Int -> Int -> Int -> Int -> RGBi -> GenMonad ()
canvas x y w h rgb = 
    tell $ rec_cnv x y 15 w h noSRL 0 0 (default_display {obj_bgcolour = rgb })
