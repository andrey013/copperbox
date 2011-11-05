{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.PDSS.Monad
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


module Sound.PDSS.Monad
  ( 

    GenMonad
  , run
  , text_

  ) where 


import Sound.PDSS.PDDoc
import Sound.PDSS.Utils.FormatCombinators
import Control.Applicative hiding ( empty ) 

data St = St () -- to fill


-- | At least a State and Writer....

newtype GenMonad a = GenMonad { getGenMonad :: St -> (a,St,Doc) }

instance Functor GenMonad where
  fmap f ma = GenMonad $ \s -> let (a,s1,d1) = getGenMonad ma s
                               in (f a,s1,d1)


instance Applicative GenMonad where
  pure a    = GenMonad $ \s -> (a,s,empty)
  mf <*> ma = GenMonad $ \s -> let (f,s1,d1) = getGenMonad mf s
                                   (a,s2,d2) = getGenMonad ma s1
                               in (f a, s2, d1 `vconcat` d2)



instance Monad GenMonad where
  return  = pure
  m >>= k = GenMonad $ \s -> let (a,s1,d1) = getGenMonad m s
                                 (b,s2,d2) = getGenMonad (k a) s1
                             in (b, s2, d1 `vconcat` d2)

tell :: Doc -> GenMonad ()
tell d1 = GenMonad $ \s -> ((),s,d1)


run :: (Int,Int,Int,Int) -> Int -> GenMonad a -> String
run (x,y,w,h) sz ma = 
    runDoc (rec_canvas x y w h sz `vconcat` body)
  where
    body = let (_,_,d1) = getGenMonad ma (St ()) in d1



text_ :: Int -> Int -> String -> GenMonad ()
text_ xp yp ss = tell $ rec_text xp yp ss