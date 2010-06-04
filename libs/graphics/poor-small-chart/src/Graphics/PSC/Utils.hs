{-# LANGUAGE NamedFieldPuns             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.PSC.Utils
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Utilities
--
--------------------------------------------------------------------------------

module Graphics.PSC.Utils
  (

  -- * functions
    unfoldrM
  , mbM

  -- * Hughes list
  , H
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH
  , veloH
  , concatH

  , toListH
  , fromListH

  ) where




unfoldrM :: Monad m => (st -> m (Maybe (a,st))) -> st -> m [a]
unfoldrM phi st = phi st >>= \ans -> case ans of 
    Nothing      -> return []
    Just (x,st') -> do {xs <- unfoldrM phi st'; return (x:xs) }

 
mbM :: Monad m => (a -> m b) -> Maybe a ->  m (Maybe b)
mbM _  Nothing  = return Nothing
mbM mf (Just a) = mf a >>= return . Just 


--------------------------------------------------------------------------------
-- Hughes list

type H a = [a] -> [a]

emptyH :: H a
emptyH = id

wrapH :: a -> H a
wrapH a = consH a id 

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH  f a = f . (a:)

appendH :: H a -> H a -> H a
appendH f g = f . g

-- | velo consumes the list as per map, but builds it back
-- as a Hughes list - so items can be dropped
-- replaced, repeated, etc...
-- 
veloH :: (a -> H b) -> [a] -> H b
veloH f = foldr step id 
  where step a hf = f a . hf

concatH :: [H a] -> H a
concatH = foldr (.) id


toListH :: H a -> [a]
toListH = ($ [])

fromListH :: [a] -> H a
fromListH xs = (xs++)
