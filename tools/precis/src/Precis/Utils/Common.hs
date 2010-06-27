{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Utils.Common
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Utils
--
--------------------------------------------------------------------------------


module Precis.Utils.Common
  (
  -- * Hughes list
    H
  , emptyH
  , wrapH
  , consH
  , snocH
  , appendH
  , veloH
  , concatH
  
  , toListH
  , fromListH

  -- * Others    
  , unlist

  , mapLeft
  , mapRight
 
  , onSuccessM

  , pstar
  , pstar2
  , star 
  , star2

  ) where


import Control.Monad


-- Hughes lists

type H a = [a] -> [a]

infixr 2 `snocH`


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


-- | Traverse a list as per 'map' applying the supplied function 
-- to each element, *but* pruduce a Hughes list as output.
--
-- 
-- 
veloH :: (a -> b) -> [a] -> H b
veloH _ []     = id
veloH f (x:xs) = consH (f x) $ veloH f xs 

concatH :: [H a] -> H a
concatH = foldr (.) id


toListH :: H a -> [a]
toListH = ($ [])

fromListH :: [a] -> H a
fromListH [] = id
fromListH xs = (xs++)

--------------------------------------------------------------------------------

unlist :: [String] -> String
unlist []              = ""
unlist [w]             = w
unlist (w:ws)          = w ++ ',' : ' ' : unwords ws


-- Where\'s the bifunctor class when you need it...

mapLeft :: (a -> s) -> Either a b -> Either s b
mapLeft f (Left a)  = Left $ f a
mapLeft _ (Right b) = Right b

mapRight :: (b -> t) -> Either a b -> Either a t
mapRight _ (Left a)  = Left a
mapRight f (Right b) = Right $ f b


  
  
onSuccessM :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
onSuccessM ma msk = ma >>= step 
  where
    step (Left a)  = return (Left a)
    step (Right b) = liftM Right $ msk b 

--------------------------------------------------------------------------------
-- pstars - starling combinator with args permuted
-- useful for record updates

pstar     :: (a -> r -> ans) 
          -> (r -> a) 
          -> r -> ans
pstar f fa x = f (fa x) x

pstar2    :: (a -> b -> r -> ans) 
          -> (r -> a) -> (r -> b) 
          -> r -> ans
pstar2 f fa fb x = f (fa x) (fb x) x

star     :: (r -> a) 
         -> (a -> r -> ans) 
         -> r -> ans
star fa f x = f (fa x) x

star2    :: (r -> a) -> (r -> b) 
         -> (a -> b -> r -> ans) 
         -> r -> ans
star2 fa fb f x = f (fa x) (fb x) x

