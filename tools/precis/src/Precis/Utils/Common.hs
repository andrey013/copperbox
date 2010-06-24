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
    unlist

  , H 
  , snocH
  , toListH

  , mapLeft
  , mapRight
 
  , onSuccessM

  , pstar
  , pstar2


  ) where


import Control.Monad

unlist :: [String] -> String
unlist []              = ""
unlist [w]             = w
unlist (w:ws)          = w ++ ',' : ' ' : unwords ws

-- Hughes lists

type H a = [a] -> [a]

snocH :: H a -> a -> H a
snocH f a = f . (a:)

toListH :: H a -> [a]
toListH = ($ [])



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

