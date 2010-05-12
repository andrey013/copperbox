{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Utils
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


module Precis.Utils
  (
    unlist

  , H 
  , snocH
  , toListH
 
  , onSuccess
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

  
onSuccess :: Monad m => Either a b -> (b -> m c) -> m (Either a c)
onSuccess (Left a)  _  = return (Left a)
onSuccess (Right b) mf = liftM Right $ mf b 

  
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

