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

    H 
  , snocH
  , toListH
 
  , para
  , mbCons
  , mbEither


  , predMaybe
  , onSuccess
  , onSuccessM

  , predMaybeM
  , condM


  ) where


import Control.Monad



-- Hughes lists

type H a = [a] -> [a]

snocH :: H a -> a -> H a
snocH f a = f . (a:)

toListH :: H a -> [a]
toListH = ($ [])



-- paramorphism (generalizes catamorphism (fold))
para :: (a -> ([a], b) -> b) -> b -> [a] -> b
para _   b []     = b
para phi b (x:xs) = phi x (xs, para phi b xs)



mbCons :: Maybe a -> [a] -> [a]
mbCons oa xs = maybe xs (:xs) $ oa

mbEither :: b -> Maybe a -> Either b a
mbEither b = maybe (Left b) Right


predMaybe :: (a -> Bool) -> a -> Maybe a
predMaybe p a | p a       = Just a
              | otherwise = Nothing



  
onSuccess :: Monad m => Either a b -> (b -> m c) -> m (Either a c)
onSuccess (Left a)  _  = return (Left a)
onSuccess (Right b) mf = liftM Right $ mf b 

  
onSuccessM :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
onSuccessM ma msk = ma >>= step 
  where
    step (Left a)  = return (Left a)
    step (Right b) = liftM Right $ msk b 


predMaybeM :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
predMaybeM mp a = mp a >>= \v -> if v then return (Just a) else return Nothing


condM :: Monad m => m Bool -> m b -> m a -> m (Either a b)
condM mtest sk fk = mtest >>= \ans -> 
                    if ans then liftM Right sk  else liftM Left fk

