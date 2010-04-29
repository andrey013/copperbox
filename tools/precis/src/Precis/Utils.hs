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
 


  , onSuccess
  , onSuccessM

  ) where


import Control.Monad



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

