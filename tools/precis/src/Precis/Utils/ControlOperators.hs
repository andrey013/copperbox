{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.Utils.ControlOperators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ALL NAMES PROVISIONAL...
--
--------------------------------------------------------------------------------


module Precis.Utils.ControlOperators
  (
    suppress
  , elaborate

  , firstSuccess
  , valid
  , validE

  ) where



suppress :: Either e a -> Maybe a
suppress (Right a) = Just a
suppress _         = Nothing

elaborate :: e -> Maybe a -> Either e a
elaborate ex Nothing  = Left ex
elaborate _  (Just a) = Right a 




-- | Apply the function to the list
firstSuccess :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstSuccess mf xs0 = step xs0 
  where
    step []     = return Nothing
    step (x:xs) = mf x >>= \ans -> case ans of
                                     Just a -> return $ Just a
                                     Nothing -> step xs
                           


valid :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
valid test a = test a >>= \ans -> 
    if ans then return (Just a) else return Nothing

validE :: Monad m => ex -> (a -> m Bool) -> a -> m (Either ex a)
validE ex test a = test a >>= \ans -> 
    if ans then return (Right a) else return (Left ex)