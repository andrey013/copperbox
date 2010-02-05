{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  M2.Unfold
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Segmenting recursion
--
--------------------------------------------------------------------------------

module M2.Unfold
  ( 
    unfoldMap
  , apoUnfoldMap
  
  , AStep(..)
  , aunfoldr
  , aunfoldMap


  ) where



-- | @unfoldMap@ is the unfold analogue of accumMapL.
-- We can signal exhaustion early by the Maybe type.                
unfoldMap  :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],st) 
unfoldMap f s0 xs0    = step s0 xs0 where
   step st []         = ([],st)
   step st (x:xs)     = case f x st of
                          Nothing -> ([],st)
                          Just (a,st') -> (a:as,st'') 
                                          where (as,st'') = step st' xs  
   


apoUnfoldMap :: (a -> st -> Maybe (b,st)) -> st -> [a] -> ([b],[a]) 
apoUnfoldMap f s0 xs0    = step s0 xs0 where
   step _  []     = ([],[])
   step st (x:xs) = case f x st of
                      Nothing       -> ([],(x:xs))
                      Just (a,st')  -> (a:as,ys) 
                                          where (as,ys) = step st' xs  


--------------------------------------------------------------------------------
-- unfolds that return the state


data AStep a st = AYield a !st
                | ADone !st
  deriving (Eq,Show)


aunfoldr :: (st -> AStep a st) -> st -> ([a],st)
aunfoldr phi s0 = step $ phi s0 where
  step (AYield a st) = (a:as,st') where (as,st') = step (phi st)
  step (ADone st)    = ([],st)


-- | @anaMap@ is the unfold analogue of accumMapL.
-- We can signal exhaustion early by the Maybe type.                
aunfoldMap  :: (a -> st -> AStep b st) -> st -> [a] -> ([b],[a],st) 
aunfoldMap f s0 xs0    = step s0 xs0 where
   step st []     = ([],[],st)
   step st (x:xs) = case f x st of
                      ADone st'     -> ([],(x:xs),st')
                      AYield a st'  -> (a:as,ys,st'') 
                                          where (as,ys,st'') = step st' xs  
   

