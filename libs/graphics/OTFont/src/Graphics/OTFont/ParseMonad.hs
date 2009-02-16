{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Monad for /random access/ parsing.
-- Note there is no automatic backtracking.  
-- 
--------------------------------------------------------------------------------

module Graphics.OTFont.ParseMonad where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Array.Unboxed
import Data.Word


type ByteSequence = UArray Int Word8 

type RAstate = Position
type RAenv   = ByteSequence
     
type Region = (Int,Int)


newtype Position = Position { getPosition :: Region }
  deriving (Eq,Show)

position :: Int -> Int -> Position
position i j = Position (i,j)
 
move1 :: Position -> Position
move1 (Position (i,j)) = Position (i+1,j)


newtype ContStateT st env m r a = ContStateT { 
         runCST :: (a -> st -> env -> m r) -> st -> env -> m r
      }    
      
instance Functor (ContStateT st env m r) where
    fmap f m = ContStateT $ \c st env -> runCST m (c . f) st env
    
csreturn :: a -> ContStateT st env m r a
csreturn x = ContStateT $ \c st env -> c x st env

csbind :: ContStateT st env m r a -> 
            (a -> ContStateT st env m r b) -> ContStateT st env m r b
csbind m f = ContStateT $ \c -> runCST m (\a -> runCST (f a) c) 

instance Monad m => Monad (ContStateT st env m r) where
  return = csreturn
  (>>=) = csbind
   
instance Monad m => Applicative (ContStateT st env m r) where
  pure  = return
  (<*>) = ap  

  
csget :: ContStateT st env m r st
csget = ContStateT $ \c st env -> c st st env

csput :: st -> ContStateT st env m r ()
csput st = ContStateT $ \c _ env -> c () st env

instance Monad m => MonadState st (ContStateT st env m r) where
  get = csget
  put = csput


  
csask :: ContStateT st env r m env
csask = ContStateT $ \c st env -> c env st env  

cslocal :: (env -> env) -> ContStateT st env m r a -> ContStateT st env m r a
cslocal f m = ContStateT $ \c st env -> runCST m c st (f env) 

  
instance Monad m => MonadReader env (ContStateT st env m r) where 
  ask = csask
  local = cslocal
  
absPosition :: Monad m => ContStateT RAstate RAenv m r Int
absPosition = fst . getPosition <$> get

movePos1 :: Monad m => ContStateT RAstate RAenv m r ()
movePos1 = do 
  p <- get
  put $ move1 p
  
newRegion :: Monad m => Region -> ContStateT RAstate RAenv m r ()
newRegion (i,j) = put $ position i j  


input :: Monad m => ContStateT RAstate RAenv m r ByteSequence
input = ask
