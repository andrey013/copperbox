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


newtype ContState st env r a = ContState { 
         runCS :: (a -> st -> env -> r) -> st -> env -> r
      }    
      
instance Functor (ContState st env r) where
    fmap f m = ContState $ \c st env -> runCS m (c . f) st env
    
csreturn :: a -> ContState st env r a
csreturn x = ContState $ \c st env -> c x st env

csbind :: ContState st env r a -> 
            (a -> ContState st env r b) -> ContState st env r b
csbind m f = ContState $ \c -> runCS m (\a -> runCS (f a) c) 

instance Monad (ContState st env r) where
  return = csreturn
  (>>=) = csbind
   
instance Applicative (ContState st env r) where
  pure  = return
  (<*>) = ap  

  
csget :: ContState st env r st
csget = ContState $ \c st env -> c st st env

csput :: st -> ContState st env r ()
csput st = ContState $ \c _ env -> c () st env

instance MonadState st (ContState st env r) where
  get = csget
  put = csput


  
csask :: ContState st env r env
csask = ContState $ \c st env -> c env st env  

cslocal :: (env -> env) -> ContState st env r a -> ContState st env r a
cslocal f m = ContState $ \c st env -> runCS m c st (f env) 

  
instance MonadReader env (ContState st env r) where 
  ask = csask
  local = cslocal
  
absPosition :: ContState RAstate RAenv r Int
absPosition = fst . getPosition <$> get

movePos1 :: ContState RAstate RAenv r ()
movePos1 = do 
  p <- get
  put $ move1 p
  
newRegion :: Region -> ContState RAstate RAenv r ()
newRegion (i,j) = put $ position i j  


input :: ContState RAstate RAenv r ByteSequence
input = ask
