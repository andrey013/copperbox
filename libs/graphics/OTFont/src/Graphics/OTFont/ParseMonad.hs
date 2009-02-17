{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-} 
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
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
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


newtype ContStateT st env r m a = ContStateT { 
         runCST :: (a -> st -> env -> m r) -> st -> env -> m r
      }    


  
        
instance Monad m => Functor (ContStateT st env r m) where
    fmap f m = ContStateT $ \c st env -> runCST m (c . f) st env
    
csreturn :: a -> ContStateT st env r m a
csreturn x = ContStateT $ \c st env -> c x st env

csbind :: ContStateT st env r m a -> 
            (a -> ContStateT st env r m b) -> ContStateT st env r m b
csbind m f = ContStateT $ \c -> runCST m (\a -> runCST (f a) c) 

instance Monad m => Monad (ContStateT st env r m) where
  return = csreturn
  (>>=) = csbind
   
instance Monad m => Applicative (ContStateT st env r m) where
  pure  = return
  (<*>) = ap  


cscallCC :: ((a -> ContStateT st env r m b) -> ContStateT st env r m a) -> 
                ContStateT st env r m a
cscallCC h = ContStateT $ \c -> runCST (h (\a -> ContStateT $ \_c' -> c a)) c


instance Monad m => MonadCont (ContStateT st env r m) where
  callCC = cscallCC 



-- shift and reset typecheck at least...
csshift :: Monad m => 
           ((a -> ContStateT st env r m r) -> ContStateT st env r m r) -> 
           ContStateT st env r m a
csshift h = 
  ContStateT $ \c st env -> runCST (h (\v -> ContStateT $ \c' st' env' -> 
                                                (c v st' env') >>= \f ->
                                                  c' f st' env')) -- ? which state and env
                                      (const . const . return)
                                      st
                                      env
                                      
                                      
csreset :: Monad m => ContStateT st env r m r -> ContStateT st env r m r
csreset m = ContStateT $ \c st env -> 
                (runCST m (const . const . return) st env) >>= \f -> c f st env 




cslift :: Monad m => m a -> ContStateT st env r m a
cslift m = ContStateT $ \c st env -> m >>= (\a -> c a st env)

instance MonadTrans (ContStateT st env r) where
  lift = cslift
  
    
csget :: ContStateT st env r m st
csget = ContStateT $ \c st env -> c st st env

csput :: st -> ContStateT st env r m ()
csput st = ContStateT $ \c _ env -> c () st env

instance Monad m => MonadState st (ContStateT st env r m) where
  get = csget
  put = csput


  
csask :: ContStateT st env r m env
csask = ContStateT $ \c st env -> c env st env  

cslocal :: (env -> env) -> ContStateT st env r m a -> ContStateT st env r m a
cslocal f m = ContStateT $ \c st env -> runCST m c st (f env) 

  
instance Monad m => MonadReader env (ContStateT st env r m) where 
  ask = csask
  local = cslocal

-- if the inner monad supports @MonadError@ use it...
csthrowError :: MonadError e m => e -> ContStateT st env r m a
csthrowError e = cslift $ throwError e

{-
-- without lift...
csthrowError' :: MonadError e m => e -> ContStateT st env r m a
csthrowError' e = ContStateT $ \c st env -> throwError e >>= (\a -> c a st env)
-}


cscatchError :: MonadError e m =>
                ContStateT st env r m a -> (e -> ContStateT st env r m a) -> 
                ContStateT st env r m a
cscatchError m h = 
    ContStateT $ \c st env -> catchError (runCST m c st env)
                                         (\e -> runCST (h e) c st env)

                

absPosition :: Monad m => ContStateT RAstate RAenv r m Int
absPosition = fst . getPosition <$> get

movePos1 :: Monad m => ContStateT RAstate RAenv r m ()
movePos1 = do 
  p <- get
  put $ move1 p
  
newRegion :: Monad m => Region -> ContStateT RAstate RAenv r m ()
newRegion (i,j) = put $ position i j  



input :: Monad m => ContStateT RAstate RAenv r m ByteSequence
input = ask
