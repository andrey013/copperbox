{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.CSEMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Cont-state-env monad  
-- 
--------------------------------------------------------------------------------

module Graphics.SFont.CSEMonad where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans


newtype ContStateEnvT st env r m a = ContStateEnvT { 
         runCSET :: (a -> st -> env -> m r) -> st -> env -> m r
      }    


  
        
instance Monad m => Functor (ContStateEnvT st env r m) where
    fmap f m = ContStateEnvT $ \c st env -> runCSET m (c . f) st env
    
csereturn :: a -> ContStateEnvT st env r m a
csereturn x = ContStateEnvT $ \c st env -> c x st env

csebind :: ContStateEnvT st env r m a -> 
            (a -> ContStateEnvT st env r m b) -> ContStateEnvT st env r m b
csebind m f = ContStateEnvT $ \c -> runCSET m (\a -> runCSET (f a) c) 

instance Monad m => Monad (ContStateEnvT st env r m) where
  return = csereturn
  (>>=) = csebind
   
instance Monad m => Applicative (ContStateEnvT st env r m) where
  pure  = return
  (<*>) = ap  


csecallCC :: ((a -> ContStateEnvT st env r m b) -> ContStateEnvT st env r m a) 
          -> ContStateEnvT st env r m a
csecallCC h = 
    ContStateEnvT $ \c -> runCSET (h (\a -> ContStateEnvT $ \_c' -> c a)) c


instance Monad m => MonadCont (ContStateEnvT st env r m) where
  callCC = csecallCC 



-- shift and reset typecheck at least...
cseshift :: Monad m 
         => ((a -> ContStateEnvT st env r m r) -> ContStateEnvT st env r m r) 
         -> ContStateEnvT st env r m a
cseshift h = 
    ContStateEnvT $ \c st env -> 
                      runCSET (h (\v -> ContStateEnvT $ \c' st' env' -> 
                                          (c v st' env') >>= \f ->
                                          c' f st' env')) -- ? which state and env
                              (const . const . return)
                              st
                              env
                                      
                                      
csereset :: Monad m => ContStateEnvT st env r m r -> ContStateEnvT st env r m r
csereset m = 
    ContStateEnvT $ \c st env -> 
                      (runCSET m (const . const . return) st env) >>= \f -> 
                      c f st env 




cselift :: Monad m => m a -> ContStateEnvT st env r m a
cselift m = ContStateEnvT $ \c st env -> m >>= (\a -> c a st env)

instance MonadTrans (ContStateEnvT st env r) where
  lift = cselift
  
    
cseget :: ContStateEnvT st env r m st
cseget = ContStateEnvT $ \c st env -> c st st env

cseput :: st -> ContStateEnvT st env r m ()
cseput st = ContStateEnvT $ \c _ env -> c () st env

instance Monad m => MonadState st (ContStateEnvT st env r m) where
  get = cseget
  put = cseput


  
cseask :: ContStateEnvT st env r m env
cseask = ContStateEnvT $ \c st env -> c env st env  

cselocal :: (env -> env) -> ContStateEnvT st env r m a -> ContStateEnvT st env r m a
cselocal f m = ContStateEnvT $ \c st env -> runCSET m c st (f env) 

  
instance Monad m => MonadReader env (ContStateEnvT st env r m) where 
  ask = cseask
  local = cselocal

-- if the inner monad supports @MonadError@ use it...

csethrowError :: MonadError e m => e -> ContStateEnvT st env r m a
csethrowError e = 
    ContStateEnvT $ \c st env -> throwError e >>= (\a -> c a st env)



csecatchError :: MonadError e m 
              => ContStateEnvT st env r m a 
              -> (e -> ContStateEnvT st env r m a) 
              -> ContStateEnvT st env r m a
csecatchError m h = 
    ContStateEnvT $ \c st env -> 
                      catchError (runCSET m c st env) 
                                 (\e -> runCSET (h e) c st env)

{-

-- This instance doesn't work without -XUndecidableInstances...
-- Instead any monad using ContStateEnvT will have to 
-- write there own instance with csecatchError and csethrowError
   
                
instance (Monad m, MonadError e m) => 
              MonadError e (ContStateEnvT st env r m) where
  catchError = csecatchError
  throwError = csethrowError
  
-}  
