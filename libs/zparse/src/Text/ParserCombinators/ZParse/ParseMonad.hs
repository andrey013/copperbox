{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2008, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ZParseMonad - the Monad and Applicative machinery for ZParse
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ZParse.ParseMonad 
  ( 
    ParserT(..)
  , Parser
  , HasInput(..)
  , runParserT
  , lookahead
  , satisfies


  ) where
 

import Text.ParserCombinators.ZParse.Utils

import Control.Applicative
import Control.Monad
import Control.Monad.Trans ()
import Control.Monad.State  



type Fk st r = st -> r
type Sk st a r = a -> Fk st r -> st ->  r
type Parser st m a r = Sk st a (m r) -> Fk st (m r) -> st ->  m r

newtype ParserT st m a = ParserT { 
          getParserT :: forall r. Sk st a (m r) -> Fk st (m r) -> st -> m r }




class HasInput st where
  type InputStream st :: *
  getInput :: st -> InputStream st
  setInput :: InputStream st -> st -> st 


runParserT :: ParserT st m a -> Parser st m a r
runParserT = getParserT


-- the return of Monad
ret :: a -> ParserT st m a
ret x = ParserT $ \sc -> sc x

-- the bind (>>=) of Monad
bind :: ParserT st m a -> (a -> ParserT st m b) -> ParserT st m b 
bind m f = ParserT $ \sk -> getParserT m $ \a -> getParserT (f a) sk
  
-- mplus of MonadPlus
alt :: ParserT st m a -> ParserT st m a -> ParserT st m a
alt p q = ParserT $ \sk fk -> getParserT p sk (getParserT q sk fk)

-- the mzero of MonadPlus
failure :: ParserT st m a
failure = ParserT $ \_ fk st -> fk st

instance Monad (ParserT st m) where
  return = ret
  (>>=)  = bind
  
instance Functor (ParserT st m) where
  fmap f a = ParserT $ \sk -> getParserT a (sk . f) 


instance MonadPlus (ParserT st m) where
  mzero = failure
  mplus = alt
    
instance Applicative (ParserT st m) where
  pure  = return
  (<*>) = ap

instance Alternative (ParserT st m) where
  empty = failure
  (<|>) = alt

liftT :: Monad m => m a -> ParserT st m a
liftT m = ParserT $ \sk fk st -> m >>= (\a -> sk a fk st)

instance MonadTrans (ParserT st) where
  lift = liftT  

getT :: ParserT st m st
getT = ParserT $ \sk fk st -> sk st fk st

putT :: st -> ParserT st m ()
putT st = ParserT $ \sk fk _ -> sk () fk st

instance MonadState st (ParserT st m) where
  get = getT
  put = putT
  


--------------------------------------------------------------------------------
-- Low level combinators

lookahead :: ParserT st m a -> (a -> ParserT st m b) -> ParserT st m b 
lookahead p f  = ParserT $ \sk fk -> 
    getParserT p (\x fc -> getParserT (f x) sk fc) fk


-- | @satisfies@ needs /bind/ so its monadic and not applicative.
satisfies :: MonadPlus m => m a -> (a -> Bool) -> m a
satisfies p f = p >>= (\x -> if f x then return x else mzero)


    
