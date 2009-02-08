{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ZParse.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ZParseMonad - the Monad and Applicative machinery for ZParse
--
--------------------------------------------------------------------------------


module Text.ZParse.ParseMonad where

import Text.ZParse.SrcPos

import Control.Applicative
import Control.Monad ()
import Control.Monad.Trans ()
import Control.Monad.State  

class SrcPos pos => ParseState st pos input | st -> pos, st -> input where
  pos           :: st -> pos
  setPos        :: pos -> st -> st
  remaining     :: st -> input 
  setRemaining  :: input -> st -> st 
  
  
type Fk st r = st -> r
type Sk st a r = a -> Fk st r -> st -> r
type CpsSt st m a r = Sk st a (m r) -> Fk st (m r) -> st -> m r

newtype ParserT st m a 
          = ParserT (forall r. Sk st a (m r) -> Fk st (m r) -> st -> m r )

unParserT :: ParserT st m a -> CpsSt st m a r 
unParserT (ParserT a) = a


-- the return of Monad
ret :: a -> ParserT st m a
ret x = ParserT $ \sc -> sc x

-- the bind (>>=) of Monad
bind :: ParserT st m a -> (a -> ParserT st m b) -> ParserT st m b 
bind m f = ParserT $ \sk -> unParserT m $ \a -> unParserT (f a) sk
  
-- mplus of MonadPlus
alt :: ParserT st m a -> ParserT st m a -> ParserT st m a
alt (ParserT p) (ParserT q) = 
    ParserT $ \sk fk st -> 
        let fkp _   = let fkq stq = fk stq 
                      in q sk fkp st
        in p sk fkp st

    
    
-- the mzero of MonadPlus
failure :: ParserT st m a
failure = ParserT $ \_ fk st -> fk st

instance Monad (ParserT st m) where
  return = ret
  (>>=) = bind
  
instance Functor (ParserT st m) where
  fmap f a = ParserT $ \sk -> unParserT a (sk . f) 


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
lookahead p f  = 
    ParserT $ \sk fk -> unParserT p (\x fc -> unParserT (f x) sk fc) fk



position :: (ParseState st pos input, SrcPos pos) => ParserT st m pos
position = pos <$> getT

input :: (ParseState st pos input, SrcPos pos) => ParserT st m input
input = remaining <$> get

updateState :: (ParseState st pos input, SrcPos pos) => 
            pos -> input -> ParserT st m ()
updateState p rest = do 
  st <- getT
  putT $ setPos p (setRemaining rest st)
      
    
    