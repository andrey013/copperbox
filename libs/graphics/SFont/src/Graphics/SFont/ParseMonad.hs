{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-} 
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.SFont.ParseMonad
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

module Graphics.SFont.ParseMonad where

import Graphics.SFont.PrimitiveDatatypes ( Region, ByteSequence )
import Graphics.SFont.CSEMonad

import Control.Applicative
-- import Control.Monad.Cont
-- import Control.Monad.Error
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Word





type ParserState = Region
data ParserEnv   = ParserEnv { input_bounds :: Region, input_data :: ByteSequence }
     

newtype ParseError = ParseError String
  deriving (Show) 

instance Error ParseError where
  noMsg = ParseError ""
  strMsg s = ParseError s  

newtype ParserT r m a = ParserT {
      getParserT :: ContStateEnvT ParserState ParserEnv r (ErrorT ParseError m) a
    }
  deriving ( Functor, Monad, MonadState ParserState, MonadReader ParserEnv ) 

runParserT :: Monad m => 
              ParserT r m r -> ByteSequence -> m (Either ParseError r)
runParserT (ParserT m) arr = runErrorT $
    runCSET m (const . const . return) st env where
        st  = bounds arr
        env = ParserEnv { input_bounds=st, input_data=arr }
        
        
instance Monad m => Applicative (ParserT r m) where
  pure = return
  (<*>) = ap

instance MonadTrans (ParserT r) where
  lift = ParserT . cselift . lift 
  
instance Monad m => MonadError ParseError (ParserT r m) where
  throwError = ParserT . csethrowError  
  m `catchError`  h = ParserT $ csecatchError (getParserT m) (getParserT . h)

pcallCC :: Monad m => ((a -> ParserT r m b) -> ParserT r m a) -> ParserT r m a
pcallCC f = ParserT $ csecallCC $ \c -> getParserT (f (\a -> ParserT (c a)))
  
instance Monad m => MonadCont (ParserT r m) where
  callCC = pcallCC

-- No instances of MonadState and MonadReader, because put, get and ask
-- shouldn't escape this module
 
pput :: ParserState -> ParserT r m ()
pput = ParserT . cseput

pget :: ParserT r m ParserState
pget = ParserT $ cseget


pask :: ParserT r m ParserEnv
pask = ParserT $ cseask
 

 
move1 :: Monad m => ParserT r m ()
move1 = do 
    (i,j) <- pget
    if i<j then put (i+1,j) 
           else throwError $ strMsg $ "move1 - out of bounds at " ++ show i

position :: Monad m => ParserT r m Int
position = fst <$> pget

reportError :: Monad m => String -> ParserT r m a
reportError s = do
  i <- position
  throwError $ strMsg $ "Error - at " ++ show i ++ ": " ++ s  


input :: Monad m => ParserT r m ByteSequence
input = input_data <$> pask

inputBounds :: Monad m => ParserT r m Region
inputBounds = input_bounds <$> pask

inputRemaining :: Monad m => ParserT r m Int
inputRemaining = f <$> pget where
  f (i,j) = j - i

setRange :: Monad m => Int -> Int -> ParserT r m ()
setRange offset len = let (m,n) = (offset, offset+len) in do
  (i,j) <- inputBounds
  if (m < i || n > j) 
      then throwError $ strMsg $ "setRange - out of bounds " 
                                    ++ show (m,n) ++ " on "
                                    ++ show (i,j)
      else put (m,n)

-- run parser p within the supplied range retore the /original/ range 
-- afterwards


-- absolute - absolute is more common (at least for fonts), hence it gets 
-- the shorthand alias
withinRange :: Monad m => Int -> Int -> ParserT r m a -> ParserT r m a 
withinRange = withinRangeAbs

withinRangeAbs :: Monad m => Int -> Int -> ParserT r m a -> ParserT r m a  
withinRangeAbs absval len p = do 
    o <- get
    setRange absval len
    a <- p
    put o
    return a
    
    
-- relative
withinRangeRel :: Monad m => Int -> Int -> ParserT r m a -> ParserT r m a  
withinRangeRel offset len p = do 
    o@(oo,_) <- get
    setRange (oo+offset) len
    a <- p
    put o
    return a



--------------------------------------------------------------------------------
-- Primitive parser - getWord8 

getWord8 :: Monad m => ParserT r m Word8
getWord8 = do
    a <- input
    i <- position
    let e = a!i
    move1
    return e