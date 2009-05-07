{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.StringRewriting
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- A module for string rewriting.
-- Actually the rewriting handles Haskell lists making it slightly
-- more general, though rewriting must be type-preserving.
-- i.e the type is [a] -> [a] rather than [Char] -> [Char]
-- 
-- The salient feature of string rewriting is that rules have 
-- aribtrary lookahead in the input stream. This is not the case 
-- for maps or folds. Also string rewriting is implacably linear
-- (i.e. list-y) unlike Strafunski and its children where the
-- traversal is syntax directed.
--
--------------------------------------------------------------------------------


module Mullein.StringRewriting where

import Control.Applicative
import Control.Monad.Identity

----

----


type FailCont tok r m = [tok] -> m r
type SuccCont tok r m a = a -> FailCont tok r m -> [tok] -> [tok] -> m r

newtype MatcherT tok r m a = MatcherT { 
          unMatcherT :: SuccCont tok r m a -> FailCont tok r m -> [tok] -> [tok] -> m r }

-- the return of Monad
succeed :: a -> MatcherT tok r m a
succeed x = MatcherT $ \sc fc ts n -> sc x fc ts n


-- bind (>>=) of Monad 
bind :: MatcherT tok r m a -> (a -> MatcherT tok r m b) -> MatcherT tok r m b 
bind m f = MatcherT $ \sk -> unMatcherT m $ \a -> unMatcherT (f a) sk

-- the mzero of MonadPlus
failure :: MatcherT tok r m a
failure = MatcherT $ \_ fk _ ln -> fk ln


-- mplus of MonadPlus
alt :: MatcherT tok r m a -> MatcherT tok r m a -> MatcherT tok r m a
alt p q = MatcherT $ \sk fk ts n -> unMatcherT p sk (unMatcherT q sk fk ts) ts n


instance Functor (MatcherT tok m r) where
  fmap f a = MatcherT $ \sk fk -> unMatcherT a (sk . f) fk 


instance Monad (MatcherT tok m r) where
  return = succeed
  (>>=) = bind

instance MonadPlus (MatcherT tok m r) where
  mzero = failure
  mplus = alt


instance Applicative (MatcherT tok m r) where
  pure  = return
  (<*>) = ap

instance Alternative (MatcherT tok m r) where
  empty = failure
  (<|>) = alt


runMatcherT :: MatcherT tok r m a 
            -> SuccCont tok r m a 
            -> FailCont tok r m 
            -> [tok] 
            -> [tok] 
            -> m r 
runMatcherT = unMatcherT


-- type H - aka Hughes lists, see worker wrapper paper
type H a = [a] -> [a]


-- For the present, only type-preserving rules are considered
-- (the output list has the same type as the input list).
-- This could easily be generalized.
--
-- @r@ and @m@ are the answer continuation and monad types.
-- Generally they will be left as type variables when writing
-- signatures.
type RuleTP tok m = MatcherT tok (H tok,[tok]) m (H tok) 



-- match any token
one :: MatcherT tok r m tok
one = MatcherT $ \sk fk ts ln -> f sk fk ts ln where
    f _  fk []     ln  = fk ln
    f sk fk (t:ts) ln  = sk t fk ts ln


-- match if the predicate yields True
pmatch :: MatcherT tok r m a -> (a -> Bool) -> MatcherT tok r m a
pmatch p f = p >>= (\x -> if f x then return x else mzero)

pmatchOne :: (tok -> Bool) -> MatcherT tok r m tok
pmatchOne p = pmatch one p


-- match a literal
lit :: Eq tok =>  tok -> MatcherT tok r m tok
lit t = MatcherT $ \sk fk ts ln -> case ts of 
   (x:xs) -> if x==t then sk x fk xs ln else fk ln
   _      -> fk ln


idOne :: RuleTP tok m
idOne = wrapH <$> one

wrap :: a -> [a]
wrap a = [a]


wrapH :: a -> H a
wrapH a = ([a] ++)

listH :: [a] -> H a
listH xs = (xs++)



rewriteTP :: Monad m => RuleTP tok m -> [tok] -> m [tok]
rewriteTP f input = step id input where
    step g [] = return $ g []
    step g xs = do (ansH,rest) <- rewriteTP1 f xs xs 
                   step (g . ansH) rest 


-- rewrite1 runs the supplied rewrite rule and returns the answer, plus
-- the remaining input.
-- If the rewrite fails the failure continuation returns the initial 
-- input as the answer. This means that rewrites must be type-preserving.
rewriteTP1 :: Monad m => RuleTP tok m -> [tok] -> [tok] -> m (H tok,[tok])
rewriteTP1 p xs ys = runMatcherT p succK failK xs ys 
  where
    succK ans _ ts _ = return (ans,ts)
    failK ts         = return (listH ts, []) 


-- rewrite with the Identity monad
rewriteId :: RuleTP tok Identity -> [tok] -> [tok]
rewriteId = (runIdentity .) . rewriteTP