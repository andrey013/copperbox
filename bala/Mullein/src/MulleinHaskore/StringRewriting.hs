{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MulleinHaskore.StringRewriting
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
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


module MulleinHaskore.StringRewriting ( 
  MatcherT,
  runMatcherT, 
  RuleTP,
  RuleTC,

  -- match combinators 
  ( <:> ),
  choice,
  count,
  between,
  option,
  optionMaybe,
  optionUnit,
  skipMany,
  skipMany1,
  many1,
  sepBy,
  sepBy1,
  sepEndBy,
  sepEndBy1,
  manyTill,
  satisfies,

  one,
  pmatchOne,
  lit,

  rewriteTP,
  rewriteTP'id,

  rewriteTC,
  rewriteTC'id,

  ) where

import Control.Applicative
import Control.Monad.Identity

import qualified Data.DList as D

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




-- Type-preserving rules have the same input and output token type.
-- However, rules are free to expand or contract the input at each 
-- step. e.g.
--
-- B3  -> BBB  - replace two tokens with three
-- BBB -> B3   - replace three tokens with two
--
-- This is why internally rules are represented as Hughes lists
-- so we can append freely.
--
-- @r@ and @m@ are the answer continuation and monad types.
-- Generally they will be left as type variables when writing
-- signatures.
type RuleTP tok m = MatcherT tok (D.DList tok,[tok]) m (D.DList tok) 


type RuleTC tok out m = MatcherT tok (D.DList out,[tok]) m (D.DList out)
 


--------------------------------------------------------------------------------
-- general Applicative combinators 

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty 
   
count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 
          
between :: Applicative f => f open -> f close -> f a -> f a
between o c a = o *> a <* c

          
option :: Alternative f => a -> f a -> f a
option x p          = p <|> pure x

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe = optional

-- aka Parsecs /optional/
optionUnit :: Alternative f => f a -> f ()
optionUnit p = () <$ p <|> pure ()

skipMany :: Alternative f => f a -> f ()
skipMany p = many_p
  where many_p = some_p <|> pure ()
        some_p = p       *> many_p


skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

-- | @many1@ an alias for @some@. 
many1 :: Alternative f => f a -> f [a]
many1 = some

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = p <:> step where
    step = (sep *> p) <:> step <|> pure []

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: Alternative f => f a -> f b -> f [a]
sepEndBy1 p sep = (p <* sep) <:> step where
    step = (p <* sep) <:> step <|> pure []
    
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = step <|> pure [] where
    step = p <:> (step <|> (pure [] <$> end))
    
-- And a monadic one...

-- | @satisfies@ needs /bind/ so its monadic and not applicative.
satisfies :: MonadPlus m => m a -> (a -> Bool) -> m a
satisfies p f = p >>= (\x -> if f x then return x else mzero)


--------------------------------------------------------------------------------
-- specific matchers

-- match any token
one :: MatcherT tok r m tok
one = MatcherT $ \sk fk ts ln -> f sk fk ts ln where
    f _  fk []     ln  = fk ln
    f sk fk (t:ts) ln  = sk t fk ts ln

pmatchOne :: (tok -> Bool) -> MatcherT tok r m tok
pmatchOne p = satisfies one p


-- match a literal
lit :: Eq tok =>  tok -> MatcherT tok r m tok
lit t = MatcherT $ \sk fk ts ln -> case ts of 
   (x:xs) -> if x==t then sk x fk xs ln else fk ln
   _      -> fk ln


--------------------------------------------------------------------------------
-- 




-- type preserving rewrite strategy
rewriteTP :: Monad m => RuleTP tok m -> [tok] -> m [tok]
rewriteTP f input = step D.empty input where
    step g [] = return $ D.toList g
    step g xs = do (ansH,rest) <- rewriteTP1 f xs xs 
                   step (g `D.append` ansH) rest 


-- rewrite1 runs the supplied rewrite rule and returns the answer, plus
-- the remaining input.
-- If the rewrite fails the failure continuation returns the initial 
-- input as the answer. This means that rewrites must be type-preserving.
rewriteTP1 :: Monad m => RuleTP tok m -> [tok] -> [tok] -> m (D.DList tok,[tok])
rewriteTP1 p xs ys = runMatcherT p succK failK xs ys 
  where
    succK ans _ ts _ = return (ans,ts)
    failK ts         = return (D.fromList ts, []) 


-- rewrite with the Identity monad
rewriteTP'id :: RuleTP tok Identity -> [tok] -> [tok]
rewriteTP'id = (runIdentity .) . rewriteTP




-- type changing rewrite strategy
rewriteTC :: Monad m =>  RuleTC tok out m -> [tok] -> m [out]
rewriteTC f input = step D.empty input where
    step g [] = return $ D.toList g
    step g xs = do (ansH,rest) <- rewriteTC1 f xs xs 
                   step (g `D.append` ansH) rest 


rewriteTC1 :: Monad m 
           => RuleTC tok out m -> [tok] -> [tok] -> m (D.DList out,[tok])
rewriteTC1 p xs ys = runMatcherT p succK failK xs ys 
  where
    succK ans _ ts _ = return (ans,ts)
    failK ts         = error $ "rewriteTC1 didn't consume all input " 
                               ++ show (length ts) 
                               ++ " items remaining."



rewriteTC'id :: RuleTC tok out Identity -> [tok] -> [out]
rewriteTC'id = (runIdentity .) . rewriteTC
