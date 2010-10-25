{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.ToyFontMetrics.ParserCombinators
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Two continuation parser combinators.
-- 
--------------------------------------------------------------------------------

module Graphics.ToyFontMetrics.ParserCombinators
  where

import Control.Applicative
import Control.Monad
import Data.Char


data Result s ans = Fail [s] | Okay ans [s]
  deriving (Eq,Ord,Show)


type SK s r ans = r -> FK s ans -> [s] -> Result s ans
type FK s ans   = Result s ans


newtype Parser s r = Parser { 
          getParser :: forall ans. SK s r ans -> FK s ans -> [s] -> Result s ans }


runParser :: Parser s a -> [s] -> Result s a
runParser p = getParser p skZero fkZero
  where
    skZero = \ans _ ss -> Okay ans ss
    fkZero = Fail []


failure :: Parser s a
failure = Parser $ \_ fk _ -> fk

yield  :: a -> Parser s a
yield a = Parser $ \sk fk ss -> sk a fk ss

-- mplus of MonadPlus, (<|>) of Applicative.
--
alt :: Parser s a -> Parser s a -> Parser s a
alt p q = Parser $ \sk fk ss -> 
            getParser p (\r _ -> sk r fk) (getParser q sk fk ss) ss

infixl 5 `apply`

apply :: Functor f => f a -> (a -> b) -> f b
apply = flip fmap

-- eager-many / zero-or-more (many of Applicative)
--
eagerMany :: Parser s a -> Parser s [a]
eagerMany p = (p >>= \ r -> eagerMany p `apply` \rs -> (r:rs)) <|> return [] 

-- eager-some / one-or-more (some of Applicative)
--
eagerSome :: Parser s a -> Parser s [a]
eagerSome p = p >>= \ r -> eagerMany p `apply` \rs -> (r:rs)


instance Functor (Parser s) where
  fmap f mf = Parser $ \sk -> getParser mf $ \a -> (sk . f) a
     


instance Applicative (Parser s) where
  pure = yield
  (<*>) = ap

instance Alternative (Parser s) where
  empty = failure
  (<|>) = alt
  many  = eagerMany
  some  = eagerSome
 
instance Monad (Parser s) where
  return  = yield
  m >>= k = Parser $ \sk -> getParser m $ \a -> getParser (k a) sk

instance MonadPlus (Parser s) where
  mzero = failure
  mplus = alt



--------------------------------------------------------------------------------
-- Combinators

symbol :: Eq s => s -> Parser s s
symbol sym = Parser go
  where
    go sk fk (s:ss) | s == sym = sk s fk ss
    go _  fk _                 = fk


satisfy :: (s -> Bool) -> Parser s s
satisfy test = Parser go
  where
    go sk fk (s:ss) | test s = sk s fk ss 
    go _  fk _               = fk

any :: Parser s s
any = Parser go
  where
   go sk fk (s:ss) = sk s fk ss
   go _  fk _      = fk

oneOf           :: Eq s => [s] -> Parser s s
oneOf cs        = satisfy (`elem` cs)

noneOf          :: Eq s => [s] -> Parser s s
noneOf cs       = satisfy (`notElem` cs)



-- Note - the type sigs of the chain parsers can be generalized 
-- to any MonadPlus.
--

chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p op = p >>= rest 
  where 
    rest x = mplus (op >>= \f -> p >>= \a -> rest (f x a)) (return x) 
               

chainr1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr1 p op = scan 
   where 
     scan   = p >>= rest 
     rest x = mplus (op >>= \f -> scan >>= \a -> rest (f x a)) (return x) 

chainl :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p op v = mplus (chainl1 p op) (return v)

chainr :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr p op v = mplus (chainr1 p op) (return v)


infixr 5 <:> 
 
-- | Applicative cons.
--
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

-- | 'many1' an alias for Control.Applicative 'some'. 
--
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
    step = p <:> ((end `apply` pure[]) <|> step)

manyTill1 :: Alternative f => f a -> f b -> f [a]
manyTill1 p end = p <:> step where
    step = (end `apply` pure []) <|> (p <:> step)
    


--------------------------------------------------------------------------------
-- Char parsers

type CharParser a = Parser Char a


char :: Char -> CharParser Char
char ch = satisfy (==ch)


digit :: CharParser Char
digit = satisfy isDigit 


-- Note - lexeme doesn\'t gobble newlines
--
lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* many1 (oneOf "\t ")
