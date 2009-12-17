{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.Combinators
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Combinators
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.Combinators 
  ( 
    satisfy
  , manyTill
  , count
  , countPrefixed
  , runOn
  , buildWhile

  ) where

import Data.ParserCombinators.Kangaroo.ParseMonad
import Data.ParserCombinators.Kangaroo.Utils

import Control.Applicative
import Data.Word

satisfy :: (Word8 -> Bool) -> GenKangaroo ust Word8
satisfy p = word8 >>= \x -> 
    if p x then return x else reportError $ "satisfy"


manyTill :: GenKangaroo ust a -> GenKangaroo ust b -> GenKangaroo ust [a]
manyTill p end = opt end >>= \ans ->
    case ans of 
      Nothing -> return []
      Just _  -> p <:> manyTill p end


   
count :: Int -> GenKangaroo ust a -> GenKangaroo ust [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 


countPrefixed :: Integral i 
              => GenKangaroo ust i -> GenKangaroo ust a -> GenKangaroo ust (i,[a]) 
countPrefixed plen p = plen >>= \i -> 
    count (fromIntegral i) p >>= \ans -> return (i,ans)


runOn :: GenKangaroo ust a -> GenKangaroo ust [a]
runOn p = atEnd >>= \end -> if end then return [] else  p <:> runOn p

-- | Build a value by while the test holds. When the test fails 
-- the position is not backtracked, instead we use the \"failing\"
-- element with @lastOp@ potentially still building the value 
-- with it.
-- 
buildWhile :: (a -> Bool) 
           -> (a -> b -> b) 
           -> (a -> b -> b) 
           -> b 
           -> GenKangaroo ust a 
           -> GenKangaroo ust b
buildWhile test op lastOp initial p = step where
    step = p >>= \ans -> 
      if test ans then (step >>= \acc -> return $ ans `op` acc)
                  else (return $ ans `lastOp` initial)



-- Can't rely on Alternative as Kangaroo doesn't support it.
{-

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty 
          
between :: Applicative f => f open -> f close -> f a -> f a
between o c a = o *> a <* c

          
option :: Alternative f => a -> f a -> f a
option x p          = p <|> pure x

optionMaybe :: Alternative f => f a -> f (Maybe a)
optionMaybe = optional

-- aka Parsecs /optional/
optionUnit :: Alternative f => f a -> f ()
optionUnit p = () <$ p <|> pure ()

skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

skipMany :: Alternative f => f a -> f ()
skipMany p = many_p
  where many_p = some_p <|> pure ()
        some_p = p       *> many_p

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

-}