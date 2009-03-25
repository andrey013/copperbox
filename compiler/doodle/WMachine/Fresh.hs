

module Fresh where

import Control.Applicative
import qualified Data.Map as Map

type Gen = Int -- (Int,Map.Map String Int)

newtype Fresh a = Fresh { runFresh :: Gen -> (a, Gen) }

instance Functor Fresh where 
  fmap f m = Fresh $ \i -> let (a,i') = runFresh m i in (f a,i')

instance Applicative Fresh where
  pure v  = Fresh $ \gen -> (v,gen)
  f <*> a = Fresh $ \gen -> let (v,gen')  = runFresh f gen
                                (w,gen'') = runFresh a gen'
                              in (v w, gen'')

instance Monad Fresh where
  return = pure
  m >>= k = Fresh $ \gen -> let (v,gen')  = runFresh m gen
                                (w,gen'') = runFresh (k v) gen'
                            in (w, gen'')

                              
freshName :: Fresh Int                
freshName = Fresh $ \i -> (i,i+1)




          
freshNames :: Int -> Fresh [Int]
freshNames = count `flip` freshName     

{-
rename :: String -> Fresh Int
rename s = Fresh $ \(i,d) -> maybe (fk i d) (sk i d) $ Map.lookup s d
  where
    fk i d    = (i,(i+1, Map.insert s i d))
    sk i d n  = (n,(i,d))
-}

evalFresh :: (Fresh a) -> a
evalFresh f = fst $ runFresh f 0

--------------------------------------------------------------------------------
-- combinators

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)


(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2

sequenceA       :: Applicative f => [f a] -> f [a] 
{-# INLINE sequenceA #-}
sequenceA ms = foldr (<:>) (pure []) ms


count :: Applicative f => Int -> f a -> f [a]
count n f = sequenceA (replicate n f)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = count





