

module S1.Fresh where

import Control.Applicative
import qualified Data.Map as Map

type Gen = (Int,Map.Map String Int)

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
freshName = Fresh $ \(i,d) -> (i, (i+1,d))

rename :: String -> Fresh Int
rename s = Fresh $ \(i,d) -> maybe (fk i d) (sk i d) $ Map.lookup s d
  where
    fk i d    = (i,(i+1, Map.insert s i d))
    sk i d n  = (n,(i,d))

evalFresh :: (Fresh a) -> a
evalFresh c = fst $ runFresh c (0, Map.empty)


{-

demo1   = evalFresh ((,) <$> freshName <*> freshName)
demo1a  = runFresh ((,) <$> freshName <*> freshName) (0, Map.empty) 

demo2 = runFresh ((,,) <$> rename "s" <*> rename "t" <*> rename "s") (0, Map.empty) 


-}