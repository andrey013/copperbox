{-# LANGUAGE EmptyDataDecls             #-}

module DummyPMonad where

import Control.Monad.Writer

data NoPath
data OpenPath
data ClosedPath         

tellln :: String ->Writer String ()
tellln s = tell (s ++ "\n")

newtype PWriter p q a = PWriter { unPWriter :: Writer String a }

newpath :: PWriter NoPath OpenPath ()
newpath = PWriter $ tellln "newpath"

lineto :: Double -> Double -> PWriter OpenPath OpenPath ()
lineto x y = PWriter $ tellln $ show x ++ " " ++ show y ++ " lineto"

closepath :: PWriter OpenPath ClosedPath ()
closepath = PWriter $ tellln "closepth"

stroke :: PWriter ClosedPath NoPath ()
stroke = PWriter $ tellln "stroke"

fill :: PWriter ClosedPath NoPath ()
fill = PWriter $ tellln "fill"


class PMonad m where
  unit :: a -> m p p a
  bind :: m p q a -> (a -> m q r b) -> m p r b
  next :: m p q a -> m q r b -> m p r b
  
  next m k = m `bind` \_ -> k


instance PMonad PWriter where
  unit = PWriter . return
  bind m f = PWriter (unPWriter m >>= unPWriter . f)

   

p1 = newpath `next` lineto 10 10 `next` lineto 20 20 `next` closepath `next` stroke

runPW :: PWriter p q a -> (a,String)
runPW (PWriter f) = runWriter f

{-

-- typechecking fails, stroking an OpenPath

p2 = newpath `next` lineto 10 10 `next` lineto 20 20  `next` stroke

-}