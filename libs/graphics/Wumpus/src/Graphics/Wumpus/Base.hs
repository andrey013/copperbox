{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Base
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Wumpus - Writer Monad PostScript 
--
--------------------------------------------------------------------------------


module Graphics.Wumpus.Base where

import qualified Data.DList as DL
import MonadLib

data PSstate = PSstate { pageNum :: Int,  bbox :: BoundingBox }
  deriving (Eq,Show)


data BoundingBox = 
       BoundingBox { 
                xZero :: Double, 
                yZero :: Double, 
                xOne  :: Double, 
                yOne  :: Double 
       }
  deriving (Eq,Show)              


st0 :: PSstate 
st0 = PSstate 1 $ BoundingBox 0.0 0.0 0.0 0.0


type PSoutput = DL.DList Char

newtype PsT m a = PsT { unPs :: StateT PSstate (WriterT PSoutput m) a }

runPsT :: Monad m => PSstate -> PsT m a -> m (a,PSoutput)
runPsT st m = (runWriterT $ runStateT st (unPs m)) >>= extr
  where
    extr ((a,_st),ps) = return (a,ps)

instance Monad m => Functor (PsT m) where
  fmap f (PsT mf) = PsT $ fmap f mf 

instance Monad m => Monad (PsT m) where
  return a  = PsT $ return a
  ma >>= f  = PsT $ unPs ma >>= unPs . f

instance Monad m => WriterM (PsT m) PSoutput where
  put = PsT . put

instance Monad m => StateM (PsT m) PSstate where
  get = PsT $ get
  set = PsT . set

instance MonadT PsT where
  lift = PsT . lift . lift 


evalPsT :: Monad m => PSstate -> PsT m a -> m PSoutput
evalPsT st m = runPsT st m >>= \(_,ps) -> return ps

pstId :: PSstate -> PsT Id a -> (a,PSoutput)
pstId st m = runId $ runPsT st m  

runWumpus :: PSstate -> PsT Id a -> String
runWumpus = ((DL.toList . snd) .) . pstId

--------------------------------------------------------------------------------
-- writer monad helpers

tell :: WriterM m i => i -> m ()
tell s = puts ((),s)

writeChar :: WriterM m PSoutput => Char -> m ()
writeChar = tell . DL.singleton 


write :: WriterM m PSoutput => String -> m ()
write = tell . DL.fromList 


writeln :: WriterM m PSoutput => String -> m ()
writeln s = write s >> writeChar '\n'


comment :: WriterM m PSoutput => String -> m ()
comment s = write "%%" >> writeln s

type Command = String

command0 :: WriterM m PSoutput => Command -> m ()
command0 cmd = writeln cmd

writeArg :: WriterM m PSoutput => String -> m () 
writeArg s = write s >> writeChar ' '

command1 :: WriterM m PSoutput => Command -> String -> m ()
command1 cmd arg1 = writeArg arg1 >> writeln cmd


command2 :: WriterM m PSoutput => Command -> String -> String -> m ()
command2 cmd arg1 arg2 = 
   writeArg arg1 >> writeArg arg2 >> writeln cmd

command3 :: WriterM m PSoutput => Command -> String -> String -> String -> m ()
command3 cmd arg1 arg2 arg3 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeln cmd

command4 :: WriterM m PSoutput 
         => Command -> String -> String -> String -> String -> m ()
command4 cmd arg1 arg2 arg3 arg4 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeArg arg4 >> writeln cmd

command5 :: WriterM m PSoutput 
         => Command -> String -> String -> String -> String -> String -> m ()
command5 cmd arg1 arg2 arg3 arg4 arg5 = 
   mapM_ writeArg [arg1, arg2, arg3, arg4, arg5 ] >> writeln cmd


getPageNum :: Monad m => PsT m Int
getPageNum = pageNum `fmap` get 


withPage :: Monad m => PsT m a -> PsT m a
withPage m = pageStart >> m >>= \a -> pageEnd >> return a 
  where
    pageStart = getPageNum >>= \i -> comment $ "Page" ++ show i

    pageEnd   = comment "-------------------"    




newpath :: Monad m => PsT m ()
newpath = command0 "newpath"

-- Note - it is preferable to show doubles as 0.0 rather than 0.
-- In PostScript the coercion from int to float is apparently 
-- quite expensive.

moveto :: Monad m => Double -> Double -> PsT m ()
moveto x y = command2 "moveto" (show x) (show y)

rmoveto :: Monad m => Double -> Double -> PsT m ()
rmoveto x y = command2 "rmoveto" (show x) (show y)


lineto :: Monad m => Double -> Double -> PsT m ()
lineto x y = command2 "lineto" (show x) (show y)

rlineto :: Monad m => Double -> Double -> PsT m ()
rlineto x y = command2 "rlineto" (show x) (show y)


arc :: Monad m => Double -> Double -> Double -> Double -> Double -> PsT m ()
arc x y r ang1 ang2 = 
    command5 "arc" (show x) (show y) (show r) (show ang1) (show ang2)

arcn :: Monad m => Double -> Double -> Double -> Double -> Double -> PsT m ()
arcn x y r ang1 ang2 = 
    command5 "arcn" (show x) (show y) (show r) (show ang1) (show ang2)




closepath :: Monad m => PsT m ()
closepath = command0 "closepath" 

clip :: Monad m => PsT m ()
clip = command0 "clip" 

