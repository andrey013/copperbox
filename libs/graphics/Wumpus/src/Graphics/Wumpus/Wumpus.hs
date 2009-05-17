{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Wumpus.Wumpus
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


module Graphics.Wumpus.Wumpus where

import qualified Graphics.Wumpus.Matrix as CTM

import qualified Data.DList as DL
import MonadLib

data PsState = PsState { 
       pageNum      :: Int,  
       bBox         :: BoundingBox,
       cTM          :: CTM.PsMatrix
    }
  deriving (Eq,Show)


data BoundingBox = BoundingBox { 
       xZero       :: Double, 
       yZero       :: Double, 
       xOne        :: Double, 
       yOne        :: Double 
    }
  deriving (Eq,Show)              


st0 :: PsState 
st0 = PsState { 
        pageNum  = 1,
        bBox     = BoundingBox 0.0 0.0 0.0 0.0,
        cTM      = CTM.initmatrix
      }

type PsOutput = DL.DList Char

newtype PsT m a = PsT { unPs :: StateT PsState (WriterT PsOutput m) a }

runPsT :: Monad m => PsState -> PsT m a -> m (a,PsOutput)
runPsT st m = (runWriterT $ runStateT st (unPs m)) >>= extr
  where
    extr ((a,_st),ps) = return (a,ps)

instance Monad m => Functor (PsT m) where
  fmap f (PsT mf) = PsT $ fmap f mf 

instance Monad m => Monad (PsT m) where
  return a  = PsT $ return a
  ma >>= f  = PsT $ unPs ma >>= unPs . f

instance Monad m => WriterM (PsT m) PsOutput where
  put = PsT . put

instance Monad m => StateM (PsT m) PsState where
  get = PsT $ get
  set = PsT . set

instance MonadT PsT where
  lift = PsT . lift . lift 


evalPsT :: Monad m => PsState -> PsT m a -> m PsOutput
evalPsT st m = runPsT st m >>= \(_,ps) -> return ps

pstId :: PsState -> PsT Id a -> (a,PsOutput)
pstId st m = runId $ runPsT st m  

runWumpus :: PsState -> PsT Id a -> String
runWumpus = ((DL.toList . snd) .) . pstId

--------------------------------------------------------------------------------
-- writer monad helpers

tell :: WriterM m i => i -> m ()
tell s = puts ((),s)

writeChar :: WriterM m PsOutput => Char -> m ()
writeChar = tell . DL.singleton 


write :: WriterM m PsOutput => String -> m ()
write = tell . DL.fromList 


writeln :: WriterM m PsOutput => String -> m ()
writeln s = write s >> writeChar '\n'


comment :: WriterM m PsOutput => String -> m ()
comment s = write "%%" >> writeln s

type Command = String

command0 :: WriterM m PsOutput => Command -> m ()
command0 cmd = writeln cmd

writeArg :: WriterM m PsOutput => String -> m () 
writeArg s = write s >> writeChar ' '

command1 :: WriterM m PsOutput => Command -> String -> m ()
command1 cmd arg1 = writeArg arg1 >> writeln cmd


command2 :: WriterM m PsOutput => Command -> String -> String -> m ()
command2 cmd arg1 arg2 = 
   writeArg arg1 >> writeArg arg2 >> writeln cmd

command3 :: WriterM m PsOutput => Command -> String -> String -> String -> m ()
command3 cmd arg1 arg2 arg3 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeln cmd

command4 :: WriterM m PsOutput 
         => Command -> String -> String -> String -> String -> m ()
command4 cmd arg1 arg2 arg3 arg4 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeArg arg4 >> writeln cmd

command5 :: WriterM m PsOutput 
         => Command -> String -> String -> String -> String -> String -> m ()
command5 cmd arg1 arg2 arg3 arg4 arg5 = 
   mapM_ writeArg [arg1, arg2, arg3, arg4, arg5 ] >> writeln cmd


getPageNum :: Monad m => PsT m Int
getPageNum = pageNum `fmap` get 


getCTM :: Monad m => PsT m CTM.PsMatrix
getCTM = cTM `fmap` get


withPage :: Monad m => PsT m a -> PsT m a
withPage m = pageStart >> m >>= \a -> pageEnd >> return a 
  where
    pageStart = getPageNum >>= \i -> comment $ "Page" ++ show i

    pageEnd   = comment "-------------------"    


--------------------------------------------------------------------------------
-- graphics state operators 

setlinewidth :: Monad m => Double -> PsT m ()
setlinewidth n = command1 "setlinewidth" (show n) 


setlinecap :: Monad m => Int -> PsT m ()
setlinecap i = command1 "setlinecap" (show i) 

setlinejoin :: Monad m => Int -> PsT m ()
setlinejoin i = command1 "setlinejoin" (show i) 

setmiterlimit :: Monad m => Double -> PsT m ()
setmiterlimit n = command1 "setmiterlimit" (show n) 


setgray :: Monad m => Double -> PsT m ()
setgray n = command1 "setgray" (show n)

sethsbcolor :: Monad m => Double -> Double -> Double -> PsT m ()
sethsbcolor h s b = command3 "sethsbcolor" (show h) (show s) (show b)


setrgbcolor :: Monad m => Double -> Double -> Double -> PsT m ()
setrgbcolor r g b = command3 "setrgbcolor" (show r) (show g) (show b)


--------------------------------------------------------------------------------
-- matrix operations


updateCTM :: Monad m => (CTM.PsMatrix -> CTM.PsMatrix) -> PsT m ()
updateCTM fn = getCTM >>= \ctm -> sets_ (\s -> s {cTM = fn ctm} )

-- emit the postscript and shadow the matrix transformation

translate :: Monad m => Double -> Double -> PsT m ()
translate tx ty = do
    command2 "translate" (show tx) (show ty)
    updateCTM $ \ctm -> CTM.translate tx ty ctm


scale :: Monad m => Double -> Double -> PsT m ()
scale sx sy = do
    command2 "scale" (show sx) (show sy)
    updateCTM $ \ctm -> CTM.scale sx sy ctm
         

rotate :: Monad m => Double -> PsT m ()
rotate ang = do
    command1 "rotate" (show ang)
    updateCTM $ \ctm -> CTM.rotate ang ctm

concat :: Monad m => CTM.PsMatrix -> PsT m ()
concat matrix = do 
    command1 "concat" (CTM.printmatrix matrix)
    updateCTM $ \ctm -> ctm `CTM.multiply` matrix

--------------------------------------------------------------------------------
-- Path construction operators


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

--------------------------------------------------------------------------------
--  painting operators

fill :: Monad m => PsT m ()
fill = command0 "fill"

stroke :: Monad m => PsT m ()
stroke = command0 "stroke"

