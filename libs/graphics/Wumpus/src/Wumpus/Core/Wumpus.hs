{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Wumpus
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


module Wumpus.Core.Wumpus where

import Wumpus.Core.Colour 
import qualified Wumpus.Core.CTM as CTM
import Wumpus.Core.Vector

import qualified Data.DList as DL
import MonadLib

-- TODO some graphical objects (e.g. arrows) will need to know
-- the currentpoint. (??) 


data PsState = PsState { 
       pageNum      :: Int,  
       bBox         :: BoundingBox,
       cTM          :: CTM.PsMatrix,
       cColour      :: Colour3,
       cLineWidth   :: Double
    }
  deriving (Eq,Show)


data BoundingBox = BoundingBox { 
       xZero        :: Double, 
       yZero        :: Double, 
       xOne         :: Double, 
       yOne         :: Double 
    }
  deriving (Eq,Show)              



st0 :: PsState 
st0 = PsState { 
        pageNum     = 1,
        bBox        = BoundingBox 0.0 0.0 0.0 0.0,
        cTM         = CTM.initmatrix,
        cColour     = wumpusBlack,
        cLineWidth  = 1
      }

type PsOutput = DL.DList Char

type WumpusM a = PsT Id a


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

runWumpus :: PsState -> WumpusM a -> String
runWumpus = ((DL.toList . snd) .) . pstId


writePS :: FilePath -> String -> IO ()
writePS filepath pstext = writeFile filepath (bang ++ pstext) 
  where
    bang = "%!PS-Adobe-2.0\n"

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


writeArg :: WriterM m PsOutput => String -> m () 
writeArg s = write s >> writeChar ' '



type Command = String

comment :: String -> WumpusM ()
comment s = write "%%" >> writeln s


command0 :: Command -> WumpusM ()
command0 cmd = writeln cmd

command1 :: Command -> String -> WumpusM ()
command1 cmd arg1 = writeArg arg1 >> writeln cmd


command2 :: Command -> String -> String -> WumpusM ()
command2 cmd arg1 arg2 = 
   writeArg arg1 >> writeArg arg2 >> writeln cmd

command3 :: Command -> String -> String -> String -> WumpusM ()
command3 cmd arg1 arg2 arg3 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeln cmd

command4 :: Command -> String -> String -> String -> String -> WumpusM ()
command4 cmd arg1 arg2 arg3 arg4 = 
   writeArg arg1 >> writeArg arg2 >> writeArg arg3 >> writeArg arg4 >> writeln cmd

command5 :: Command -> String -> String -> String -> String -> String -> WumpusM ()
command5 cmd arg1 arg2 arg3 arg4 arg5 = 
   mapM_ writeArg [arg1, arg2, arg3, arg4, arg5 ] >> writeln cmd


getPageNum :: WumpusM Int
getPageNum = pageNum `fmap` get 


getCTM :: WumpusM  CTM.PsMatrix
getCTM = cTM `fmap` get


withPage :: WumpusM a -> WumpusM a
withPage m = pageStart >> m >>= \a -> pageEnd >> return a 
  where
    pageStart = getPageNum >>= \i -> comment $ "Page" ++ show i

    pageEnd   = comment "-------------------"    


--------------------------------------------------------------------------------
-- graphics state operators 

-- c.f. local of the Reader monad. 
-- Checkpoint the state, run the computation restore the state 
-- This pattern is very common in PostScript where the action 
-- is run between @gsave@ and @grestore@.
saveExecRestore :: WumpusM a -> WumpusM a
saveExecRestore m = do 
    command0 "gsave"
    st  <- get
    a   <- m
    set st
    command0 "grestore"
    return a

gsave :: WumpusM ()
gsave = command0 "gsave"

grestore :: WumpusM ()
grestore = command0 "grestore"



setlinewidth :: Double -> WumpusM ()
setlinewidth n = command1 "setlinewidth" (show n) 


setlinecap :: Int -> WumpusM ()
setlinecap i = command1 "setlinecap" (show i) 

setlinejoin :: Int -> WumpusM ()
setlinejoin i = command1 "setlinejoin" (show i) 

setmiterlimit :: Double -> WumpusM ()
setmiterlimit n = command1 "setmiterlimit" (show n) 


setgray :: Double -> WumpusM ()
setgray n = command1 "setgray" (show n)

setColour :: Colour3 -> WumpusM ()
setColour c = sets_ (\s -> s {cColour = c} )


sethsbcolor :: Double -> Double -> Double -> WumpusM ()
sethsbcolor h s b = do 
    setColour $ hsb2rgb' h s b
    command3 "sethsbcolor" (show h) (show s) (show b)


setrgbcolor :: Double -> Double -> Double -> WumpusM ()
setrgbcolor r g b = do 
  setColour $ V3 r g b
  command3 "setrgbcolor" (show r) (show g) (show b)


--------------------------------------------------------------------------------
-- matrix operations


updateCTM :: (CTM.PsMatrix -> CTM.PsMatrix) -> WumpusM ()
updateCTM fn = getCTM >>= \ctm -> sets_ (\s -> s {cTM = fn ctm} )

-- emit the postscript and shadow the matrix transformation

translate :: Double -> Double -> WumpusM ()
translate tx ty = do
    command2 "translate" (show tx) (show ty)
    updateCTM $ \ctm -> CTM.translate tx ty ctm


scale :: Double -> Double -> WumpusM ()
scale sx sy = do
    command2 "scale" (show sx) (show sy)
    updateCTM $ \ctm -> CTM.scale sx sy ctm
         

rotate :: Double -> WumpusM ()
rotate ang = do
    command1 "rotate" (show ang)
    updateCTM $ \ctm -> CTM.rotate ang ctm

concat :: CTM.PsMatrix -> WumpusM ()
concat matrix = do 
    command1 "concat" (CTM.printmatrix matrix)
    updateCTM $ \ctm -> ctm `CTM.multiply` matrix

--------------------------------------------------------------------------------
-- Path construction operators




newpath :: WumpusM ()
newpath = command0 "newpath"

-- There is no equivalent to PostScript's @currentpoint@ command. 


-- Note - it is preferable to show doubles as 0.0 rather than 0.
-- In PostScript the coercion from int to float is apparently 
-- quite expensive.

moveto :: Double -> Double -> WumpusM ()
moveto x y = command2 "moveto" (show x) (show y)

rmoveto :: Double -> Double -> WumpusM ()
rmoveto x y = command2 "rmoveto" (show x) (show y)


lineto :: Double -> Double -> WumpusM ()
lineto x y = command2 "lineto" (show x) (show y)

rlineto :: Double -> Double -> WumpusM ()
rlineto x y = command2 "rlineto" (show x) (show y)


arc :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
arc x y r ang1 ang2 = 
    command5 "arc" (show x) (show y) (show r) (show ang1) (show ang2)

arcn :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
arcn x y r ang1 ang2 = 
    command5 "arcn" (show x) (show y) (show r) (show ang1) (show ang2)




closepath :: WumpusM ()
closepath = command0 "closepath" 

clip :: WumpusM ()
clip = command0 "clip" 

--------------------------------------------------------------------------------
--  painting operators


erasepage :: WumpusM () 
erasepage = command0 "erasepage"

fill :: WumpusM ()
fill = command0 "fill"

stroke :: WumpusM ()
stroke = command0 "stroke"

