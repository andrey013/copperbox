{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.PostScript
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


module Wumpus.Drawing.PostScript where

import Wumpus.Core.Colour 
import Wumpus.Drawing.GraphicsState

import qualified Data.DList as DL
import MonadLib

import Data.List ( foldl' )

type PostScript = String

data PsState = PsState { 
       pageNum      :: Int,  
       cPen         :: Pen,
       cFont        :: Font,
       cColour      :: DRGB
    }
  deriving (Eq,Show)


st0 :: PsState 
st0 = PsState { 
        pageNum     = 1,
        cPen        = newPen,
        cFont       = helvetica 10,
        cColour     = wumpusBlack
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

-- Some effort is made to optimize the PostScript.
-- When the graphics state changes, only show the parts that change.


whenDiff :: Eq a => a -> a -> WumpusM () -> WumpusM ()
whenDiff a b dk = if a==b then dk else return ()

class WumpusDiff a where
  wumpusDiff :: a -> a -> WumpusM ()

instance WumpusDiff DRGB where
  wumpusDiff new@(RGB3 r g b) old = whenDiff new old (ps_setrgbcolor r g b)

instance WumpusDiff Font where 
  wumpusDiff new@(Font nn nsz) old@(Font on osz) =
      whenDiff new old (diffName nn on >> diffSize nsz osz >> ps_setfont)
    where
      diffName n o = whenDiff n o (do { (Font _ sz) <- cFont `fmap` get
                                      ; sets_ (\s -> s {cFont = Font n sz} )
                                      ;  ps_findfont n })

      diffSize n o = whenDiff n o (do { (Font nm _) <- cFont `fmap` get
                                      ; sets_ (\s -> s {cFont = Font nm n} )
                                      ; ps_scalefont n })


instance WumpusDiff Pen where
  wumpusDiff new@(Pen nw nc nj nd) old@(Pen ow oc oj od) = 
    whenDiff new old (do { sets_ (\s -> s { cPen = new})
                         ; diffWidth nw ow
                         ; diffCap nc oc
                         ; diffJoin nj oj
                         ; diffDash nd od })
    where
      diffWidth n o = whenDiff n o (ps_setlinewidth n)
      diffCap n o   = whenDiff n o (ps_setlinecap $ fromEnum n)
      diffJoin n o  = whenDiff n o (ps_setlinejoin $ fromEnum n)
      diffDash n o  = whenDiff n o (setDash n)
      
      setDash Solid       = ps_setdash [] 0 
      setDash (Dash x xs) = ps_setdash xs x


withPen :: Pen -> WumpusM a -> WumpusM a
withPen p mf = do 
    old <- cPen `fmap` get
    if p==old then mf else saveExecRestore (wumpusDiff p old >> mf)

withFont :: Font -> WumpusM a -> WumpusM a
withFont ft mf = do 
    old <- cFont `fmap` get
    if ft==old then mf else saveExecRestore (wumpusDiff ft old >> mf)


withColour :: DRGB -> WumpusM a -> WumpusM a
withColour c@(RGB3 r g b) mf = do
    old <- cColour `fmap` get
    if c==old then mf else saveExecRestore (ps_setrgbcolor r g b >> mf)

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


dtrunc :: Double -> String
dtrunc d | abs d < 0.0001  = "0.0"
         | d < 0.0           = '-' :  show (abs tx)
         | otherwise         = show tx
  where
    tx :: Double
    tx = (realToFrac (roundi (d*1000000.0))) / 1000000.0
 
    roundi :: RealFrac a => a -> Integer
    roundi = round


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

command6 :: Command -> String -> String -> String -> 
                       String -> String -> String -> WumpusM ()
command6 cmd arg1 arg2 arg3 arg4 arg5 arg6 = 
   mapM_ writeArg [arg1, arg2, arg3, arg4, arg5, arg6 ] >> writeln cmd


showArray :: (a -> ShowS) -> [a] -> String
showArray _ []     = "[ ]"
showArray f (x:xs) = sfun "]" 
  where 
    sfun = foldl' (\a e -> a . (' ':) . f e) (('[':) . f x) xs

showStr :: String -> String 
showStr s = '(' : xs where xs = s++[')']
                              
getPageNum :: WumpusM Int
getPageNum = pageNum `fmap` get 



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

ps_gsave :: WumpusM ()
ps_gsave = command0 "gsave"

ps_grestore :: WumpusM ()
ps_grestore = command0 "grestore"



ps_setlinewidth :: Double -> WumpusM ()
ps_setlinewidth n = command1 "setlinewidth" (show n) 

-- 0 = butt, 1 = round, 2 = square
ps_setlinecap :: Int -> WumpusM ()
ps_setlinecap i = command1 "setlinecap" (show i) 

ps_setlinejoin :: Int -> WumpusM ()
ps_setlinejoin i = command1 "setlinejoin" (show i) 

ps_setmiterlimit :: Double -> WumpusM ()
ps_setmiterlimit n = command1 "setmiterlimit" (show n) 


ps_setdash :: [Int] -> Int -> WumpusM ()
ps_setdash arr n = command2 "setdash" (showArray shows arr) (show n)

ps_setgray :: Double -> WumpusM ()
ps_setgray n = command1 "setgray" (dtrunc n)

setColour :: DRGB -> WumpusM ()
setColour c = sets_ (\s -> s {cColour = c} )


ps_sethsbcolor :: Double -> Double -> Double -> WumpusM ()
ps_sethsbcolor h s b = do 
    setColour $ hsb2rgb' h s b
    command3 "sethsbcolor" (dtrunc h) (dtrunc s) (dtrunc b)


ps_setrgbcolor :: Double -> Double -> Double -> WumpusM ()
ps_setrgbcolor r g b = do 
  setColour $ RGB3 r g b
  command3 "setrgbcolor" (dtrunc r) (dtrunc g) (dtrunc b)





rgb2hsb' :: Double -> Double -> Double -> DHSB
rgb2hsb' r g b = rgb2hsb $ (RGB3 r g b)

hsb2rgb' :: Double -> Double -> Double -> DRGB
hsb2rgb' h s b = hsb2rgb $ (HSB3 h s b)

rgb2gray' :: Double -> Double -> Double -> Double
rgb2gray' r g b = rgb2gray $ (RGB3 r g b) 


--------------------------------------------------------------------------------
-- matrix operations

ps_translate :: Double -> Double -> WumpusM ()
ps_translate tx ty = do
    command2 "translate" (show tx) (show ty)


ps_scale :: Double -> Double -> WumpusM ()
ps_scale sx sy = do
    command2 "scale" (show sx) (show sy)
         

ps_rotate :: Double -> WumpusM ()
ps_rotate ang = do
    command1 "rotate" (show ang)



--------------------------------------------------------------------------------
-- Path construction operators




ps_newpath :: WumpusM ()
ps_newpath = command0 "newpath"

-- There is no equivalent to PostScript's @currentpoint@ command. 


-- Note - it is preferable to show doubles as 0.0 rather than 0.
-- In PostScript the coercion from int to float is apparently 
-- quite expensive.

ps_moveto :: Double -> Double -> WumpusM ()
ps_moveto x y = command2 "moveto" (dtrunc x) (dtrunc y)

ps_rmoveto :: Double -> Double -> WumpusM ()
ps_rmoveto x y = command2 "rmoveto" (dtrunc x) (dtrunc y)


ps_lineto :: Double -> Double -> WumpusM ()
ps_lineto x y = command2 "lineto" (dtrunc x) (dtrunc y)

ps_rlineto :: Double -> Double -> WumpusM ()
ps_rlineto x y = command2 "rlineto" (show x) (show y)


ps_arc :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
ps_arc x y r ang1 ang2 = 
    command5 "arc" (show x) (show y) (show r) (show ang1) (show ang2)

ps_arcn :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
ps_arcn x y r ang1 ang2 = 
    command5 "arcn" (show x) (show y) (show r) (show ang1) (show ang2)


ps_curveto :: Double -> Double -> Double -> Double -> 
                     Double -> Double -> WumpusM ()
ps_curveto x1 y1 x2 y2 x3 y3 = 
    command6 "curveto" (dtrunc x1) (dtrunc y1) (dtrunc x2) (dtrunc y2)
                       (dtrunc x3) (dtrunc y3)


ps_closepath :: WumpusM ()
ps_closepath = command0 "closepath" 

ps_clip :: WumpusM ()
ps_clip = command0 "clip" 

--------------------------------------------------------------------------------
--  painting operators


ps_erasepage :: WumpusM () 
ps_erasepage = command0 "erasepage"

ps_fill :: WumpusM ()
ps_fill = command0 "fill"

ps_stroke :: WumpusM ()
ps_stroke = command0 "stroke"

--------------------------------------------------------------------------------
-- Character and font operators


ps_findfont :: String -> WumpusM ()
ps_findfont key = command1 "findfont" ('/' : key)

ps_setfont :: WumpusM ()
ps_setfont = command0 "setfont"

ps_show :: String -> WumpusM ()
ps_show = command1 "show" . showStr

ps_scalefont :: Double -> WumpusM () 
ps_scalefont sc = command1 "scalefont" (dtrunc sc)
