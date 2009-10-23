{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PostScript
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


module Wumpus.Core.PostScript where

import Wumpus.Core.Colour 

import qualified Data.DList as DL
import MonadLib

import Data.Monoid
import Data.List ( foldl' )




type PostScript = String

type PsOutput = DL.DList Char

type WumpusM a = PsT Id a



instance Monoid (WumpusM ()) where
  mempty  = return ()
  mappend = (>>) 


newtype PsT m a = PsT { unPs :: ReaderT PsEnv (WriterT PsOutput m) a }

runPsT :: Monad m => PsEnv -> PsT m a -> m (a,PsOutput)
runPsT st m = runWriterT $ runReaderT st (unPs m) 

instance Monad m => Functor (PsT m) where
  fmap f (PsT mf) = PsT $ fmap f mf 

instance Monad m => Monad (PsT m) where
  return a  = PsT $ return a
  ma >>= f  = PsT $ unPs ma >>= unPs . f

instance Monad m => WriterM (PsT m) PsOutput where
  put = PsT . put

instance Monad m => ReaderM (PsT m) PsEnv where
  ask = PsT $ ask

instance Monad m => RunReaderM (PsT m) PsEnv where
  local e mf = PsT $ local e (unPs mf)

instance MonadT PsT where
  lift = PsT . lift . lift 


pstId :: PsEnv -> PsT Id a -> (a,PsOutput)
pstId env m = runId $ runPsT env m  

runWumpus :: PsEnv -> WumpusM a -> String
runWumpus = ((DL.toList . snd) .) . pstId


--------------------------------------------------------------------------------
-- Graphics state datatypes

data PsEnv = PsEnv { 
       cPen         :: Pen,
       cFont        :: Font,
       cColour      :: DRGB
    }
  deriving (Eq,Show)


env0 :: PsEnv 
env0 = PsEnv { 
        cPen        = newPen,
        cFont       = Font "Helvetica" 10,
        cColour     = wumpusBlack
      }



data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

data JoinStyle = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

data DashPattern = Solid | Dash Int [Int]
  deriving (Eq,Show)

data Pen = Pen { 
      lineWidth     :: Double,
      miterLimit    :: Double,
      lineCap       :: LineCap,
      lineJoin      :: JoinStyle,
      dashPattern   :: DashPattern 
    }
  deriving (Eq,Show)



newPen :: Pen
newPen = Pen { lineWidth    = 1.0,          
               miterLimit   = 10.0,
               lineCap      = CapButt, 
               lineJoin     = JoinMiter,    
               dashPattern  = Solid }


data Font = Font { 
      fontName    :: String,
      unitSize    :: Int
    }
  deriving (Eq,Show)



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
comment s = write "%% " >> writeln s

command :: Command -> [String] -> WumpusM ()
command cmd xs = mapM_ writeArg xs >> writeln cmd



showArray :: (a -> ShowS) -> [a] -> String
showArray _ []     = "[ ]"
showArray f (x:xs) = sfun "]" 
  where 
    sfun = foldl' (\a e -> a . (' ':) . f e) (('[':) . f x) xs

showStr :: String -> String 
showStr s = '(' : xs where xs = s++[')']
                              


withPage :: WumpusM a -> WumpusM a
withPage m = pageStart >> m >>= \a -> pageEnd >> return a 
  where
    pageStart = comment $ "Page " ++ show (1::Int)

    pageEnd   = comment "-------------------"    

 
--------------------------------------------------------------------------------
-- graphics state operators 

-- c.f. local of the Reader monad (mapReader in MonadLib). 
-- Checkpoint the state, run the computation restore the state 
-- This pattern is very common in PostScript where the action 
-- is run between @gsave@ and @grestore@.
saveExecRestore :: (PsEnv -> PsEnv) -> WumpusM a -> WumpusM a
saveExecRestore f mf = do 
    command "gsave" []
    a   <- mapReader f mf
    command "grestore" []
    return a


withDiff :: Eq e 
         => (PsEnv -> e) 
         -> (PsEnv -> PsEnv) 
         -> e 
         -> WumpusM () 
         -> WumpusM a 
         -> WumpusM a
withDiff query update e updCmd mf =
  asks query >>= \v -> if (e==v) then mf 
                                 else saveExecRestore update (updCmd >> mf)



localRgbColour :: DRGB -> WumpusM a -> WumpusM a
localRgbColour c mf = 
    withDiff cColour (\e -> e {cColour=c}) c (updColour c) mf
  where
    updColour (RGB3 r g b) = command "setrgbcolor" $ map dtrunc [r,g,b]

localGray :: Double -> WumpusM a -> WumpusM a
localGray gray mf = 
    withDiff cColour (\e -> e {cColour = c}) c (updColour gray) mf
  where
    c = gray2rgb gray
    updColour g = command "setgray" [dtrunc g]


localPen :: Pen -> WumpusM a -> WumpusM a
localPen pen mf = 
    withDiff cPen (\e -> e {cPen=pen}) pen updPen mf 
  where
    updPen = do
        command "setlinewidth"  [dtrunc $ lineWidth pen]
        command "setmiterlimit" [dtrunc $ miterLimit pen]
        command "setlinecap"    [show   $ lineCap pen]
        command "setlinejoin"   [show   $ lineJoin pen]
        setDash (dashPattern pen)  
        
    setDash Solid        = command "setdash" ["[]", "0"]
    setDash (Dash n arr) = command "setdash" [showArray shows arr, show n]


localFont :: Font -> WumpusM a -> WumpusM a 
localFont font mf = 
    withDiff cFont (\e -> e {cFont=font}) font (updFont font) mf
  where
    updFont (Font name sz) = do
        command "findfont" ['/' : name]
        command "scalefont" [show sz]
        command "setfont" []


ps_gsave :: WumpusM ()
ps_gsave = command "gsave" []

ps_grestore :: WumpusM () 
ps_grestore = command "grestore" []

ps_translate :: Double -> Double -> WumpusM ()
ps_translate tx ty = do
    command "translate" $ map dtrunc [tx,ty]

-- Do not use setmatrix for changing the CTM use concat
ps_concat :: Double -> Double -> Double 
          -> Double -> Double -> Double 
          -> WumpusM ()
ps_concat a b c d e f = command "concat" [mat] where 
    mat = showArray ((++) . dtrunc) [a,b,c,d,e,f]

--------------------------------------------------------------------------------
-- Path construction operators




ps_newpath :: WumpusM ()
ps_newpath = command "newpath" []

-- There is no equivalent to PostScript's @currentpoint@ command. 


-- Note - it is preferable to show doubles as 0.0 rather than 0.
-- In PostScript the coercion from int to float is apparently 
-- quite expensive.

ps_moveto :: Double -> Double -> WumpusM ()
ps_moveto x y = command "moveto" [dtrunc x, dtrunc y]

ps_rmoveto :: Double -> Double -> WumpusM ()
ps_rmoveto x y = command "rmoveto" [dtrunc x, dtrunc y]


ps_lineto :: Double -> Double -> WumpusM ()
ps_lineto x y = command "lineto" [dtrunc x, dtrunc y]

ps_rlineto :: Double -> Double -> WumpusM ()
ps_rlineto x y = command "rlineto" [dtrunc x, dtrunc y]


ps_arc :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
ps_arc x y r ang1 ang2 = 
    command "arc" $ map dtrunc [x,y,r,ang1,ang2]

ps_arcn :: Double -> Double -> Double -> Double -> Double -> WumpusM ()
ps_arcn x y r ang1 ang2 = 
    command "arcn" $ map dtrunc [x,y,r,ang1,ang2]


ps_curveto :: Double -> Double -> Double -> Double -> 
                     Double -> Double -> WumpusM ()
ps_curveto x1 y1 x2 y2 x3 y3 = 
    command "curveto" $ map dtrunc [x1,y1, x2,y2, x3,y3]


ps_closepath :: WumpusM ()
ps_closepath = command "closepath" []

ps_clip :: WumpusM ()
ps_clip = command "clip" []

--------------------------------------------------------------------------------
--  painting operators


ps_fill :: WumpusM ()
ps_fill = command "fill" []

ps_stroke :: WumpusM ()
ps_stroke = command "stroke" []

--------------------------------------------------------------------------------
-- Character and font operators


ps_show :: String -> WumpusM ()
ps_show str = command "show" [showStr str]

