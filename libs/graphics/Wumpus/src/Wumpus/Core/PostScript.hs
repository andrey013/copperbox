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

import Wumpus.Core.Geometry ( Frame2(..), Vec2(..), Point2(..), Matrix3'3(..) )
import Wumpus.Core.Utils ( dtrunc )

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


newtype PsT m a = PsT { unPsT :: WriterT PsOutput m a }

runPsT :: Monad m => PsT m a -> m (a,PsOutput)
runPsT m = runWriterT (unPsT m) 

instance Monad m => Functor (PsT m) where
  fmap f (PsT mf) = PsT $ fmap f mf 

instance Monad m => Monad (PsT m) where
  return a  = PsT $ return a
  ma >>= f  = PsT $ unPsT ma >>= unPsT . f

instance Monad m => WriterM (PsT m) PsOutput where
  put = PsT . put

instance MonadT PsT where
  lift = PsT . lift


pstId :: PsT Id a -> (a,PsOutput)
pstId m = runId $ runPsT m  

runWumpus :: WumpusM a -> String
runWumpus = (DL.toList . snd)  . pstId


writePS :: FilePath -> String -> IO ()
writePS filepath pstext = writeFile filepath (bang ++ pstext) 
  where
    bang = "%!PS-Adobe-2.0\n"


--------------------------------------------------------------------------------
-- Graphics state datatypes


data PenAttr = LineWidth   Double
             | MiterLimit  Double
             | LineCap     LineCap
             | LineJoin    LineJoin
             | DashPattern DashPattern 
  deriving (Eq,Show)

data LineCap = CapButt | CapRound | CapSquare
  deriving (Enum,Eq,Show)

data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Enum,Eq,Show)

data DashPattern = Solid | Dash Int [Int]
  deriving (Eq,Show)


-- PostScript (or at least GhostScript) seems to require both
-- attributes (name & size) are set at the same time.

data FontAttr = FontAttr { fontName :: String, fontSize :: Int }
  deriving (Eq,Show)

data PSColour = PSRgb  Double Double Double
              | PSHsb  Double Double Double
              | PSGray Double
  deriving (Eq,Show)

-- | PostScript's current transformation matrix.
-- 
-- PostScript and its documentation considers the matrix to be 
-- in this form:
--
-- > | a  b  0 |
-- > | c  d  0 | 
-- > | tx ty 1 |
-- 
-- i.e it considers the homogeneous coordinates of an affine 
-- frame as /rows/ rather than /columns/ (Wumpus uses rows, as 
-- they were the usual representation in the geometry 
-- presentations that inspired it).
-- 
-- Using the component names that we have used in the 
-- description of 'Frame2', the CTM is:
--
-- > | e0x  e0y  0 |
-- > | e1x  e1y  0 | 
-- > | ox   oy   1 |
-- 
-- The CTM is represented in PostScript as an array, using our 
-- names its layout is
--
-- > [ e0x e0y e1x e1y ox oy ] 
--
-- Some examples, the scaling matrix:
--
-- > | sx 0  0 |
-- > | 0  sy 0 |  = [ sx 0 0 sy 0 0 ]
-- > | 0  0  1 |
-- 
-- Translation (displacement) :
--
-- > | 1  0  0 |
-- > | 0  1  0 |  = [ 1 0 0 1 tx ty ]
-- > | tx ty 1 |
-- 
-- Rotation:
-- 
-- > |  cos(a)  sin(a)  0 |
-- > | -sin(a)  cos(a)  0 |  = [ cos(a) sin(a) -sin(a) cos(a) 0 0 ]
-- > |    0       0     1 |

data CTM = CTM Double Double  Double Double  Double Double
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Conversion to CTM


class ToCTM a where 
  toCTM :: a -> CTM

instance Real a => ToCTM (Frame2 a) where
  toCTM (Frame2 (V2 e0x e0y) (V2 e1x e1y) (P2 ox oy)) 
    = CTM (toD e0x) (toD e0y) (toD e1x) (toD e1y) (toD ox) (toD oy)
 

instance Real a => ToCTM (Matrix3'3 a) where
  toCTM (M3'3 e0x e1x ox  
              e0y e1y oy  
              _   _   _  ) 
    = CTM (toD e0x) (toD e0y) (toD e1x)(toD e1y) (toD ox) (toD oy)

toD :: Real a => a -> Double 
toD = realToFrac

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

ps_comment :: String -> WumpusM ()
ps_comment s = write "%% " >> writeln s

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
    pageStart = ps_comment $ "Page " ++ show (1::Int)

    pageEnd   = ps_comment "-------------------"    


--------------------------------------------------------------------------------
-- graphics state operators


ps_gsave :: WumpusM ()
ps_gsave = command "gsave" []

ps_grestore :: WumpusM () 
ps_grestore = command "grestore" []


ps_setlinewidth :: Double -> WumpusM ()
ps_setlinewidth = command "setlinewidth" . return . dtrunc

ps_setlinecap :: LineCap -> WumpusM ()
ps_setlinecap = command "setlinecap" . return . show . fromEnum

ps_setlinejoin :: LineJoin -> WumpusM ()
ps_setlinejoin = command "setlinejoin" . return . show . fromEnum

ps_setmiterlimit :: Double -> WumpusM ()
ps_setmiterlimit = command "setmiterlimit" . return . dtrunc

ps_setdash :: DashPattern -> WumpusM ()
ps_setdash Solid        = command "setdash" ["[]", "0"]
ps_setdash (Dash n arr) = command "setdash" [showArray shows arr, show n]


ps_setgray :: Double -> WumpusM ()
ps_setgray = command "setgray" . return . dtrunc 


ps_setrgbcolor :: Double -> Double -> Double -> WumpusM ()
ps_setrgbcolor r g b = command "setrgbcolor" $ map dtrunc [r,g,b]

ps_sethsbcolor :: Double -> Double -> Double -> WumpusM ()
ps_sethsbcolor h s b = command "sethsbcolor" $ map dtrunc [h,s,b]


--------------------------------------------------------------------------------
-- coordinate system and matrix operators 

ps_translate :: Double -> Double -> WumpusM ()
ps_translate tx ty = do
    command "translate" $ map dtrunc [tx,ty]

-- Do not use setmatrix for changing the CTM use concat
ps_concat :: CTM -> WumpusM ()
ps_concat (CTM a b  c d  e f) = command "concat" [mat] where 
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

ps_findfont :: String -> WumpusM () 
ps_findfont = command "findfont" . return . ('/' :)

ps_scalefont :: Int -> WumpusM ()
ps_scalefont = command "scalefont" . return . show

ps_setfont :: WumpusM ()
ps_setfont = command "setfont" []

ps_show :: String -> WumpusM ()
ps_show str = command "show" [showStr str]

