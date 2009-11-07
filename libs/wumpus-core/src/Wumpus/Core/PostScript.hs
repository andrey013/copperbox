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


module Wumpus.Core.PostScript 
  (
  -- * Types
    PostScript
  , WumpusM

  , runWumpus

  -- * Emit PostScript 
  , ps_comment
  
  , ps_gsave
  , ps_grestore
  , ps_setlinewidth
  , ps_setlinecap
  , ps_setlinejoin
  , ps_setmiterlimit
  , ps_setdash
  , ps_setgray
  , ps_setrgbcolor
  , ps_sethsbcolor
  , ps_translate
  , ps_scale
  , ps_concat
  , ps_newpath
  , ps_moveto
  , ps_rmoveto
  , ps_lineto
  , ps_rlineto
  , ps_arc
  , ps_arcn
  , ps_curveto
  , ps_closepath
  , ps_clip
  , ps_fill
  , ps_stroke
  , ps_showpage
  , ps_findfont
  , ps_scalefont
  , ps_setfont
  , ps_show
  , bang_PS
  , bang_EPS
  , dsc_comment
  , dsc_BoundingBox
  , dsc_CreationDate
  , dsc_Pages
  , dsc_Page
  , dsc_EndComments
  , dsc_EOF

  ) where

import Wumpus.Core.GraphicsState
import Wumpus.Core.Utils ( dtrunc, roundup, parens )

import qualified Data.DList as DL
import MonadLib

import Data.Monoid
import Data.List ( foldl', intersperse )




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
                              


--------------------------------------------------------------------------------
-- graphics state operators

-- | @ gsave @
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

ps_scale :: Double -> Double -> WumpusM ()
ps_scale tx ty = do
    command "scale" $ map dtrunc [tx,ty]



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
-- Output operators

ps_showpage :: WumpusM ()
ps_showpage = command "showpage" []




--------------------------------------------------------------------------------
-- Character and font operators

-- | The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-Bolditalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- List from Bill Casselman \'Mathematical Illustrations\' p279.

ps_findfont :: String -> WumpusM () 
ps_findfont = command "findfont" . return . ('/' :)

ps_scalefont :: Int -> WumpusM ()
ps_scalefont = command "scalefont" . return . show

ps_setfont :: WumpusM ()
ps_setfont = command "setfont" []

ps_show :: String -> WumpusM ()
ps_show str = command "show" [parens str]

--------------------------------------------------------------------------------
-- document structuring conventions

-- | @ %!PS-Adobe-3.0 @
bang_PS :: WumpusM ()
bang_PS = writeln "%!PS-Adobe-3.0"

-- | @ %!PS-Adobe-3.0 EPSF-3.0 @
bang_EPS :: WumpusM ()
bang_EPS = writeln "%!PS-Adobe-3.0 EPSF-3.0"

dsc_comment :: String -> [String] -> WumpusM ()
dsc_comment name [] = write "%%" >> writeln name
dsc_comment name xs = write "%%" >> write name >> write ": " >> writeln body
  where body = concat (intersperse " " xs)

-- | @ %%BoundingBox: ... ... ... ... @  /llx lly urx ury/
dsc_BoundingBox :: Double -> Double -> Double -> Double -> WumpusM ()
dsc_BoundingBox llx lly urx ury = 
  dsc_comment "BoundingBox"  (map roundup [llx,lly,urx,ury])

-- | @ %%CreationDate: ... @
-- 
-- The creation date is informational and never interpreted, 
-- thus the format is entirely arbitrary.
dsc_CreationDate :: String -> WumpusM ()
dsc_CreationDate = dsc_comment "CreationDate" . return

-- | @ %%Pages: ... @
dsc_Pages :: Int -> WumpusM ()
dsc_Pages = dsc_comment "Pages" . return . show


-- | @ %%Page: ... ... @
dsc_Page :: String -> Int -> WumpusM ()
dsc_Page label ordinal = 
    dsc_comment "Page" [label, show ordinal]


-- | @ %%EndComments @
dsc_EndComments :: WumpusM ()
dsc_EndComments = dsc_comment "EndComments" []

-- | @ %%EOF @
dsc_EOF :: WumpusM ()
dsc_EOF = dsc_comment "EOF" []

