{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PostScript
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- PostScript generation via a Writer monad.
--
-- PostScript is emitted line by line - there is no abstract
-- syntax tree representing PostScript. So we use a writer 
-- monad.
---
--------------------------------------------------------------------------------


module Wumpus.Core.PostScript 
  (
  -- * Types
    PostScript
  , PsMonad

  , execPsMonad
  , ask

  -- * Escape sepcial characters
  , escapeStringPS

  -- * Deltas 
  , deltaFontAttr
  , deltaRgbColour

  , deltaStrokeWidth
  , deltaMiterLimit
  , deltaLineCap
  , deltaLineJoin
  , deltaDashPattern
 
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
  , ps_glyphshow
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

import Wumpus.Core.Colour
import Wumpus.Core.GraphicsState
import Wumpus.Core.TextEncoder
import Wumpus.Core.Utils

import Control.Applicative
import Data.List ( foldl' )


-- Graphics state for PostScript Rendering
--
-- Values with no default value (e.g. font) in the graphics 
-- state of the PostScript interpreter (not Wumpus\'s renderer) 
-- are Maybes.
-- 
-- The graphics state is considered successive - all elements 
-- have a colour and all text labels have a font. So during 
-- processing there are two situations: 

-- (1) If font or colour is the same as the last no state
-- change needs to be printed.
--
-- (2) If the font or colour changes the update needs to be 
-- printed, but as the next element always has a colour (and a
-- font if it is a label), no @undo@ needs to be printed.
--
-- This contrasts with the behaviour for stroke attributes
-- which needs @undo@.

data St = St 
      { st_font         :: Maybe FontAttr
      , st_rgb_colour   :: DRGB
      }
  deriving (Eq,Show)



-- | Stroke properties do not have to be fully specified in 
-- Wumpus\'s picture types - i.e. a Path might have it\'s stroke 
-- width set but nothing else.
--
-- If a path changes any of the stroke properties, it 
-- immediately undoes the changes after drawing, returning the 
-- the stroke values to their PostScript defaults.
--
-- This means Wumpus doesn't have to carry a nested environment 
-- around as it renders to PostScript. As stroke properties can
-- only be assigned to leaves in the picture tree, a nested 
-- environment wouldn\'t really be an ideal fit anyway.


gs_stroke_width :: Double
gs_stroke_width = 1.0

gs_miter_limit  :: Double
gs_miter_limit  = 10.0

gs_line_cap     :: LineCap
gs_line_cap     = CapSquare

gs_line_join    :: LineJoin
gs_line_join    = JoinMiter

gs_dash_pattern :: DashPattern
gs_dash_pattern = Solid



type PostScript = String

type PsOutput = H Char


-- WumpusM is a Reader-Writer-State monad.
--
newtype PsMonad a = PsMonad { 
            getPsMonad :: TextEncoder -> St -> (a, St, PsOutput) }


st_zero :: St 
st_zero = St { st_font           = Nothing
             , st_rgb_colour     = black 
             }
           
            
runPsMonad :: TextEncoder -> PsMonad a -> (a, St, PsOutput)
runPsMonad enc m = getPsMonad m enc st_zero

instance Functor PsMonad where
  fmap f mf = PsMonad $ \r s -> let (a,s',w) = getPsMonad mf r s 
                                in (f a, s', w)

instance Applicative PsMonad where
  pure a    = PsMonad $ \_ s -> (a,s,id)
  mf <*> ma = PsMonad $ \r s -> let (f,s1,w1) = getPsMonad mf r s
                                    (a,s2,w2) = getPsMonad ma r s1
                                in (f a, s2, w1 . w2)

instance Monad PsMonad where
  return a  = PsMonad $ \_ s -> (a,s,id)
  m >>= k   = PsMonad $ \r s -> let (a,s1,w1) = getPsMonad m r s
                                    (b,s2,w2) = (getPsMonad . k) a r s1
                                in (b, s2, w1 . w2)


-- | Drop state and result, take the Writer trace.
execPsMonad :: TextEncoder -> PsMonad a -> PostScript
execPsMonad enc mf = let (_,_,w) = runPsMonad enc mf in toListH w


get :: PsMonad St
get = PsMonad $ \_ s -> (s,s,id)

sets_ :: (St -> St) -> PsMonad ()
sets_ f = PsMonad $ \_ s -> ((), f s, id)

ask :: PsMonad TextEncoder
ask = PsMonad $ \r s -> (r,s,id)

--------------------------------------------------------------------------------
-- Escape special chars

-- | Escape these characters:
--
-- > \\ - (, ), <, >, [, ], {, }, /, and %
--
escapeStringPS :: String -> String
escapeStringPS = foldr f "" where
  f c ss | c `elem` ps_special = '\\' : c : ss
         | otherwise           = c : ss

ps_special :: [Char]
ps_special = "\\()<>[]{}/%"


--------------------------------------------------------------------------------
-- "Deltas" of the graphics state

deltaFontAttr :: FontAttr -> PsMonad (Maybe FontAttr)
deltaFontAttr new = get >>= maybe update diff . st_font
  where
    update :: PsMonad (Maybe FontAttr)
    update = sets_ (\s -> s { st_font = Just new }) >> return (Just new)
    
    diff :: FontAttr -> PsMonad (Maybe FontAttr)
    diff old | old == new = return Nothing
             | otherwise  = update


deltaRgbColour :: DRGB -> PsMonad (Maybe DRGB)
deltaRgbColour new = get >>= diff . st_rgb_colour
  where
    diff :: DRGB -> PsMonad (Maybe DRGB)
    diff old | old == new = return Nothing
             | otherwise  = do { sets_ (\s -> s { st_rgb_colour = new })
                               ; return (Just new)
                               }


deltaStrokeWidth :: Double -> Maybe (Double,Double)
deltaStrokeWidth n
    | n == gs_stroke_width = Nothing
    | otherwise            = Just (n,gs_stroke_width)

deltaMiterLimit :: Double -> Maybe (Double,Double)
deltaMiterLimit n 
    | n == gs_miter_limit  = Nothing
    | otherwise            = Just (n,gs_miter_limit)


deltaLineCap :: LineCap -> Maybe (LineCap,LineCap)
deltaLineCap lc
    | lc == gs_line_cap    = Nothing
    | otherwise            = Just (lc,gs_line_cap)

deltaLineJoin :: LineJoin -> Maybe (LineJoin,LineJoin)
deltaLineJoin lj 
    | lj == gs_line_join   = Nothing
    | otherwise            = Just (lj,gs_line_join)

deltaDashPattern :: DashPattern -> Maybe (DashPattern,DashPattern)
deltaDashPattern p 
    | p == gs_dash_pattern = Nothing
    | otherwise            = Just (p,gs_dash_pattern)



--------------------------------------------------------------------------------
-- writer monad helpers

tell :: H Char -> PsMonad ()
tell msg = PsMonad $ \_ s -> ((),s,msg)

writeChar :: Char -> PsMonad ()
writeChar = tell . showChar 


write :: String -> PsMonad ()
write = tell . showString 


writeln :: String -> PsMonad ()
writeln s = write s >> writeChar '\n'


writeArg :: String -> PsMonad () 
writeArg s = write s >> writeChar ' '




type Command = String

command :: Command -> [String] -> PsMonad ()
command cmd xs = mapM_ writeArg xs >> writeln cmd



showArray :: (a -> ShowS) -> [a] -> String
showArray _ []     = "[ ]"
showArray f (x:xs) = sfun "]" 
  where 
    sfun = foldl' (\a e -> a . (' ':) . f e) (('[':) . f x) xs
                              


-- | @ %% ... @
--
ps_comment :: String -> PsMonad ()
ps_comment s = write "%% " >> writeln s

--------------------------------------------------------------------------------
-- graphics state operators

-- | @ gsave @
--
ps_gsave :: PsMonad ()
ps_gsave = command "gsave" []

-- | @ grestore @
--
ps_grestore :: PsMonad () 
ps_grestore = command "grestore" []

-- | @ ... setlinewidth @
--
ps_setlinewidth :: PSUnit u => u -> PsMonad ()
ps_setlinewidth = command "setlinewidth" . return . dtrunc

-- | @ ... setlinecap @
--
ps_setlinecap :: LineCap -> PsMonad ()
ps_setlinecap = command "setlinecap" . return . show . fromEnum

-- | @ ... setlinejoin @
--
ps_setlinejoin :: LineJoin -> PsMonad ()
ps_setlinejoin = command "setlinejoin" . return . show . fromEnum

-- | @ ... setmiterlimit @
--
ps_setmiterlimit :: PSUnit u => u -> PsMonad ()
ps_setmiterlimit = command "setmiterlimit" . return . dtrunc

-- | @ [... ...] ... setdash @
--
ps_setdash :: DashPattern -> PsMonad ()
ps_setdash Solid          = command "setdash" ["[]", "0"]
ps_setdash (Dash n pairs) = command "setdash" [showArray shows arr, show n]
  where
    arr = foldr (\(x,y) a -> x:y:a) [] pairs

-- | @ ... setgray @
--
ps_setgray :: PSUnit u => u -> PsMonad ()
ps_setgray = command "setgray" . return . dtrunc 

-- | @ ... ... ... setrgbcolor @
--
ps_setrgbcolor :: PSUnit u => u -> u -> u -> PsMonad ()
ps_setrgbcolor r g b = command "setrgbcolor" $ map dtrunc [r,g,b]

-- | @ ... ... ... sethsbcolor @
--
ps_sethsbcolor :: PSUnit u => u -> u -> u -> PsMonad ()
ps_sethsbcolor h s b = command "sethsbcolor" $ map dtrunc [h,s,b]


--------------------------------------------------------------------------------
-- coordinate system and matrix operators 

-- | @ ... ... translate @
ps_translate :: PSUnit u => u -> u -> PsMonad ()
ps_translate tx ty = do
    command "translate" $ map dtrunc [tx,ty]

-- | @ ... ... scale @
ps_scale :: PSUnit u => u -> u -> PsMonad ()
ps_scale tx ty = do
    command "scale" $ map dtrunc [tx,ty]


-- Do not use setmatrix for changing the CTM use concat...

-- | @ [... ... ... ... ... ...] concat @
ps_concat :: PSUnit u => CTM u -> PsMonad ()
ps_concat (CTM a b  c d  e f) = command "concat" [mat] where 
    mat = showArray ((++) . dtrunc) [a,b,c,d,e,f]


--------------------------------------------------------------------------------
-- Path construction operators

-- | @ newpath @
ps_newpath :: PsMonad ()
ps_newpath = command "newpath" []


-- Note - it is preferable to show doubles as 0.0 rather than 0.
-- In PostScript the coercion from int to float is apparently 
-- quite expensive.

-- | @ ... ... moveto @
ps_moveto :: PSUnit u => u -> u -> PsMonad ()
ps_moveto x y = command "moveto" [dtrunc x, dtrunc y]

-- | @ ... ... rmoveto @
ps_rmoveto :: PSUnit u => u -> u -> PsMonad ()
ps_rmoveto x y = command "rmoveto" [dtrunc x, dtrunc y]

-- | @ ... ... lineto @
ps_lineto :: PSUnit u => u -> u -> PsMonad ()
ps_lineto x y = command "lineto" [dtrunc x, dtrunc y]

-- | @ ... ... rlineto @
ps_rlineto :: PSUnit u => u -> u -> PsMonad ()
ps_rlineto x y = command "rlineto" [dtrunc x, dtrunc y]

-- | @ ... ... ... ... ... arc @
ps_arc :: PSUnit u => u -> u -> u -> u -> u -> PsMonad ()
ps_arc x y r ang1 ang2 = 
    command "arc" $ map dtrunc [x,y,r,ang1,ang2]

-- | @ ... ... ... ... ... arcn @
ps_arcn :: PSUnit u => u -> u -> u -> u -> u -> PsMonad ()
ps_arcn x y r ang1 ang2 = 
    command "arcn" $ map dtrunc [x,y,r,ang1,ang2]

-- | @ ... ... ... ... ... ... curveto @
ps_curveto :: PSUnit u => u -> u -> u -> u -> u -> u -> PsMonad ()
ps_curveto x1 y1 x2 y2 x3 y3 = 
    command "curveto" $ map dtrunc [x1,y1, x2,y2, x3,y3]

-- | @ closepath @
ps_closepath :: PsMonad ()
ps_closepath = command "closepath" []

-- | @ clip @
ps_clip :: PsMonad ()
ps_clip = command "clip" []

--------------------------------------------------------------------------------
--  painting operators

-- | @ fill @
ps_fill :: PsMonad ()
ps_fill = command "fill" []

-- | @ stroke @
ps_stroke :: PsMonad ()
ps_stroke = command "stroke" []


--------------------------------------------------------------------------------
-- Output operators

-- | @ showpage @
ps_showpage :: PsMonad ()
ps_showpage = command "showpage" []



--------------------------------------------------------------------------------
-- Character and font operators

-- | The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-BoldItalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- List from Bill Casselman \'Mathematical Illustrations\' p279.

-- | @ /... findfont @
ps_findfont :: String -> PsMonad () 
ps_findfont = command "findfont" . return . ('/' :)

-- | @ ... scalefont @
ps_scalefont :: Int -> PsMonad ()
ps_scalefont = command "scalefont" . return . show

-- | @ setfont @
ps_setfont :: PsMonad ()
ps_setfont = command "setfont" []

-- | @ (...) show  @
ps_show :: String -> PsMonad ()
ps_show = command "show" . return . parens

-- | @ (...) show  @
ps_glyphshow :: String -> PsMonad ()
ps_glyphshow = command "glyphshow" . return . ('/':)


--------------------------------------------------------------------------------
-- document structuring conventions

-- | @ %!PS-Adobe-3.0 @
bang_PS :: PsMonad ()
bang_PS = writeln "%!PS-Adobe-3.0"

-- | @ %!PS-Adobe-3.0 EPSF-3.0 @
bang_EPS :: PsMonad ()
bang_EPS = writeln "%!PS-Adobe-3.0 EPSF-3.0"

-- | @ %%...: ... @
dsc_comment :: String -> [String] -> PsMonad ()
dsc_comment name [] = write "%%" >> writeln name
dsc_comment name xs = write "%%" >> write name >> write ": " >> writeln (hsep xs)


-- | @ %%BoundingBox: ... ... ... ... @  /llx lly urx ury/
dsc_BoundingBox :: PSUnit u => u -> u -> u -> u -> PsMonad ()
dsc_BoundingBox llx lly urx ury = 
  dsc_comment "BoundingBox"  (map (roundup . toDouble) [llx,lly,urx,ury])

-- | @ %%CreationDate: ... @
-- 
-- The creation date is informational and never interpreted, 
-- thus the format is entirely arbitrary.
dsc_CreationDate :: String -> PsMonad ()
dsc_CreationDate = dsc_comment "CreationDate" . return

-- | @ %%Pages: ... @
dsc_Pages :: Int -> PsMonad ()
dsc_Pages = dsc_comment "Pages" . return . show


-- | @ %%Page: ... ... @
dsc_Page :: String -> Int -> PsMonad ()
dsc_Page label ordinal = 
    dsc_comment "Page" [label, show ordinal]


-- | @ %%EndComments @
dsc_EndComments :: PsMonad ()
dsc_EndComments = dsc_comment "EndComments" []

-- | @ %%EOF @
dsc_EOF :: PsMonad ()
dsc_EOF = dsc_comment "EOF" []

