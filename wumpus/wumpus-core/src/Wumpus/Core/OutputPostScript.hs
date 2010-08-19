{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputPostScript
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output PostScript - either PostScript (PS) files or 
-- EPS (Encapusulated PostScript) files can be generated. 
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputPostScript 
  ( 
  -- * Output PostScript
    writePS
  , writeEPS
  
  , writePS_latin1
  , writeEPS_latin1

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList ( OneList, ViewL(..), viewl )
import Wumpus.Core.PictureInternal
import Wumpus.Core.PostScript
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextEncodingInternal
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils


import Control.Monad


--------------------------------------------------------------------------------
-- Render to PostScript

-- | Output a series of pictures to a Postscript file. Each 
-- picture will be printed on a separate page. 
--
writePS :: (Real u, Floating u, PSUnit u) 
        => FilePath -> TextEncoder -> [Picture u] -> IO ()
writePS filepath enc pic = do 
    timestamp <- mkTimeStamp
    writeFile filepath $ psDraw timestamp enc pic

-- | Output a picture to an EPS (Encapsulated PostScript) file. 
-- The .eps file can then be imported or embedded in another 
-- document.
--
writeEPS :: (Real u, Floating u, PSUnit u)  
         => FilePath -> TextEncoder -> Picture u -> IO ()
writeEPS filepath enc pic = do
    timestamp <- mkTimeStamp
    writeFile filepath $ epsDraw timestamp enc pic


-- | Version of 'writePS' - using Latin1 encoding. 
writePS_latin1 :: (Real u, Floating u, PSUnit u) 
        => FilePath -> [Picture u] -> IO ()
writePS_latin1 filepath = writePS filepath latin1Encoder 

-- | Version of 'writeEPS' - using Latin1 encoding. 
writeEPS_latin1 :: (Real u, Floating u, PSUnit u)  
         => FilePath -> Picture u -> IO ()
writeEPS_latin1 filepath = writeEPS filepath latin1Encoder



--------------------------------------------------------------------------------
-- Internals


-- | Draw a picture, generating PostScript output.
psDraw :: (Real u, Floating u, PSUnit u) 
       => String -> TextEncoder -> [Picture u] -> PostScript
psDraw timestamp enc pics = execPsMonad enc $ do
    psHeader 1 timestamp
    zipWithM_ psDrawPage pages pics
    psFooter
  where
    pages = map (\i -> (show i,i)) [1..]


psDrawPage :: (Real u, Floating u, PSUnit u) 
           => (String,Int) -> Picture u -> PsMonad ()
psDrawPage (lbl,ordinal) pic = do
    dsc_Page lbl ordinal
    ps_gsave
    cmdtrans
    outputPicture pic
    ps_grestore
    ps_showpage
  where
    (_,mbv)   = repositionProperties pic
    cmdtrans  = maybe (return ()) (\(V2 x y) -> ps_translate x y) mbv
  


-- | Note the bounding box may /below the origin/ - if it is, it 
-- will need translating.
--
epsDraw :: (Real u, Floating u, PSUnit u) 
        => String -> TextEncoder -> Picture u -> PostScript
epsDraw timestamp enc pic = execPsMonad enc $ do 
    epsHeader bb timestamp      
    ps_gsave
    cmdtrans
    outputPicture pic
    ps_grestore
    epsFooter  
  where
    (bb,mbv)  = repositionProperties pic
    cmdtrans  = maybe (return ()) (\(V2 x y) -> ps_translate x y) mbv
     

psHeader :: Int -> String -> PsMonad ()
psHeader pagecount timestamp = do
    bang_PS
    dsc_Pages pagecount
    dsc_CreationDate $ parens timestamp
    dsc_EndComments


epsHeader :: PSUnit u => BoundingBox u -> String -> PsMonad ()
epsHeader bb timestamp = do
    bang_EPS
    dsc_BoundingBox llx lly urx ury
    dsc_CreationDate $ parens timestamp
    dsc_EndComments
  where
    (llx,lly,urx,ury) = getBounds bb

getBounds :: Num u => BoundingBox u -> (u,u,u,u)
getBounds (BBox (P2 llx lly) (P2 urx ury)) = (llx,lly,urx,ury)

psFooter :: PsMonad ()
psFooter = dsc_EOF


epsFooter :: PsMonad ()
epsFooter = do
    ps_showpage
    dsc_EOF

-- Create margins at the left and bottom of 4 points...


-- | outputPicture 
-- Frame changes, representing scalings translation, rotations...
-- are drawn when they are encountered as a @concat@ statement in a 
-- block of @gsave ... grestore@.
--
outputPicture :: (Real u, Floating u, PSUnit u) => Picture u -> PsMonad ()
outputPicture (PicBlank  _)             = return ()

outputPicture (Leaf (fr,_) _ ones)      = 
    updateFrame fr (revMapM outputPrimitive ones)

-- output right picture first, to satisfy zordering...
outputPicture (Picture (fr,_) ones)      =
    updateFrame fr (revMapM outputPicture ones) 

outputPicture (Clip (fr,_) cp p)        = 
    updateFrame fr (clipPath cp >> outputPicture p)




revMapM ::  (a -> PsMonad ()) -> OneList a -> PsMonad ()
revMapM mf = step . viewl
  where
    step (OneL a)  = mf a
    step (a :< xs) = step (viewl xs) >> mf a


-- | @updateFrame@ relies on the current frame, when translated
-- to a matrix being invertible.
--
-- This is an allowable optimization because the current frame
-- is only manipulated with the affine transformations (scalings, 
-- rotations...) which are invertible. 
-- 
-- It also performs another optimization:
--
-- If the frame is the standard frame @ [1 0 0 1 0 0] @ then 
-- the monadic action is run as-is rather than being nested
-- in a block:
-- 
-- > [1 0 0 1 0 0] concat
-- > ...
-- > [1 0 0 1 0 0] concat
--

updateFrame :: (Fractional u, PSUnit u) => Frame2 u -> PsMonad () -> PsMonad ()
updateFrame frm ma 
  | standardFrame frm = ma
  | otherwise         = let m1 = frame2Matrix frm in 
                        do { ps_concat $ toCTM m1
                           ; ma 
                           ; ps_concat $ toCTM $ invert m1
                           }

outputPrimitive :: (Real u, Floating u, PSUnit u) => Primitive u -> PsMonad ()
outputPrimitive (PPath (c,dp) p)    = outputPath dp c p 
outputPrimitive (PLabel props l)    = updateFont props $ outputLabel l
outputPrimitive (PEllipse (c,dp) e) = outputEllipse dp c e

updateFont :: LabelProps -> PsMonad () -> PsMonad ()
updateFont (c,fnt) ma = updateColour c $ do 
    mb_fnt <- deltaFontAttr fnt
    maybe (return ()) fontCommand mb_fnt
    ma
    


updateColour :: PSColour c => c -> PsMonad () -> PsMonad ()
updateColour c ma = let rgbc = psColour c in do 
    mb_col  <- deltaRgbColour rgbc
    maybe (return ()) colourCommand mb_col
    ma
  where
    colourCommand :: DRGB -> PsMonad ()
    colourCommand (RGB3 r g b) = ps_setrgbcolor r g b
 
    


fontCommand :: FontAttr -> PsMonad ()
fontCommand (FontAttr sz face) = do
    ps_findfont (font_name face)
    ps_scalefont sz
    ps_setfont



    
outputPath :: (PSColour c, PSUnit u) 
           => DrawPath -> c -> Path u -> PsMonad ()
outputPath CFill        c p = updateColour c $ do  
    startPath p
    ps_closepath
    ps_fill

outputPath (CStroke xs) c p = updatePen c xs $ do
    startPath p
    ps_closepath
    ps_stroke

outputPath (OStroke xs) c p = updatePen c xs $ do
    startPath p
    ps_stroke
  

startPath :: PSUnit u => Path u -> PsMonad ()
startPath (Path (P2 x y) xs) = do
    ps_newpath
    ps_moveto x y
    mapM_ outputPathSeg xs



clipPath :: PSUnit u => Path u -> PsMonad ()
clipPath p = do 
    startPath p
    ps_closepath
    ps_clip



updatePen :: PSColour c => c -> [StrokeAttr] -> PsMonad () -> PsMonad ()
updatePen c xs ma = let (mset, mreset) = strokeSetReset xs in 
                    updateColour c $ do { mset ; ma ; mreset }

strokeSetReset :: [StrokeAttr] -> (PsMonad (), PsMonad ())
strokeSetReset = 
    foldr (\c acc -> link (cmd c) acc) (return (), return ())
  where
    link Nothing      funs    = funs 
    link (Just (f,g)) (fs,gs) = (fs >> f, gs >> g)
    
    mkSetReset mf        = maybe Nothing (\(a,b) -> Just (mf a, mf b))
    
    cmd (LineWidth d)    = mkSetReset ps_setlinewidth  $ deltaStrokeWidth d
    cmd (MiterLimit d)   = mkSetReset ps_setmiterlimit $ deltaMiterLimit d
    cmd (LineCap lc)     = mkSetReset ps_setlinecap    $ deltaLineCap lc
    cmd (LineJoin lj)    = mkSetReset ps_setlinejoin   $ deltaLineJoin lj
    cmd (DashPattern dp) = mkSetReset ps_setdash       $ deltaDashPattern dp


outputPathSeg :: PSUnit u => PathSegment u -> PsMonad ()
outputPathSeg (PLineTo (P2 x y))  = ps_lineto x y
outputPathSeg (PCurveTo p1 p2 p3) = ps_curveto x1 y1 x2 y2 x3 y3 
  where
    P2 x1 y1 = p1
    P2 x2 y2 = p2
    P2 x3 y3 = p3


-- | This is unfortunate - but (probably) unavoidable.
--
-- The use of PostScript's @concat@ operator will vary the line 
-- width during the drawing of a stroked ellipse.
--
outputEllipse :: (PSColour c, Real u, Floating u, PSUnit u)
              => DrawEllipse -> c -> PrimEllipse u -> PsMonad ()
outputEllipse dp c (PrimEllipse pt@(P2 x y) hw hh ctm)
    | hw==hh  && ctm == identityCTM = outputArc dp c x y hw
    | otherwise                     = 
          let matrix     =  matrixRepCTM $ scaleCTM 1 (hh/hw) ctm
              matrix'    = invert matrix
              (P2 dx dy) = matrix' *# pt 
          in do { ps_concat $ toCTM matrix
                ; outputArc dp c dx dy hw
                ; ps_concat $ toCTM $ matrix'
                }

outputArc :: (PSColour c, PSUnit u) 
          => DrawEllipse -> c -> u -> u -> u -> PsMonad ()
outputArc EFill        c x y r = updateColour c $ do 
    ps_newpath
    ps_arc x y r 0 360 
    ps_closepath
    ps_fill

outputArc (EStroke xs) c x y r = updatePen c xs $ do 
    ps_newpath
    ps_arc x y r 0 360 
    ps_closepath
    ps_stroke


-- Note - for the otherwise case the x-and-y coordinates are 
-- encoded in the matrix, hence the @ 0 0 moveto @.
--
outputLabel :: (Real u, Floating u, PSUnit u) => Label u -> PsMonad ()
outputLabel (Label (P2 x y) entxt ctm) 
    | ctm == identityCTM  = do { ps_moveto x y; outputEncodedText entxt }
    | otherwise           = do { ps_concat $ toCTM matrix
                               ; ps_moveto 0 (0 :: Double)
                               ; outputEncodedText entxt
                               ; ps_concat $ toCTM $ invert matrix
                               }
  where
    matrix = translMatrixRepCTM x y ctm

outputEncodedText :: EncodedText -> PsMonad () 
outputEncodedText = mapM_ outputTextChunk . getEncodedText

outputTextChunk :: TextChunk -> PsMonad () 
outputTextChunk (SText s)  = ps_show $ escapeStringPS s

outputTextChunk (EscInt i) = 
    ask >>= \env -> maybe (failk env) ps_glyphshow $ lookupByCharCode i env
  where
    failk = missingCode i . ps_fallback  

outputTextChunk (EscStr s) = ps_glyphshow s 

missingCode :: CharCode -> GlyphName -> PsMonad ()
missingCode i fallback =  do
    ps_comment $ "missing lookup for &#" ++ show i ++ ";" 
    ps_glyphshow fallback
            

