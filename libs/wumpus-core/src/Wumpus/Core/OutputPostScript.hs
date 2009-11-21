{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputPostScript
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputPostScript 
  ( 
  -- * Output PostScript
  -- $fontdoc
    writePS
  , writeEPS
  
  , writePS_latin1
  , writeEPS_latin1

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.PostScript
import Wumpus.Core.TextEncoding
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils

import MonadLib hiding ( Label )

import Control.Monad ( mapM_, zipWithM_ )






-- $fontdoc
--
-- The following fonts are expected to exist on most platforms:
--
-- > Times-Roman  Times-Italic  Times-Bold  Times-Bolditalic
-- > Helvetica  Helvetica-Oblique  Helvetica-Bold  Helvetica-Bold-Oblique
-- > Courier  Courier-Oblique  Courier-Bold  Courier-Bold-Oblique
-- > Symbol
--
-- See the PostScript Language Reference Manual.

-- type FontSpec = (String,Int)



--------------------------------------------------------------------------------
-- Render to PostScript

-- | Write a series of pictures to a Postscript file. Each 
-- picture will be printed on a separate page. 
--
-- If the picture contains text labels, you should provide a 
-- FontSpec to transmit @findfont@, @scalefont@ etc. commands 
-- to PostScript.    
writePS :: (Fractional u, Ord u, PSUnit u) 
        => FilePath -> TextEncoder -> [Picture u] -> IO ()
writePS filepath enc pic = do 
    timestamp <- mkTimeStamp
    writeFile filepath $ psDraw timestamp enc pic

-- | Write a picture to an EPS (Encapsulated PostScript) file. 
-- The .eps file can then be imported or embedded in another 
-- document.
--
-- If the picture contains text labels, you should provide a 
-- FontSpec to transmit @findfont@, @scalefont@ etc. commands 
-- to PostScript.    
writeEPS :: (Fractional u, Ord u, PSUnit u)  
         => FilePath -> TextEncoder -> Picture u -> IO ()
writeEPS filepath enc pic = do
    timestamp <- mkTimeStamp
    writeFile filepath $ epsDraw timestamp enc pic


-- | Version of 'writePS' - using Latin1 encoding. 
writePS_latin1 :: (Fractional u, Ord u, PSUnit u) 
        => FilePath -> [Picture u] -> IO ()
writePS_latin1 filepath = writePS filepath latin1Encoder 

-- | Version of 'writeEPS' - using Latin1 encoding. 
writeEPS_latin1 :: (Fractional u, Ord u, PSUnit u)  
         => FilePath -> Picture u -> IO ()
writeEPS_latin1 filepath = writeEPS filepath latin1Encoder









-- | Draw a picture, generating PostScript output.
psDraw :: (Fractional u, Ord u, PSUnit u) 
       => String -> TextEncoder -> [Picture u] -> PostScript
psDraw timestamp enc pics = runWumpus enc $ do
    psHeader 1 timestamp
    zipWithM_ psDrawPage pages pics
    psFooter
  where
    pages = map (\i -> (show i,i)) [1..]


psDrawPage :: (Fractional u, Ord u, PSUnit u) 
           => (String,Int) -> Picture u -> WumpusM ()
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
  


-- Note the bounding box may have negative components - if it 
-- does it will need translating.

epsDraw :: (Fractional u, Ord u, PSUnit u) 
        => String -> TextEncoder -> Picture u -> PostScript
epsDraw timestamp enc pic = runWumpus enc $ do 
    epsHeader bb timestamp      
    ps_gsave
    cmdtrans
    outputPicture pic
    ps_grestore
    epsFooter  
  where
    (bb,mbv)  = repositionProperties pic
    cmdtrans  = maybe (return ()) (\(V2 x y) -> ps_translate x y) mbv
     

psHeader :: Int -> String -> WumpusM ()
psHeader pagecount timestamp = do
    bang_PS
    dsc_Pages pagecount
    dsc_CreationDate $ parens timestamp
    dsc_EndComments


epsHeader :: PSUnit u => BoundingBox u -> String -> WumpusM ()
epsHeader bb timestamp = do
    bang_EPS
    dsc_BoundingBox llx lly urx ury
    dsc_CreationDate $ parens timestamp
    dsc_EndComments
  where
    (llx,lly,urx,ury) = getBounds bb

getBounds :: Num u => BoundingBox u -> (u,u,u,u)
getBounds (BBox (P2 llx lly) (P2 urx ury)) = (llx,lly,urx,ury)

psFooter :: WumpusM ()
psFooter = dsc_EOF


epsFooter :: WumpusM ()
epsFooter = do
    ps_showpage
    dsc_EOF

-- Create margins at the left and bottom of 4 points...


-- | outputPicture 
-- Frame changes, representing scalings translation, rotations...
-- are drawn when they are encountered as a @concat@ statement in a 
-- block of @gsave ... grestore@.

outputPicture :: (Fractional u, PSUnit u) => Picture u -> WumpusM ()
outputPicture (PicBlank  _)             = return ()
outputPicture (Single (fr,_) prim)      = 
    updateFrame fr $ outputPrimitive prim
outputPicture (Picture (fr,_) ones)      = do
    updateFrame fr $ onesmapM_ outputPicture  ones
outputPicture (Clip (fr,_) cp p)        = 
    updateFrame fr $ do { clipPath cp ; outputPicture p }


-- | @updateFrame@ performs one optimization:
-- 
-- If the frame is the standard frame @ [1 0 0 1 0 0] @ then 
-- the monadic action is run as-is rather than being nested
-- in a block:
-- 
-- > gsave 
-- > [1 0 0 1 0 0] concat
-- > ...
-- > grestore
--

updateFrame :: PSUnit u => Frame2 u -> WumpusM () -> WumpusM ()
updateFrame frm ma 
  | standardFrame frm = ma
  | otherwise         = do { ps_gsave
                           ; ps_concat $ toCTM frm
                           ; ma
                           ; ps_grestore 
                           }



outputPrimitive :: (Fractional u, PSUnit u) => Primitive u -> WumpusM ()
outputPrimitive (PPath (c,dp) p)           = outputPath dp c p 
outputPrimitive (PLabel props l)           = updateFont props $ outputLabel l
outputPrimitive (PEllipse (c,dp) ct hw hh) = outputEllipse dp c ct hw hh

updateFont :: LabelProps -> WumpusM () -> WumpusM ()
updateFont (c,fnt) ma = do 
    ps_gsave
    colourCommand c
    fontCommand fnt
    ma
    ps_grestore
    


updateColour :: PSColour c => c -> WumpusM () -> WumpusM ()
updateColour c ma = do 
    ps_gsave
    colourCommand c
    ma
    ps_grestore
    


fontCommand :: FontAttr -> WumpusM ()
fontCommand (FontAttr name _ _ sz) = do
    ps_findfont name
    ps_scalefont sz
    ps_setfont


colourCommand :: PSColour c => c -> WumpusM ()
colourCommand = cmd . psColour
  where
    cmd (RGB3 r g b) = ps_setrgbcolor r g b

    
outputPath :: (PSColour c, PSUnit u) => DrawProp -> c -> Path u -> WumpusM ()
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
  

startPath :: PSUnit u => Path u -> WumpusM ()
startPath (Path (P2 x y) xs) = do
    ps_newpath
    ps_moveto x y
    mapM_ outputPathSeg xs



clipPath :: PSUnit u => Path u -> WumpusM ()
clipPath p = do 
    startPath p
    ps_closepath
    ps_clip



updatePen :: PSColour c => c -> [StrokeAttr] -> WumpusM () -> WumpusM ()
updatePen c xs ma = do 
    ps_gsave
    colourCommand c
    mapM_ penCommand xs
    ma
    ps_grestore

penCommand :: StrokeAttr -> WumpusM ()
penCommand (LineWidth d)    = ps_setlinewidth d
penCommand (MiterLimit d)   = ps_setmiterlimit d
penCommand (LineCap lc)     = ps_setlinecap lc
penCommand (LineJoin lj)    = ps_setlinejoin lj
penCommand (DashPattern dp) = ps_setdash dp


outputPathSeg :: PSUnit u => PathSegment u -> WumpusM ()
outputPathSeg (PLine (P2 x y))  = ps_lineto x y
outputPathSeg (PCurve p1 p2 p3) = ps_curveto x1 y1 x2 y2 x3 y3 
  where
    P2 x1 y1 = p1
    P2 x2 y2 = p2
    P2 x3 y3 = p3

-- | This is not very good as it uses a PostScript's
-- @scale@ operator - this will vary the line width during the
-- drawing of a stroked ellipse.
outputEllipse :: (PSColour c, Fractional u, PSUnit u)
              => DrawEllipse -> c -> Point2 u -> u -> u -> WumpusM ()
outputEllipse dp c (P2 x y) hw hh 
    | hw==hh    = outputArc dp c x y hw
    | otherwise = do { ps_gsave
                     -- Not so good -- the next line changes stroke width...
                     ; ps_scale 1 (hh/hw)
                     ; outputArc dp c x y hw
                     ; ps_grestore
                     }

outputArc :: (PSColour c, PSUnit u) 
          => DrawEllipse -> c -> u -> u -> u -> WumpusM ()
outputArc EFill        c x y r = updateColour c $ do 
    ps_arc x y r 0 360 
    ps_closepath
    ps_fill

outputArc (EStroke xs) c x y r = updatePen c xs $ do 
    ps_arc x y r 0 360 
    ps_closepath
    ps_stroke


outputLabel :: PSUnit u => Label u -> WumpusM ()
outputLabel (Label (P2 x y) entxt) = do
    ps_moveto x y
    outputEncodedText entxt
--    ps_show str

outputEncodedText :: EncodedText -> WumpusM () 
outputEncodedText = mapM_ outputTextChunk . getEncodedText

outputTextChunk :: TextChunk -> WumpusM () 
outputTextChunk (SText s)  = ps_show s

outputTextChunk (EscInt i) = 
    ask >>= \env -> maybe (failk env) ps_glyphshow $ lookupByCharCode i env
  where
    failk = missingCode i . ps_fallback  

outputTextChunk (EscStr s) = ps_glyphshow s 

missingCode :: CharCode -> GlyphName -> WumpusM ()
missingCode i fallback =  do
    ps_comment $ "missing lookup for &#" ++ show i ++ ";" 
    ps_glyphshow fallback
            


