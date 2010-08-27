{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PostScript
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh PostScript.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.PostScript
  where

import Wumpus.Fresh.Colour
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.PictureInternal
import Wumpus.Fresh.PostScriptDoc
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

import Control.Applicative hiding ( empty )
-- import Data.Time


--------------------------------------------------------------------------------
-- PsMonad

-- PsMonad is at least a Reader and a State...
--
-- Graphics state works differently to SVG - PostScript has no 
-- nesting (at least not in the code Wumpus generates) so there
-- are no /savings/ by diffing from an outer environment, instead
-- the diff is with the last drawn object.
--
newtype PsMonad a = PsMonad { 
            getPsMonad :: TextEncoder -> GraphicsState -> (a,GraphicsState) }


instance Functor PsMonad where
  fmap f mf = PsMonad $ \r s -> let (a,s1) = getPsMonad mf r s in (f a,s1)

instance Applicative PsMonad where
  pure a    = PsMonad $ \_ s -> (a,s)
  mf <*> ma = PsMonad $ \r s -> let (f,s1) = getPsMonad mf r s
                                    (a,s2) = getPsMonad ma r s1
                                  in (f a,s2)

instance Monad PsMonad where
  return a  = PsMonad $ \_ s  -> (a,s)
  m >>= k   = PsMonad $ \r s -> let (a,s1) = getPsMonad m r s
                                in (getPsMonad . k) a r s1
                              

runPsMonad :: TextEncoder -> PsMonad a -> a
runPsMonad enc mf = fst $ getPsMonad mf enc zeroGS

askCharCode :: Int -> PsMonad (Either GlyphName GlyphName)
askCharCode i = PsMonad $ \r s -> case lookupByCharCode i r of
    Just n  -> (Right n,s)
    Nothing -> (Left $ ps_fallback r,s)

getDrawColour       :: PsMonad RGB255
getDrawColour       = PsMonad $ \_ s -> (gs_draw_colour s, s)

setDrawColour       :: RGB255 -> PsMonad ()
setDrawColour a     = PsMonad $ \_ s -> ((), s {gs_draw_colour=a})


getFontAttr         :: PsMonad FontAttr
getFontAttr         = PsMonad $ \_ s -> let sz = gs_font_size s 
                                            ff = gs_font_face s
                                        in (FontAttr sz ff, s)

setFontAttr         :: FontAttr -> PsMonad ()
setFontAttr (FontAttr sz ff) = 
    PsMonad $ \_ s -> ((), s { gs_font_size=sz,gs_font_face=ff })

  
getLineWidth        :: PsMonad Double
getLineWidth        = PsMonad $ \_ s -> (gs_line_width s, s)

setLineWidth        :: Double -> PsMonad ()
setLineWidth a      = PsMonad $ \_ s -> ((), s { gs_line_width=a })


getMiterLimit       :: PsMonad Double
getMiterLimit       = PsMonad $ \_ s -> (gs_miter_limit s,s)

setMiterLimit       :: Double -> PsMonad ()
setMiterLimit a     = PsMonad $ \_ s -> ((), s { gs_miter_limit=a })


getLineCap          :: PsMonad LineCap
getLineCap          = PsMonad $ \_ s -> (gs_line_cap s,s)

setLineCap          :: LineCap -> PsMonad ()
setLineCap a        = PsMonad $ \_ s -> ((), s { gs_line_cap=a })


getLineJoin         :: PsMonad LineJoin
getLineJoin         = PsMonad $ \_ s -> (gs_line_join s,s)

setLineJoin         :: LineJoin -> PsMonad ()
setLineJoin a       = PsMonad $ \_ s -> ((), s { gs_line_join=a })


getDashPattern      :: PsMonad DashPattern
getDashPattern      = PsMonad $ \_ s -> (gs_dash_pattern s,s)

setDashPattern      :: DashPattern -> PsMonad ()
setDashPattern a    = PsMonad $ \_ s -> ((), s { gs_dash_pattern=a })

--------------------------------------------------------------------------------



primPath :: PSUnit u
         => PathProps -> PrimPath u -> PsMonad Doc
primPath (CFill rgb)     p = 
    (\rgbd -> vcat [rgbd, makeStartPath p, ps_closepath, ps_fill]) 
      <$> deltaDrawColour rgb  

primPath (CStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [rgbd, attrd, makeStartPath p
                         , ps_closepath, ps_stroke])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs
 
primPath (OStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [rgbd, attrd, makeStartPath p, ps_stroke]) 
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs

primPath (CFillStroke fc attrs sc) p = 
    (\d1 d2 -> vcat [d1,d2])
      <$> primPath (CFill fc) p <*> primPath (CStroke attrs sc) p


startPath :: PSUnit u => PrimPath u -> PsMonad Doc
startPath = pure . makeStartPath

makeStartPath :: PSUnit u => PrimPath u -> Doc
makeStartPath (PrimPath start xs) = 
    vcat $ ps_newpath : ps_moveto start : map makePathSegment xs



makePathSegment :: PSUnit u => PrimPathSegment u -> Doc
makePathSegment (PLineTo p1)        = ps_lineto p1 
makePathSegment (PCurveTo p1 p2 p3) = ps_curveto p1 p2 p3 


-- | Drawing stroked ellipse has an unfortunate - but (probably) 
-- unavoidable deficiency.
--
-- The use of PostScript\'s @concat@ operator to alter the arc 
-- hw/hh will vary the line width during the drawing of a stroked 
-- ellipse.
--
-- For good stroked ellipses, Bezier curves constructed from 
-- PrimPaths should be used.
--
primEllipse :: (Real u, Floating u, PSUnit u) 
            => EllipseProps -> PrimEllipse u -> PsMonad Doc
primEllipse props (PrimEllipse center hw hh ctm) =
    bracketPrimCTM center (scaleCTM 1 (hh/hw) ctm) (drawF props)
  where
    drawF (EFill rgb)               pt = fillArcPath rgb hw pt
    drawF (EStroke attrs rgb)       pt = strokeArcPath rgb attrs hw pt
    drawF (EFillStroke fc attrs sc) pt = 
        vconcat <$> fillArcPath fc hw pt <*>  strokeArcPath sc attrs hw pt
                       


-- This will need to become monadic to handle /colour delta/.
--
fillArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> PsMonad Doc
fillArcPath rgb radius pt = 
    (\rgbd -> vcat [ rgbd
                   , ps_newpath
                   , ps_arc pt radius 0 360
                   , ps_closepath
                   , ps_fill ])
      <$> deltaDrawColour rgb

strokeArcPath :: PSUnit u 
              => RGB255 -> [StrokeAttr] -> u -> Point2 u -> PsMonad Doc
strokeArcPath rgb attrs radius pt =
    (\rgbd attrd -> vcat [ rgbd
                         , attrd
                         , ps_newpath
                         , ps_arc pt radius 0 360
                         , ps_closepath
                         , ps_stroke ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs


-- Note - for the otherwise case, the x-and-y coordinates are 
-- encoded in the matrix, hence the @ 0 0 moveto @.
--
primLabel :: (Real u, Floating u, PSUnit u) 
          => LabelProps -> PrimLabel u -> PsMonad Doc
primLabel (LabelProps rgb font) (PrimLabel basept txt ctm) = 
    bracketPrimCTM basept ctm mf
  where
    mf pt = (\rgbd fontd showd -> vcat [ rgbd, fontd, ps_moveto pt, showd ]) 
              <$> deltaDrawColour rgb <*> deltaFontAttrs font 
                                      <*> encodedText txt

encodedText :: EncodedText -> PsMonad Doc 
encodedText etext = vcat <$> (mapM textChunk $ getEncodedText etext)


textChunk :: TextChunk -> PsMonad Doc
textChunk (SText s)  = pure (ps_show $ escapeSpecial s)
textChunk (EscStr s) = pure (ps_glyphshow s)
textChunk (EscInt i) = (either failk ps_glyphshow) <$> askCharCode i 
  where
    failk gly_name = missingCharCode i gly_name

--------------------------------------------------------------------------------
-- Stroke, font and drawing colour attribute delta

-- This needs more thought than SVG as there is no natural 
-- nesting to benefit from...

-- All graphics are annotated with colour - the graphics state
-- doesn\'t tell us the actual colour of anything, only if we 
-- need to write a colour change to the output.
-- 
-- So we compare the current colour with the state - if it is
-- different we put the new colour in the state and output a 
-- @setrgbcolor@ message.
--

deltaDrawColour :: RGB255 -> PsMonad Doc
deltaDrawColour rgb = getDrawColour >>= \inh -> 
   if rgb==inh then return empty
               else setDrawColour rgb >> return (ps_setrgbcolor rgb)



-- Wrong - should follow delta draw colour...
deltaStrokeAttrs :: [StrokeAttr] -> PsMonad Doc
deltaStrokeAttrs xs = hcat <$> mapM df xs
  where
    df (LineWidth d)    = getLineWidth >>= \inh -> 
                          if d==inh then return empty 
                                    else setLineWidth d >> 
                                         return (ps_setlinewidth d)

    df (MiterLimit d)   = getMiterLimit >>= \inh -> 
                          if d==inh then return empty 
                                    else setMiterLimit d >> 
                                         return (ps_setmiterlimit d)
                            
    df (LineCap d)      = getLineCap >>= \inh -> 
                          if d==inh then return empty 
                                    else setLineCap d >> 
                                         return (ps_setlinecap d)
                      
    df (LineJoin d)     = getLineJoin >>= \inh -> 
                          if d==inh then return empty 
                                    else setLineJoin d >> 
                                         return (ps_setlinejoin d)

    df (DashPattern d)  = getDashPattern >>= \inh -> 
                          if d==inh then return empty 
                                    else setDashPattern d >> 
                                         return (ps_setdash d)
           


deltaFontAttrs :: FontAttr -> PsMonad Doc
deltaFontAttrs fa  = getFontAttr >>= \inh ->
    if fa==inh then return empty 
               else setFontAttr fa >> return (makeFontAttrs fa)

makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = 
    vcat [ ps_findfont (font_name face), ps_scalefont sz, ps_setfont ]


--------------------------------------------------------------------------------
-- PrimCTM

bracketPrimCTM :: forall u. (Real u, Floating u, PSUnit u)
               => Point2 u -> PrimCTM u 
               -> (Point2 u -> PsMonad Doc) -> PsMonad Doc
bracketPrimCTM pt@(P2 x y) ctm mf 
    | ctm == identityCTM  = mf pt
    | otherwise           = (\doc -> vcat [inn, doc, out]) <$> mf zeroPt'
  where
    zeroPt' :: Point2 u
    zeroPt' = zeroPt

    mtrx  = translMatrixRepCTM x y ctm
    inn   = ps_concat $ mtrx
    out   = ps_concat $ invert mtrx
