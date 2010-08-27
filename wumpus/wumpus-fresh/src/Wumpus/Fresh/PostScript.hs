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
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
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

getDrawColour   :: PsMonad RGB255
getDrawColour   = PsMonad $ \_ s -> (gs_draw_colour s,s)

setDrawColour   :: RGB255 -> PsMonad ()
setDrawColour c = PsMonad $ \_ s -> ((), s {gs_draw_colour=c})

getFontAttr :: PsMonad FontAttr
getFontAttr = PsMonad $ \_ s -> (FontAttr (gs_font_size s) (gs_font_face s),s)

getLineWidth    :: PsMonad Double
getLineWidth    = PsMonad $ \_ s -> (gs_line_width s,s)

setLineWidth    :: Double -> PsMonad ()
setLineWidth u  = PsMonad $ \_ s -> ((), s { gs_line_width=u })


getMiterLimit   :: PsMonad Double
getMiterLimit   = PsMonad $ \_ s -> (gs_miter_limit s,s)

getLineCap      :: PsMonad LineCap
getLineCap      = PsMonad $ \_ s -> (gs_line_cap s,s)

getLineJoin     :: PsMonad LineJoin
getLineJoin     = PsMonad $ \_ s -> (gs_line_join s,s)

getDashPattern  :: PsMonad DashPattern
getDashPattern  = PsMonad $ \_ s -> (gs_dash_pattern s,s)

--------------------------------------------------------------------------------



primPath :: PSUnit u
         => PathProps -> PrimPath u -> PsMonad Doc
primPath (CFill rgb)     p = 
    (\docrgb -> vcat [docrgb, makeStartPath p, ps_closepath, ps_fill]) 
      <$> deltaDrawColour rgb

primPath (CStroke _ _rgb) p = 
    (\doc -> vcat [doc, ps_closepath, ps_stroke]) <$> startPath p
 
primPath (OStroke _ _rgb) p = 
    (\doc -> vcat [doc, ps_stroke]) <$> startPath p

primPath (CFillStroke fc attrs sc) p = (\d1 d2 -> vcat [d1,d2]) 
    <$> primPath (CFill fc) p <*> primPath (CStroke attrs sc) p

{-
outputPath (CStroke xs) c p =
    updatePen c xs $ startPath p >> ps_closepath >> ps_stroke

outputPath (OStroke xs) c p =
    updatePen c xs $ startPath p >> ps_stroke
-}


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
    drawF (EStroke _attrs rgb)       pt = strokeArcPath rgb hw pt
    drawF (EFillStroke fc _attrs sc) pt = 
        vconcat <$> fillArcPath fc hw pt <*>  strokeArcPath sc hw pt
                       

-- This will need to become monadic to handle /colour delta/.
--
fillArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> PsMonad Doc
fillArcPath _rgb radius pt = pure $ 
    vcat [ ps_newpath,  ps_arc pt radius 0 360, ps_closepath, ps_fill ]

strokeArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> PsMonad Doc
strokeArcPath _rgb radius pt = pure $ 
    vcat [ ps_newpath,  ps_arc pt radius 0 360, ps_closepath, ps_stroke ]



-- Note - for the otherwise case, the x-and-y coordinates are 
-- encoded in the matrix, hence the @ 0 0 moveto @.
--
primLabel :: (Real u, Floating u, PSUnit u) => PrimLabel u -> PsMonad Doc
primLabel (PrimLabel basept txt ctm) = bracketPrimCTM basept ctm mf
  where
    mf pt = (\doc -> vcat [ ps_moveto pt, doc ]) <$> encodedText txt

encodedText :: EncodedText -> PsMonad Doc 
encodedText etext = vcat <$> (mapM textChunk $ getEncodedText etext)


textChunk :: TextChunk -> PsMonad Doc
textChunk (SText s)  = pure (ps_show $ escapeSpecial s)
textChunk (EscStr s) = pure (ps_glyphshow s)
textChunk (EscInt i) = (either failk ps_glyphshow) <$> askCharCode i 
  where
    failk gname = missingCharCode i gname

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
    df (LineWidth d)    = (\inh -> if d==inh then empty 
                                             else ps_setlinewidth d) 
                            <$> getLineWidth

    df (MiterLimit d)   = (\inh -> if d==inh then empty 
                                             else ps_setmiterlimit d)
                            <$> getMiterLimit

    df (LineCap d)      = (\inh -> if d==inh then empty 
                                             else ps_setlinecap d)
                            <$> getLineCap

    df (LineJoin d)     = (\inh -> if d==inh then empty 
                                             else ps_setlinejoin d)
                            <$> getLineJoin

    df (DashPattern d)  = (\inh -> if d==inh then empty 
                                             else ps_setdash d) 
                            <$> getDashPattern

-- wrong...
deltaFontAttrs :: FontAttr -> PsMonad Doc
deltaFontAttrs fa  = 
    (\inh -> if fa ==inh then empty else makeFontAttrs fa) <$> getFontAttr

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
