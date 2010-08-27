{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.SVG
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh SVG.
--
--------------------------------------------------------------------------------

module Wumpus.Fresh.SVG 
  where

import Wumpus.Fresh.Colour
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.SVGDoc
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

import Control.Applicative hiding ( empty )


-- SvgMonad is at least a two Readers...
--
newtype SvgMonad a = SvgMonad { 
            getSvgMonad :: TextEncoder -> GraphicsState -> a }

instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r1 r2 -> let a = getSvgMonad mf r1 r2 in f a

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_  _  -> a
  mf <*> ma = SvgMonad $ \r1 r2 -> let f = getSvgMonad mf r1 r2
                                       a = getSvgMonad ma r1 r2
                                   in f a

instance Monad SvgMonad where
  return a  = SvgMonad $ \_  _  -> a
  m >>= k   = SvgMonad $ \r1 r2 -> let a = getSvgMonad m r1 r2
                                       b = (getSvgMonad . k) a r1 r2
                                   in b


runSvgMonad :: TextEncoder -> SvgMonad a -> a
runSvgMonad enc mf = getSvgMonad mf enc zeroGS

askGlyphName :: String -> SvgMonad (Either GlyphName GlyphName)
askGlyphName nm = SvgMonad $ \r1 _ -> case lookupByGlyphName nm r1 of
    Just a  -> Right $ escapeSpecial a
    Nothing -> Left  $ escapeSpecial $ svg_fallback r1

askFontAttr :: SvgMonad FontAttr
askFontAttr = SvgMonad $ \_ r2 -> FontAttr (gs_font_size r2) (gs_font_face r2)

askLineWidth    :: SvgMonad Double
askLineWidth    = SvgMonad $ \_ r2 -> gs_line_width r2

askMiterLimit   :: SvgMonad Double
askMiterLimit   = SvgMonad $ \_ r2 -> gs_miter_limit r2

askLineCap      :: SvgMonad LineCap
askLineCap      = SvgMonad $ \_ r2 -> gs_line_cap r2

askLineJoin     :: SvgMonad LineJoin
askLineJoin     = SvgMonad $ \_ r2 -> gs_line_join r2

askDashPattern  :: SvgMonad DashPattern
askDashPattern  = SvgMonad $ \_ r2 -> gs_dash_pattern r2

-- Note - it will be wise to make coordinate remapping and output
-- separate passes (unlike in Wumpus-Core). Then I\'ll at least 
-- be able to debug the remapped Picture.
--


primPath :: PSUnit u => PathProps -> PrimPath u -> SvgMonad Doc
primPath props pp = (\(a,f) d -> elem_path a (f d)) 
                      <$> pathProps props <*> path pp


path :: PSUnit u => PrimPath u -> SvgMonad Doc
path (PrimPath start xs) = 
    pure $ path_m start <+> hsep (map seg xs)
  where
    seg (PLineTo pt)        = path_l pt
    seg (PCurveTo p1 p2 p3) = path_c p1 p2 p3

-- Return - drawing props, plus a function to close the path (or not). 
--
pathProps :: PathProps -> SvgMonad (Doc, Doc -> Doc)
pathProps props = fn props
  where
    fn (CFill rgb)                = pure (fillNotStroke rgb, close) 

    fn (CStroke attrs rgb)        = 
        (\a -> (strokeNotFill rgb <+> a, close))   <$> deltaStrokeAttrs attrs

    fn (OStroke attrs rgb)        = 
        (\a -> (strokeNotFill rgb <+> a, id))      <$> deltaStrokeAttrs attrs

    fn (CFillStroke fc attrs sc)  =
        (\a -> (fillAndStroke fc sc <+> a, close)) <$> deltaStrokeAttrs attrs

    fillNotStroke rgb             = attr_fill rgb   <+> attr_stroke_none 
    strokeNotFill rgb             = attr_stroke rgb <+> attr_fill_none
    fillAndStroke a b             = attr_fill a     <+> attr_stroke b
    close                         = (<+> char 'Z')
 




-- Note - if hw==hh then draw the ellipse as a circle.
--
primEllipse :: (Real u, Floating u, PSUnit u)
            => EllipseProps -> PrimEllipse u -> SvgMonad Doc
primEllipse props (PrimEllipse pt hw hh ctm) 
    | hw == hh  = (\a b -> elem_circle (a <+> circle_radius <+> b))
                    <$> bracketPrimCTM pt ctm mkCXCY <*> ellipseProps props
    | otherwise = (\a b -> elem_ellipse (a <+> ellipse_radius <+> b))
                    <$> bracketPrimCTM pt ctm mkCXCY <*> ellipseProps props
  where
   mkCXCY (P2 x y) = pure $ attr_cx x <+> attr_cy y
   
   circle_radius   = attr_r hw
   ellipse_radius  = attr_rx hw <+> attr_ry hh

 

ellipseProps :: EllipseProps -> SvgMonad Doc
ellipseProps (EFill rgb)                   = 
    pure (attr_fill rgb <+> attr_stroke_none)

ellipseProps (EStroke attrs rgb)           = 
    (\a -> attr_stroke rgb <+> attr_fill_none <+> a)  <$> deltaStrokeAttrs attrs

ellipseProps (EFillStroke frgb attrs srgb) = 
    (\a -> attr_fill frgb <+> attr_stroke srgb <+> a) <$> deltaStrokeAttrs attrs



-- Note - Rendering coloured text seemed convoluted 
-- (mandating the tspan element). 
--
-- TO CHECK - is this really the case?
-- 
--

primLabel :: (Real u, Floating u, PSUnit u) 
      => LabelProps -> PrimLabel u -> SvgMonad Doc
primLabel (LabelProps rgb attrs) (PrimLabel pt etext ctm) = 
    (\fa ca txt -> elem_text (fa <+> ca) txt)
      <$> deltaFontAttrs attrs <*> bracketPrimCTM pt ctm mkXY 
                               <*> tspan rgb etext
  where
    mkXY (P2 x y) = pure $ attr_x x <+> attr_y y    

tspan :: RGB255 -> EncodedText -> SvgMonad Doc
tspan rgb enctext = 
    (\txt -> elem_tspan (attr_fill rgb) txt) 
      <$> encodedText enctext

encodedText :: EncodedText -> SvgMonad Doc
encodedText enctext = hcat <$> mapM textChunk (getEncodedText enctext)


textChunk :: TextChunk -> SvgMonad Doc
textChunk (SText s)  = pure $ text s
textChunk (EscInt i) = pure $ text $ escapeSpecial i
textChunk (EscStr s) = either text text <$> askGlyphName s 


--------------------------------------------------------------------------------
-- Stroke and font attribute delta

deltaStrokeAttrs :: [StrokeAttr] -> SvgMonad Doc
deltaStrokeAttrs xs = hcat <$> mapM df xs
  where
    df (LineWidth d)    = (\inh -> if d==inh then empty 
                                         else attr_stroke_width d) 
                            <$> askLineWidth

    df (MiterLimit d)   = (\inh -> if d==inh then empty 
                                         else attr_stroke_miterlimit d)
                            <$> askMiterLimit

    df (LineCap d)      = (\inh -> if d==inh then empty 
                                         else attr_stroke_linecap d)
                            <$> askLineCap

    df (LineJoin d)     = (\inh -> if d==inh then empty 
                                         else attr_stroke_linejoin d)
                            <$> askLineJoin

    df (DashPattern d)  = (\inh -> if d==inh then empty 
                                             else makeDashPattern d) 
                            <$> askDashPattern

makeDashPattern :: DashPattern -> Doc
makeDashPattern Solid       = attr_stroke_dasharray_none
makeDashPattern (Dash n xs) = 
    attr_stroke_dashoffset n <+> attr_stroke_dasharray xs



deltaFontAttrs :: FontAttr -> SvgMonad Doc
deltaFontAttrs fa  = 
    (\inh -> if fa ==inh then empty else makeFontAttrs fa) <$> askFontAttr

makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = 
    sf (svg_font_style face) $ attr_font_size sz
  where  
    sf SVG_REGULAR      = id
    sf SVG_BOLD         = (<+> attr_font_weight "bold")
    sf SVG_ITALIC       = (<+> attr_font_style "italic")
    sf SVG_BOLD_ITALIC  = (<+> attr_font_weight "bold" 
                           <+> attr_font_style "italic")
    sf SVG_OBLIQUE      = (<+> attr_font_style "oblique")
    sf SVG_BOLD_OBLIQUE = (<+> attr_font_weight "bold" 
                           <+> attr_font_style "oblique")



--------------------------------------------------------------------------------
-- PrimCTM


bracketPrimCTM :: forall u. (Real u, Floating u, PSUnit u)
               => Point2 u -> PrimCTM u 
               -> (Point2 u -> SvgMonad Doc) -> SvgMonad Doc
bracketPrimCTM pt@(P2 x y) ctm pf 
    | ctm == identityCTM  = pf pt
    | otherwise           = (\xy -> xy <+> attr_transform mtrx) <$> pf zeroPt'
  where
    zeroPt' :: Point2 u
    zeroPt' = zeroPt

    mtrx  = val_matrix $ translMatrixRepCTM x y ctm
