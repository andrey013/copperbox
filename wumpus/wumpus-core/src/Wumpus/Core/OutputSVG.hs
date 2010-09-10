{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.OutputSVG
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output SVG. 
--
-- This is complicated by two differences with PostScript.
--
-- 1. The coordinate space of SVG is /origin top-left/, for 
-- PostScript it is /origin bottom-left/.
-- 
-- 2. Clipping in PostScript works by changing the graphics state
-- Clip a path, then all subsequent drawing be rendered only 
-- when it is within the clip bounds. Clearly using clipping 
-- paths within a @gsave ... grestore@ block is a good idea...
--
-- SVG uses /tagging/. A clipPath element is declared and named 
-- then referenced in subsequent elements via the clip-path 
-- attribute - @clip-path=\"url(#clip_path_tag)\"@.
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.OutputSVG 
  (

  -- * Output SVG
    writeSVG

  , writeSVG_latin1

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.FormatCombinators
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.PageTranslation
import Wumpus.Core.PictureInternal
import Wumpus.Core.SVGDoc
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextInternal
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils

import Control.Applicative hiding ( empty, some )
import qualified Data.Foldable as F
import Data.Maybe

-- SvgMonad is two Readers plus Int state for clip paths...
--
newtype SvgMonad a = SvgMonad { 
            getSvgMonad :: TextEncoder -> GraphicsState -> Int -> (a,Int) }



instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r1 r2 s -> let (a,s1) = getSvgMonad mf r1 r2 s
                                     in (f a,s1)

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_  _  s -> (a,s)
  mf <*> ma = SvgMonad $ \r1 r2 s -> let (f,s1) = getSvgMonad mf r1 r2 s
                                         (a,s2) = getSvgMonad ma r1 r2 s1
                                   in (f a, s2)

instance Monad SvgMonad where
  return a  = SvgMonad $ \_  _  s -> (a,s)
  m >>= k   = SvgMonad $ \r1 r2 s -> let (a,s1) = getSvgMonad m r1 r2 s
                                     in (getSvgMonad . k) a r1 r2 s1
                            


runSvgMonad :: TextEncoder -> SvgMonad a -> a
runSvgMonad enc mf = fst $ getSvgMonad mf enc zeroGS 0

newClipLabel :: SvgMonad String
newClipLabel = SvgMonad $ \_ _ s -> ('c':'l':'i':'p':show s, s+1)

askGlyphName :: String -> SvgMonad (Either GlyphName GlyphName)
askGlyphName nm = SvgMonad $ \r1 _ s -> case lookupByGlyphName nm r1 of
    Just a  -> (Right $ escapeSpecial a, s)
    Nothing -> (Left  $ escapeSpecial $ svg_fallback r1, s)

-- This is different to the PsMonad version, as SVG is nested 
-- (and /graphics state/ is via a Reader), so it is the same as 
-- local with a Reader monad.
--
runLocalGS :: (GraphicsState -> GraphicsState) -> SvgMonad a -> SvgMonad a
runLocalGS upd mf = 
    SvgMonad $ \r1 r2 s -> getSvgMonad mf r1 (upd r2) s


askGraphicsState :: SvgMonad GraphicsState
askGraphicsState = SvgMonad $ \_ r2 s -> (r2,s)

asksGraphicsState :: (GraphicsState -> a) -> SvgMonad a
asksGraphicsState fn = SvgMonad $ \_ r2 s -> (fn r2,s)

askFontAttr :: SvgMonad FontAttr
askFontAttr = 
    asksGraphicsState $ \r -> FontAttr (gs_font_size r) (gs_font_face r)

askLineWidth    :: SvgMonad Double
askLineWidth    = asksGraphicsState (line_width . gs_stroke_attr)

askMiterLimit   :: SvgMonad Double
askMiterLimit   = asksGraphicsState (miter_limit . gs_stroke_attr)

askLineCap      :: SvgMonad LineCap
askLineCap      = asksGraphicsState (line_cap . gs_stroke_attr)

askLineJoin     :: SvgMonad LineJoin
askLineJoin     = asksGraphicsState (line_join . gs_stroke_attr)

askDashPattern  :: SvgMonad DashPattern
askDashPattern  = asksGraphicsState (dash_pattern . gs_stroke_attr)

--------------------------------------------------------------------------------

-- | Output a picture to a SVG file. 
--
writeSVG :: (Real u, Floating u, PSUnit u) 
         => FilePath -> TextEncoder -> Picture u -> IO ()
writeSVG filepath enc pic = 
    writeFile filepath $ show $ svgDraw enc pic 

-- | Version of 'writeSVG' - using Latin1 encoding. 
--
writeSVG_latin1 :: (Real u, Floating u, PSUnit u) 
                => FilePath -> Picture u -> IO ()
writeSVG_latin1 filepath = writeSVG filepath latin1Encoder

svgDraw :: (Real u, Floating u, PSUnit u) 
        => TextEncoder -> Picture u -> Doc
svgDraw enc original_pic = 
    let pic          = trivialTranslation original_pic
        (_,imgTrafo) = imageTranslation pic
        body         = runSvgMonad enc $ picture pic
    in vcat [ xml_version, doctype, elem_svg $ imgTrafo body ]



imageTranslation :: (Ord u, PSUnit u) 
                 => Picture u -> (BoundingBox u, Doc -> Doc)
imageTranslation pic = case repositionDeltas pic of
  (bb, Nothing) -> (bb, id)
  (bb, Just v)  -> let attr = attr_transform (val_translate v) 
                   in (bb, elem_g attr)

--------------------------------------------------------------------------------

-- Note - it will be wise to make coordinate remapping and output
-- separate passes (unlike in Wumpus-Core). Then I\'ll at least 
-- be able to debug the remapped Picture.
--



picture :: (Real u, Floating u, PSUnit u) => Picture u -> SvgMonad Doc
picture (Leaf    (_,xs) ones)   = bracketTrafos xs $ revConcat primElement ones
picture (Picture (_,xs) ones)   = bracketTrafos xs $ revConcat picture ones
picture (Clip    (_,xs) cp pic) = 
    bracketTrafos xs $ do { lbl <- newClipLabel
                          ; d1  <- clipPath lbl cp
                          ; d2  <- picture pic
                          ; return (vconcat d1 (elem_g (attr_clip_path lbl) d2))
                          } 
picture (Group   (_,xs) fn pic) = bracketTrafos xs $ bracketGS fn (picture pic)



-- This starts with an empty line...
--

revConcat :: (a -> SvgMonad Doc) -> OneList a -> SvgMonad Doc
revConcat fn ones = some empty <$> F.foldrM step None ones
  where
    step e ac = (\d -> d `conc` ac) <$> fn e
    conc d None      = Some d
    conc d (Some ac) = Some $ ac `vconcat` d


primElement :: (Real u, Floating u, PSUnit u) => PrimElement u -> SvgMonad Doc
primElement (Atom prim)          = primitive prim
primElement (XLinkGroup xl ones) = drawXLink xl <$> revConcat primElement ones

primitive :: (Real u, Floating u, PSUnit u) => Primitive u -> SvgMonad Doc
primitive (PPath props pp)     = primPath props pp
primitive (PLabel props lbl)   = primLabel props lbl
primitive (PEllipse props ell) = primEllipse props ell
 

drawXLink :: XLink -> Doc -> Doc
drawXLink (XLink href) doc = elem_a_xlink href doc

clipPath :: PSUnit u => String -> PrimPath u -> SvgMonad Doc
clipPath clip_id pp = (\doc -> elem_clipPath (attr_id clip_id) doc) <$> path pp


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
primLabel (LabelProps rgb attrs) (PrimLabel pt body ctm) = 
    (\fa ca dtxt -> elem_text (fa <+> ca) (makeTspan rgb dtxt))
      <$> deltaFontAttrs attrs <*> bracketPrimCTM pt ctm (labelBodyCoords body)
                               <*> labelBodyText body
    


labelBodyCoords :: PSUnit u => LabelBody u -> Point2 u -> SvgMonad Doc
labelBodyCoords (StdLayout _)  pt = pure $ makeXY pt
labelBodyCoords (KernTextH xs) pt = pure $ makeXsY pt xs        
labelBodyCoords (KernTextV xs) pt = pure $ makeXYs pt xs

labelBodyText :: LabelBody u -> SvgMonad Doc
labelBodyText (StdLayout enctext) = encodedText enctext
labelBodyText (KernTextH xs)      = hcat <$> mapM kerningChar xs
labelBodyText (KernTextV xs)      = hcat <$> mapM kerningChar xs


encodedText :: EncodedText -> SvgMonad Doc
encodedText enctext = hcat <$> mapM textChunk (getEncodedText enctext)

textChunk :: TextChunk -> SvgMonad Doc
textChunk (TextSpan s)    = pure $ text s
textChunk (TextEscInt i)  = pure $ text $ escapeSpecial i
textChunk (TextEscName s) = either text text <$> askGlyphName s 

kerningChar :: KerningChar u -> SvgMonad Doc
kerningChar (_, CharLiteral c) = pure $ char c
kerningChar (_, CharEscInt i)  = pure $ text $ escapeSpecial i
kerningChar (_, CharEscName s) = either text text <$> askGlyphName s 


makeTspan :: RGBi -> Doc -> Doc
makeTspan rgb body = elem_tspan (attr_fill rgb) body

makeXY :: PSUnit u => Point2 u -> Doc
makeXY (P2 x y) = attr_x x <+> attr_y y

-- This is for horizontal kerning text, the output is of the 
-- form:
-- 
-- > x="0 10 25 35" y="0"
--
makeXsY :: PSUnit u => Point2 u -> [KerningChar u] -> Doc
makeXsY (P2 x y) ks = attr_xs (step x ks) <+> attr_y y
  where 
    step ax ((d,_):ds) = let a = ax+d in a : step a ds 
    step _  []         = []


-- This is for vertical kerning text, the output is of the 
-- form:
-- 
-- > x="0 0 0 0" y="0 10 25 35"
--
-- Note - this is different to the horizontal version as the 
-- x-coord needs to be /realigned/ at each step.
--
makeXYs :: PSUnit u => Point2 u -> [KerningChar u] -> Doc
makeXYs (P2 x y) ks = attr_xs xcoords <+> attr_ys (step y ks)
  where 
    xcoords            = replicate (length ks) x
    step ay ((d,_):ds) = let a = ay+d in a : step a ds 
    step _  []         = []
    
    

--------------------------------------------------------------------------------
-- Stroke and font attribute delta

deltaStrokeAttrs :: StrokeAttr -> SvgMonad Doc
deltaStrokeAttrs sa = 
    (\d1 d2 d3 d4 d5 -> hsep $ catMaybes [d1,d2,d3,d4,d5])  
      <$> lw <*> ml <*> lc <*> lj <*> dp
  where
    lw = let d = line_width sa in
         askLineWidth >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_width d) 
         
    ml = let d = miter_limit sa in
         askMiterLimit >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_miterlimit d)

    lc = let d = line_cap sa in
         askLineCap >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_linecap d)

    lj = let d = line_join sa in
         askLineJoin >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ attr_stroke_linejoin d)

    dp = let d = dash_pattern sa in 
         askDashPattern >>= \inh -> 
         if d==inh then return Nothing 
                   else return (Just $ makeDashPattern d) 

makeDashPattern :: DashPattern -> Doc
makeDashPattern Solid       = attr_stroke_dasharray_none
makeDashPattern (Dash n xs) = 
    attr_stroke_dashoffset n <+> attr_stroke_dasharray xs



deltaFontAttrs :: FontAttr -> SvgMonad Doc
deltaFontAttrs fa = 
    (\inh -> if fa ==inh then empty else makeFontAttrs fa) <$> askFontAttr

makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = 
    attr_font_family (svg_font_family face) <+> attr_font_size sz 
                                            <> suffix (svg_font_style face) 
  where  
    suffix SVG_REGULAR      = empty

    suffix SVG_BOLD         = space <> attr_font_weight "bold"

    suffix SVG_ITALIC       = space <> attr_font_style "italic"

    suffix SVG_BOLD_ITALIC  = 
        space <> attr_font_weight "bold" <+> attr_font_style "italic"

    suffix SVG_OBLIQUE      = space <> attr_font_style "oblique"

    suffix SVG_BOLD_OBLIQUE = 
        space <> attr_font_weight "bold" <+> attr_font_style "oblique"


-- NOTE - as is only practical to delta the FontFace attributes 
-- it might be good to specialize / simplify the graphics state
-- GSUpdate to a simpler type rather than a functional one...

bracketGS :: FontCtx -> SvgMonad Doc -> SvgMonad Doc
bracketGS (FontCtx new_font) mf = 
    (\old body -> mkElem old body) 
        <$> askGraphicsState <*> runLocalGS updateF mf
  where
    mkElem old body 
      | fontMatch old new_font = elem_g_no_attrs body
      | otherwise              = let a = makeFontAttrs new_font
                                 in elem_g a body

    updateF s = s { gs_font_size = font_size new_font
                  , gs_font_face = font_face new_font }
                

fontMatch :: GraphicsState -> FontAttr -> Bool
fontMatch gs fa = 
   gs_font_size gs == font_size fa && gs_font_face gs == font_face fa


--------------------------------------------------------------------------------
-- Bracket matrix and PrimCTM trafos

bracketTrafos :: (Real u, Floating u, PSUnit u) 
              => [AffineTrafo u] -> SvgMonad Doc -> SvgMonad Doc
bracketTrafos xs ma = bracketMatrix (concatTrafos xs) ma 

bracketMatrix :: (Fractional u, PSUnit u) 
              => Matrix3'3 u -> SvgMonad Doc -> SvgMonad Doc
bracketMatrix mtrx ma 
    | mtrx == identityMatrix = (\doc -> elem_g_no_attrs doc) <$>  ma
    | otherwise              = (\doc -> elem_g trafo doc) <$> ma
  where
    trafo = attr_transform $ val_matrix mtrx


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
