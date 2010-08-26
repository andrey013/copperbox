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

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.SVGDoc
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

import Control.Applicative hiding ( empty )

type DocS = Doc -> Doc

-- SvgMonad is at least a Reader monad...
--
newtype SvgMonad a = SvgMonad { 
            getSvgMonad :: TextEncoder -> a }

instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r -> let a = getSvgMonad mf r
                               in f a

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_ -> a
  mf <*> ma = SvgMonad $ \r -> let f = getSvgMonad mf r
                                   a = getSvgMonad ma r
                               in f a

instance Monad SvgMonad where
  return a  = SvgMonad $ \_ -> a
  m >>= k   = SvgMonad $ \r -> let a = getSvgMonad m r
                                   b = (getSvgMonad . k) a r
                                in b


runSvgMonad :: TextEncoder -> SvgMonad a -> a
runSvgMonad enc mf = getSvgMonad mf enc

askGlyphName :: String -> SvgMonad (Either GlyphName GlyphName)
askGlyphName nm = SvgMonad $ \r -> case lookupByGlyphName nm r of
    Just a  -> Right $ escapeSpecial a
    Nothing -> Left  $ escapeSpecial $ svg_fallback r



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
pathProps :: PathProps -> SvgMonad (Doc, Doc -> Doc)
pathProps props = case props of
    CFill rgb                   -> pure (fillNotStroke rgb, suffixClose) 
    CStroke attrs rgb           -> pure (strokeNotFill rgb, suffixClose)
    OStroke attrs rgb           -> pure (strokeNotFill rgb, id)
    CFillStroke frgb attrs srgb -> pure (attr_fill frgb <+> attr_stroke srgb
                                        , suffixClose) 
  where
    fillNotStroke rgb = attr_fill rgb <+> attr_stroke_none 
    strokeNotFill rgb = attr_stroke rgb <+> attr_fill_none

    suffixClose doc = doc <+> char 'Z'
 




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
ellipseProps (EFill rgb)        = pure $ attr_fill rgb <+> attr_stroke_none

ellipseProps (EStroke attrs rgb) = pure $ attr_stroke rgb <+> attr_fill_none 

ellipseProps (EFillStroke frgb attrs srgb) = 
    pure $ attr_fill frgb <+> attr_stroke srgb



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
      <$> fontAttrs attrs <*> bracketPrimCTM pt ctm mkXY <*> tspan rgb etext
  where
    mkXY (P2 x y) = pure $ attr_x x <+> attr_y y    


fontAttrs :: FontAttr -> SvgMonad Doc
fontAttrs (FontAttr sz face) = 
   (\szd df -> df szd) <$> fontSize sz <*> fontStyle (svg_font_style face)

fontSize :: Int -> SvgMonad Doc
fontSize sz = pure (attr_font_size sz) 

fontStyle :: SVGFontStyle -> SvgMonad DocS
fontStyle SVG_REGULAR      = pure id

fontStyle SVG_BOLD         = pure (<+> attr_font_weight "bold")

fontStyle SVG_ITALIC       = pure (<+> attr_font_style "italic")

fontStyle SVG_BOLD_ITALIC  = 
    pure (<+> attr_font_weight "bold" <+> attr_font_style "italic")

fontStyle SVG_OBLIQUE      = pure (<+> attr_font_style "oblique")

fontStyle SVG_BOLD_OBLIQUE = 
    pure (<+> attr_font_weight "bold" <+> attr_font_style "oblique")


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
