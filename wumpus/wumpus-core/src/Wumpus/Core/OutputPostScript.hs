{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PostScript
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Output PostScript - either PostScript (PS) files or 
-- EPS (Encapusulated PostScript) files can be generated. 
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
import Wumpus.Core.FormatCombinators
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.PictureInternal
import Wumpus.Core.PostScriptDoc
import Wumpus.Core.TextEncoder
import Wumpus.Core.TextInternal
import Wumpus.Core.TextLatin1
import Wumpus.Core.Utils

import Control.Applicative hiding ( empty, some )
import Control.Monad

import qualified Data.Foldable          as F
import Data.Maybe 
import Data.Time


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

-- runLocalGS :: GSUpdate -> PsMonad a -> PsMonad a
-- runLocalGS upd mf = 
--     PsMonad $ \r s -> let (a,_) = getPsMonad mf r (getGSU upd s) in (a,s)

getDrawColour       :: PsMonad RGBi
getDrawColour       = PsMonad $ \_ s -> (gs_draw_colour s, s)

setDrawColour       :: RGBi -> PsMonad ()
setDrawColour a     = PsMonad $ \_ s -> ((), s {gs_draw_colour=a})


getFontAttr         :: PsMonad FontAttr
getFontAttr         = PsMonad $ \_ s -> let sz = gs_font_size s 
                                            ff = gs_font_face s
                                        in (FontAttr sz ff, s)

setFontAttr         :: FontAttr -> PsMonad ()
setFontAttr (FontAttr sz ff) = 
    PsMonad $ \_ s -> ((), s { gs_font_size=sz,gs_font_face=ff })

  
getLineWidth        :: PsMonad Double
getLineWidth        = PsMonad $ \_ s -> (line_width $ gs_stroke_attr s, s)

setLineWidth        :: Double -> PsMonad ()
setLineWidth a      = PsMonad $ \_ s -> 
    ((), s { gs_stroke_attr = fn $ gs_stroke_attr s })
  where
    fn sa = sa { line_width = a }

getMiterLimit       :: PsMonad Double
getMiterLimit       = PsMonad $ \_ s -> (miter_limit $ gs_stroke_attr s,s)

setMiterLimit       :: Double -> PsMonad ()
setMiterLimit a     = PsMonad $ \_ s -> 
    ((), s { gs_stroke_attr = fn $ gs_stroke_attr s })
  where
    fn sa = sa { miter_limit = a }


getLineCap          :: PsMonad LineCap
getLineCap          = PsMonad $ \_ s -> (line_cap $ gs_stroke_attr s,s)

setLineCap          :: LineCap -> PsMonad ()
setLineCap a        = PsMonad $ \_ s ->
    ((), s { gs_stroke_attr = fn $ gs_stroke_attr s })
  where
    fn sa = sa { line_cap = a }


getLineJoin         :: PsMonad LineJoin
getLineJoin         = PsMonad $ \_ s -> (line_join $ gs_stroke_attr s,s)

setLineJoin         :: LineJoin -> PsMonad ()
setLineJoin a       = PsMonad $ \_ s ->
    ((), s { gs_stroke_attr = fn $ gs_stroke_attr s })
  where
    fn sa = sa { line_join = a }


getDashPattern      :: PsMonad DashPattern
getDashPattern      = PsMonad $ \_ s -> (dash_pattern $ gs_stroke_attr s,s)

setDashPattern      :: DashPattern -> PsMonad ()
setDashPattern a    = PsMonad $ \_ s -> 
    ((), s { gs_stroke_attr = fn $ gs_stroke_attr s })
  where
    fn sa = sa { dash_pattern = a }


--------------------------------------------------------------------------------
-- Render to PostScript

-- | Output a series of pictures to a Postscript file. Each 
-- picture will be printed on a separate page. 
--
writePS :: (Real u, Floating u, PSUnit u) 
        => FilePath -> TextEncoder -> [Picture u] -> IO ()
writePS filepath enc pic = 
    getZonedTime >>= \ztim -> writeFile filepath (show $ psDraw ztim enc pic)

-- | Output a picture to an EPS (Encapsulated PostScript) file. 
-- The .eps file can then be imported or embedded in another 
-- document.
--
writeEPS :: (Real u, Floating u, PSUnit u)  
         => FilePath -> TextEncoder -> Picture u -> IO ()
writeEPS filepath enc pic =
    getZonedTime >>= \ztim -> writeFile filepath (show $ epsDraw ztim enc pic)


-- | Version of 'writePS' - using Latin1 encoding.
-- 
writePS_latin1 :: (Real u, Floating u, PSUnit u) 
               => FilePath -> [Picture u] -> IO ()
writePS_latin1 filepath = writePS filepath latin1Encoder

-- | Version of 'writeEPS' - using Latin1 encoding. 
--
writeEPS_latin1 :: (Real u, Floating u, PSUnit u)  
                => FilePath -> Picture u -> IO ()
writeEPS_latin1 filepath = writeEPS filepath latin1Encoder

--------------------------------------------------------------------------------
-- Internals


-- | Draw a picture, generating PostScript output.
--
-- Note - the bounding box may /below the origin/ - if it is, it
-- will need translating.
--

psDraw :: (Real u, Floating u, PSUnit u) 
       => ZonedTime -> TextEncoder -> [Picture u] -> Doc
psDraw timestamp enc pics = 
    let body = vcat $ runPsMonad enc $ zipWithM psDrawPage pages pics
    in vcat [ psHeader 1 timestamp
            , body
            , psFooter 
            ]
  where
    pages = map (\i -> (show i,i)) [1..]

-- | Note the bounding box may /below the origin/ - if it is, it
-- will need translating.
--
psDrawPage :: (Real u, Floating u, PSUnit u)
           => (String,Int) -> Picture u -> PsMonad Doc
psDrawPage (lbl,ordinal) pic = 
    let (_,cmdtrans) = imageTranslation pic in 
    (\doc -> vcat [ dsc_Page lbl ordinal
                  , ps_gsave
                  , cmdtrans
                  , doc
                  , ps_grestore
                  , ps_showpage
                  ]) 
      <$> picture pic


-- | Note the bounding box may /below the origin/ - if it is, it
-- will need translating.
--
epsDraw :: (Real u, Floating u, PSUnit u)
        => ZonedTime -> TextEncoder -> Picture u -> Doc
epsDraw timestamp enc pic =
    let (bb,cmdtrans) = imageTranslation pic 
        body          = runPsMonad enc (picture pic) 
    in vcat [ epsHeader bb timestamp
            , ps_gsave
            , cmdtrans
            , body
            , ps_grestore
            , epsFooter
            ]


imageTranslation :: (Ord u, PSUnit u) => Picture u -> (BoundingBox u, Doc)
imageTranslation pic = case repositionDeltas pic of
  (bb, Nothing) -> (bb, empty)
  (bb, Just v)  -> (bb, ps_translate v)


--------------------------------------------------------------------------------

-- Note - PostScript ignotes any FontCtx changes via the @Group@
-- constructor.
--
picture :: (Real u, Floating u, PSUnit u) => Picture u -> PsMonad Doc
picture (Leaf    (_,xs) ones)   = bracketTrafos xs $ revConcat primitive ones
picture (Picture (_,xs) ones)   = bracketTrafos xs $ revConcat picture ones
picture (Clip    (_,xs) cp pic) = bracketTrafos xs $
                                    (vconcat <$> clipPath cp <*> picture pic)
picture (Group   (_,xs) _ pic) = bracketTrafos xs (picture pic)


revConcat :: (a -> PsMonad Doc) -> OneList a -> PsMonad Doc
revConcat fn ones = some empty <$> F.foldrM step None ones
  where
    step e ac = (\d -> d `conc` ac) <$> fn e
    conc d None      = Some d
    conc d (Some ac) = Some $ ac `vconcat` d


primitive :: (Real u, Floating u, PSUnit u) => Primitive u -> PsMonad Doc
primitive (PPath props _ pp)     = primPath props pp
primitive (PLabel props _ lbl)   = primLabel props lbl
primitive (PEllipse props _ ell) = primEllipse props ell


primPath :: PSUnit u
         => PathProps -> PrimPath u -> PsMonad Doc
primPath (CFill rgb)     p = 
    (\rgbd -> vcat [rgbd, makeStartPath p, ps_closepath, ps_fill]) 
      <$> deltaDrawColour rgb  

primPath (CStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [ rgbd, attrd, makeStartPath p
                         , ps_closepath, ps_stroke ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs
 
primPath (OStroke attrs rgb) p = 
    (\rgbd attrd -> vcat [rgbd, attrd, makeStartPath p, ps_stroke]) 
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs attrs

primPath (CFillStroke fc attrs sc) p = 
    (\d1 d2 -> vcat [d1,d2])
      <$> primPath (CFill fc) p <*> primPath (CStroke attrs sc) p


clipPath :: PSUnit u => PrimPath u -> PsMonad Doc
clipPath p = pure $ vcat [makeStartPath p , ps_closepath, ps_clip]


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
    drawF (EFill rgb)            pt = fillArcPath rgb hw pt
    drawF (EStroke sa rgb)       pt = strokeArcPath rgb sa hw pt
    drawF (EFillStroke fc sa sc) pt = 
        vconcat <$> fillArcPath fc hw pt <*>  strokeArcPath sc sa hw pt
                       


-- This will need to become monadic to handle /colour delta/.
--
fillArcPath :: PSUnit u => RGBi -> u -> Point2 u -> PsMonad Doc
fillArcPath rgb radius pt = 
    (\rgbd -> vcat [ rgbd
                   , ps_newpath
                   , ps_arc pt radius 0 360
                   , ps_closepath
                   , ps_fill ])
      <$> deltaDrawColour rgb

strokeArcPath :: PSUnit u 
              => RGBi -> StrokeAttr -> u -> Point2 u -> PsMonad Doc
strokeArcPath rgb sa radius pt =
    (\rgbd attrd -> vcat [ rgbd
                         , attrd
                         , ps_newpath
                         , ps_arc pt radius 0 360
                         , ps_closepath
                         , ps_stroke ])
      <$> deltaDrawColour rgb <*> deltaStrokeAttrs sa


-- Note - for the otherwise case, the x-and-y coordinates are 
-- encoded in the matrix, hence the @ 0 0 moveto @.
--
primLabel :: (Real u, Floating u, PSUnit u) 
          => LabelProps -> PrimLabel u -> PsMonad Doc
primLabel (LabelProps rgb font) (PrimLabel basept body ctm) = 
    bracketPrimCTM basept ctm mf
  where
    mf pt = (\rgbd fontd showd -> vcat [ rgbd, fontd, ps_moveto pt, showd ]) 
              <$> deltaDrawColour rgb <*> deltaFontAttrs font 
                                      <*> labelBody body

labelBody :: LabelBody u -> PsMonad Doc
labelBody (StdLayout txt) = encodedText txt

encodedText :: EncodedText -> PsMonad Doc 
encodedText etext = vcat <$> (mapM textChunk $ getEncodedText etext)


textChunk :: TextChunk -> PsMonad Doc
textChunk (TextSpan s)    = pure (ps_show $ escapeSpecial s)
textChunk (TextEscName s) = pure (ps_glyphshow s)
textChunk (TextEscInt i)  = (either failk ps_glyphshow) <$> askCharCode i 
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
--
-- Note - because these combinators return /empty/ for no 
-- difference some extraneous blank lines are produced in the 
-- output.
--

deltaDrawColour :: RGBi -> PsMonad Doc
deltaDrawColour rgb = getDrawColour >>= \inh -> 
   if rgb==inh then return empty
               else setDrawColour rgb >> return (ps_setrgbcolor rgb)


deltaStrokeAttrs :: StrokeAttr -> PsMonad Doc
deltaStrokeAttrs sa = 
    (\d1 d2 d3 d4 d5 -> hcat $ catMaybes [d1,d2,d3,d4,d5])  
      <$> lw <*> ml <*> lc <*> lj <*> dp
  where
    lw = let d = line_width sa in 
         getLineWidth >>= \inh -> 
         if d == inh 
              then return Nothing 
              else setLineWidth d >> return (Just $ ps_setlinewidth d)

    ml = let d = miter_limit sa in 
         getMiterLimit >>= \inh -> 
         if d==inh 
              then return Nothing 
              else setMiterLimit d >> return (Just $ ps_setmiterlimit d)
                            
    lc = let d = line_cap sa in
         getLineCap >>= \inh -> 
         if d==inh 
              then return Nothing
              else setLineCap d >> return (Just $ ps_setlinecap d)
                      
    lj = let d = line_join sa in 
         getLineJoin >>= \inh -> 
         if d==inh 
              then return Nothing
              else setLineJoin d >> return (Just $ ps_setlinejoin d)

    dp = let d = dash_pattern sa in
         getDashPattern >>= \inh -> 
         if d==inh 
               then return Nothing
               else setDashPattern d >> return (Just $ ps_setdash d)



deltaFontAttrs :: FontAttr -> PsMonad Doc
deltaFontAttrs fa  = getFontAttr >>= \inh ->
    if fa==inh then return empty 
               else setFontAttr fa >> return (makeFontAttrs fa)

makeFontAttrs :: FontAttr -> Doc
makeFontAttrs (FontAttr sz face) = 
    vcat [ ps_findfont (font_name face), ps_scalefont sz, ps_setfont ]


--------------------------------------------------------------------------------
-- Bracket matrix and PrimCTM trafos

bracketTrafos :: (Real u, Floating u, PSUnit u) 
              => [AffineTrafo u] -> PsMonad Doc -> PsMonad Doc
bracketTrafos xs ma = bracketMatrix (concatTrafos xs) ma 

bracketMatrix :: (Fractional u, PSUnit u) 
              => Matrix3'3 u -> PsMonad Doc -> PsMonad Doc
bracketMatrix mtrx ma 
    | mtrx == identityMatrix = ma
    | otherwise              = (\doc -> vcat [inn, doc, out]) <$> ma
  where
    inn   = ps_concat $ mtrx
    out   = ps_concat $ invert mtrx


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

