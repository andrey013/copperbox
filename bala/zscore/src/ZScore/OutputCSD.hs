{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.OutputCSD
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output combined orchestra and score files (.csd files). 
--
--------------------------------------------------------------------------------

module ZScore.OutputCSD
  (

  -- * Output a CSD file
    writeCSD


  ) where




{-

-- DESIGN NOTE
--
-- SVG output is complicated by two differences with PostScript.
--
-- 1. The coordinate space of SVG is /origin top-left/, for 
-- PostScript it is /origin bottom-left/.
-- 
-- 2. Clipping in SVG uses /tagging/. A clipPath element is 
-- declared and named, subsequent elements within the clipping 
-- area reference it via the clip-path attribute - 
-- @clip-path=\"url(#clip_path_tag)\"@.
--


-- SvgMonad is two Readers plus Int state for clip paths...
--

type ClipCount = Int

newtype SvgMonad a = SvgMonad { 
          getSvgMonad :: GraphicsState -> ClipCount -> (a,ClipCount) }



instance Functor SvgMonad where
  fmap f mf = SvgMonad $ \r s -> let (a,s1) = getSvgMonad mf r s
                                 in (f a,s1)

instance Applicative SvgMonad where
  pure a    = SvgMonad $ \_ s -> (a,s)
  mf <*> ma = SvgMonad $ \r s -> let (f,s1) = getSvgMonad mf r s
                                     (a,s2) = getSvgMonad ma r s1
                                 in (f a, s2)

instance Monad SvgMonad where
  return a  = SvgMonad $ \_ s -> (a,s)
  m >>= k   = SvgMonad $ \r s -> let (a,s1) = getSvgMonad m r s
                                 in (getSvgMonad . k) a r s1
                            


runSvgMonad :: SvgMonad a -> a
runSvgMonad mf = fst $ getSvgMonad mf zeroGS 0

newClipLabel :: SvgMonad String
newClipLabel = SvgMonad $ \_ s -> ('c':'l':'i':'p':show s, s+1)



-- This is different to the PsMonad version, as SVG is nested 
-- (and /graphics state/ is via a Reader), so it is the same as 
-- local with a Reader monad.
--
runLocalGS :: (GraphicsState -> GraphicsState) -> SvgMonad a -> SvgMonad a
runLocalGS upd mf = 
    SvgMonad $ \r s -> getSvgMonad mf (upd r) s


askGraphicsState :: SvgMonad GraphicsState
askGraphicsState = SvgMonad $ \r s -> (r,s)

asksGraphicsState :: (GraphicsState -> a) -> SvgMonad a
asksGraphicsState fn = fmap fn askGraphicsState

askFontAttr     :: SvgMonad FontAttr
askFontAttr     = asksGraphicsState $ \r -> 
                    FontAttr (gs_font_size r) (gs_font_face r)

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


svgChar :: EscapedChar -> Doc
svgChar (CharLiteral c) | ord c < 0x80  = char c
svgChar (CharLiteral c)                 = escapeSpecial $ ord c
svgChar (CharEscInt i)                  = escapeSpecial i
svgChar (CharEscName s)                 = 
   escapeSpecial $ fromMaybe 0x0020 $ Map.lookup s ps_glyph_indices 

--------------------------------------------------------------------------------
-}

-- | Output a picture to a SVG file. 
--
writeCSD :: FilePath -> () -> IO ()
writeCSD filepath csd = 
    writeFile filepath $ "TODO"  




