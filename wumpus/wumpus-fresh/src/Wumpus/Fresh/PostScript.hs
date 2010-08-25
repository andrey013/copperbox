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

import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.FreshIR
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PostScriptDoc
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

import Control.Applicative
-- import Data.Time


--------------------------------------------------------------------------------
-- PsMonad

-- PsMonad is at least a Reader monad...
--
newtype PsMonad a = PsMonad { 
            getPsMonad :: TextEncoder -> a }


instance Functor PsMonad where
  fmap f mf = PsMonad $ \r -> let a = getPsMonad mf r
                              in f a

instance Applicative PsMonad where
  pure a    = PsMonad $ \_ -> a
  mf <*> ma = PsMonad $ \r -> let f = getPsMonad mf r
                                  a = getPsMonad ma r
                                in f a

instance Monad PsMonad where
  return a  = PsMonad $ \_ -> a
  m >>= k   = PsMonad $ \r -> let a = getPsMonad m r
                                  b = (getPsMonad . k) a r
                                in b


runPsMonad :: TextEncoder -> PsMonad a -> a
runPsMonad enc mf = getPsMonad mf enc

askCharCode :: Int -> PsMonad (Either GlyphName GlyphName)
askCharCode i = PsMonad $ \r -> case lookupByCharCode i r of
    Just n  -> Right n
    Nothing -> Left $ ps_fallback r

--------------------------------------------------------------------------------



primPath :: PSUnit u
           => PathProps -> PrimPath u -> PsMonad Doc
primPath (CFill rgb)     p = 
    (\doc -> vcat [doc, ps_closepath, ps_fill]) <$> startPath p

primPath (CStroke _ rgb) p = 
    (\doc -> vcat [doc, ps_closepath, ps_stroke]) <$> startPath p
 
primPath (OStroke _ rgb) p = 
    (\doc -> vcat [doc, ps_stroke]) <$> startPath p


primPath (CFillStroke f attrs s) p = (\d1 d2 -> vcat [d1,d2]) 
    <$> primPath (CFill f) p <*> primPath (CStroke attrs f) p

{-
outputPath (CStroke xs) c p =
    updatePen c xs $ startPath p >> ps_closepath >> ps_stroke

outputPath (OStroke xs) c p =
    updatePen c xs $ startPath p >> ps_stroke
-}

startPath :: PSUnit u => PrimPath u -> PsMonad Doc
startPath (PrimPath start xs) = 
    (\docs -> vcat $ ps_newpath : ps_moveto start : docs)
      <$> mapM pathSegment xs


-- Monadic or pure?
--
pathSegment :: PSUnit u => PrimPathSegment u -> PsMonad Doc
pathSegment (PLineTo p1)        = pure $ ps_lineto p1 
pathSegment (PCurveTo p1 p2 p3) = pure $ ps_curveto p1 p2 p3 


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
primEllipse props ell@(PrimEllipse center hw hh ctm) =
    bracketPrimCTM center (scaleCTM 1 (hh/hw) ctm) (drawF props)
  where
    drawF (EFill rgb)               pt = fillArcPath rgb hw pt
    drawF (EStroke attrs rgb)       pt = strokeArcPath rgb hw pt
    drawF (EFillStroke fc attrs sc) pt = 
        vconcat <$> fillArcPath fc hw pt <*>  strokeArcPath sc hw pt
                       

-- This will need to become monadic to handle /colour delta/.
--
fillArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> PsMonad Doc
fillArcPath rgb radius pt = pure $ 
    vcat [ ps_newpath,  ps_arc pt radius 0 360, ps_closepath, ps_fill ]

strokeArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> PsMonad Doc
strokeArcPath rgb radius pt = pure $ 
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
