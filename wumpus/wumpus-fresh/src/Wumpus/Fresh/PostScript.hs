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


askCharCode :: Int -> PsMonad (Either GlyphName GlyphName)
askCharCode i = PsMonad $ \r -> case lookupByCharCode i r of
    Just n  -> Right n
    Nothing -> Left $ ps_fallback r

--------------------------------------------------------------------------------

-- outputting labels needs an environment containing a TextEncoder

-- This will need to become monadic to handle /colour delta/.
--
fillArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> Doc
fillArcPath rgb radius pt = 
    vcat [ ps_newpath,  ps_arc pt radius 0 360, ps_closepath, ps_fill ]

strokeArcPath :: PSUnit u => RGB255 -> u -> Point2 u -> Doc
strokeArcPath rgb radius pt = 
    vcat [ ps_newpath,  ps_arc pt radius 0 360, ps_closepath, ps_stroke ]


pathSegment :: PSUnit u => PrimPathSegment u -> Doc
pathSegment (PLineTo p1)        = ps_lineto p1 
pathSegment (PCurveTo p1 p2 p3) = ps_curveto p1 p2 p3 


textChunk :: TextChunk -> PsMonad Doc
textChunk (SText s)  = pure (ps_show $ escapeSpecial s)
textChunk (EscStr s) = pure (ps_glyphshow s)
textChunk (EscInt i) = (either failk ps_glyphshow) <$> askCharCode i 
  where
    failk gname = missingCharCode i gname


bracketPrimCTM :: forall u. (Real u, Floating u, PSUnit u)
               => Point2 u -> PrimCTM u -> PsMonad Doc -> PsMonad Doc
bracketPrimCTM pt@(P2 x y) ctm mf 
    | ctm == identityCTM  = (\doc -> vcat [ps_moveto pt,doc]) <$> mf
    | otherwise           = (\doc -> vcat [inn, ps_moveto zeroPt', doc, out])
                              <$> mf
  where
    zeroPt' :: Point2 u
    zeroPt' = zeroPt

    mtrx  = translMatrixRepCTM x y ctm
    inn   = ps_concat $ mtrx
    out   = ps_concat $ invert mtrx
