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
import Wumpus.Fresh.SVGDoc
import Wumpus.Fresh.TextEncoder
import Wumpus.Fresh.Utils

import Control.Applicative

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




-- Note - it will be wise to make coordinate remapping and output
-- separate passes (unlike in Wumpus-Core). Then I\'ll at least 
-- be able to debug the remapped Picture.
--

{-
primPath :: PSUnit u
         => PathProps -> PrimPath u -> SvgMonad Doc
primPath (CFill rgb)     p = 
    (\svgp -> vcat [doc, ps_closepath, ps_fill]) <$> path p

primPath (CStroke _ rgb) p = 
    (\doc -> vcat [doc, ps_closepath, ps_stroke]) <$> startPath p
 
primPath (OStroke _ rgb) p = 
    (\doc -> vcat [doc, ps_stroke]) <$> startPath p


primPath (CFillStroke f attrs s) p = (\d1 d2 -> vcat [d1,d2]) 
    <$> primPath (CFill f) p <*> primPath (CStroke attrs f) p
-}

path :: PSUnit u => PrimPath u -> SvgMonad Doc
path (PrimPath start xs) = 
    pure $ svg_path_m start <+> separate space (map seg xs)
  where
    seg (PLineTo pt)        = svg_path_l pt
    seg (PCurveTo p1 p2 p3) = svg_path_c p1 p2 p3

suffixClose :: Doc -> Doc
suffixClose doc       = doc <+> char 'Z'
