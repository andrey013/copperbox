{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Hawa.PrettyPrim
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- primitive ops
--
--------------------------------------------------------------------------------

module Graphics.Hawa.PrettyPrim where

import Graphics.Hawa.Prims

import Text.PrettyPrint.Leijen

type Name = String

arity0 :: Name -> Doc 
arity1 :: Pretty a => Name -> a -> Doc 
arity2 :: (Pretty a, Pretty b) => Name -> a -> b -> Doc 

arity0          = text 
arity1 s x      = pretty x <+> text s
arity2 s x y    = pretty x <+> pretty y <+> text s


ppop :: PrimOp -> Doc
 
ppop Pop                          = arity0 "pop"

      -- Path ops  
ppop (Moveto            x   y)    = arity2 "moveto"       x   y
ppop (Lineto            x   y)    = arity2 "lineto"       x   y
ppop Closepath                    = arity0 "closepath"

      -- paint ops
ppop Stroke                       = arity0 "stroke"


      