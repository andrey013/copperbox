{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Syntax library for GLSL the OpenGL Shading Language.
-- 
--------------------------------------------------------------------------------

module Language.GLSL (
  module Language.GLSL.Parse,
--  module Language.GLSL.Pretty,
  module Language.GLSL.Syntax
)
where

import Language.GLSL.Parse
import Language.GLSL.Pretty ()
import Language.GLSL.Syntax

