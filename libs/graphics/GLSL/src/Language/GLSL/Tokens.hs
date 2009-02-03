{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Tokens
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Token data type for the lexer.
-- 
--------------------------------------------------------------------------------


module Language.GLSL.Tokens  where

data GlslToken 
    -- keywords
    = Tk_kw_attribute
    | Tk_kw_const 
    | Tk_kw_uniform 
    | Tk_kw_varying
    | Tk_kw_centroid
    | Tk_kw_break 
    | Tk_kw_continue 
    | Tk_kw_do 
    | Tk_kw_for 
    | Tk_kw_while
    | Tk_kw_if 
    | Tk_kw_else
    | Tk_kw_in 
    | Tk_kw_out 
    | Tk_kw_inout
    | Tk_kw_float 
    | Tk_kw_int 
    | Tk_kw_void 
    | Tk_kw_bool 
    | Tk_kw_true 
    | Tk_kw_false
    | Tk_kw_invariant
    | Tk_kw_discard 
    | Tk_kw_return
    | Tk_kw_mat2 
    | Tk_kw_mat3 
    | Tk_kw_mat4
    | Tk_kw_mat2x2 
    | Tk_kw_mat2x3 
    | Tk_kw_mat2x4
    | Tk_kw_mat3x2 
    | Tk_kw_mat3x3 
    | Tk_kw_mat3x4
    | Tk_kw_mat4x2 
    | Tk_kw_mat4x3 
    | Tk_kw_mat4x4
    | Tk_kw_vec2 
    | Tk_kw_vec3 
    | Tk_kw_vec4 
    | Tk_kw_ivec2 
    | Tk_kw_ivec3 
    | Tk_kw_ivec4 
    | Tk_kw_bvec2 
    | Tk_kw_bvec3 
    | Tk_kw_bvec4
    | Tk_kw_sampler1D 
    | Tk_kw_sampler2D 
    | Tk_kw_sampler3D 
    | Tk_kw_samplerCube
    | Tk_kw_sampler1DShadow 
    | Tk_kw_sampler2DShadow
    | Tk_kw_struct
    -- more
    | Tk_Period
    | Tk_Integer Integer
    | Tk_EOF
  deriving (Eq,Show)



        