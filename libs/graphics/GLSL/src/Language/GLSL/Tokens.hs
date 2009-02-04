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
    -- punctuators
    | Tk_p_plus
    | Tk_p_dash
    | Tk_p_bang
    | Tk_p_tilde
    | Tk_p_star
    | Tk_p_divide
    | Tk_p_percent
    | Tk_p_shiftl
    | Tk_p_shiftr
    | Tk_p_less
    | Tk_p_greater
    | Tk_p_lesseq
    | Tk_p_greatereq
    | Tk_p_equality
    | Tk_p_notequal
    | Tk_p_ampersand
    | Tk_p_caret
    | Tk_p_bar
    | Tk_p_dblampersand 
    | Tk_p_dblcaret
    | Tk_p_dblbar 
    | Tk_p_eq
    | Tk_p_stareq
    | Tk_p_divideeq
    | Tk_p_percenteq
    | Tk_p_pluseq
    | Tk_p_minuseq
    | Tk_p_shiftleq
    | Tk_p_shiftreq
    | Tk_p_ampersandeq
    | Tk_p_careteq
    | Tk_p_bareq
    
    | Tk_Period
    | Tk_Integer Integer
    | Tk_EOF
  deriving (Eq,Show)



        