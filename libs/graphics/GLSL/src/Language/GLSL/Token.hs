{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Token
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Token data type for the lexer.
-- 
--------------------------------------------------------------------------------


module Language.GLSL.Token 
  (

    GlslToken(..)

  ) where

data GlslToken 
    -- keywords
    = Tk_kw_attribute
    | Tk_kw_const
    | Tk_kw_bool 
    | Tk_kw_float 
    | Tk_kw_int 
    -- 
    | Tk_kw_break 
    | Tk_kw_continue 
    | Tk_kw_do 
    | Tk_kw_else
    | Tk_kw_for 
    | Tk_kw_if 
    | Tk_kw_discard 
    | Tk_kw_return
    -- 
    | Tk_kw_bvec2 
    | Tk_kw_bvec3 
    | Tk_kw_bvec4
    | Tk_kw_ivec2 
    | Tk_kw_ivec3 
    | Tk_kw_ivec4 
    | Tk_kw_vec2 
    | Tk_kw_vec3 
    | Tk_kw_vec4 
    --
    | Tk_kw_mat2 
    | Tk_kw_mat3 
    | Tk_kw_mat4    
    | Tk_kw_in 
    | Tk_kw_out 
    | Tk_kw_inout
    | Tk_kw_uniform 
    | Tk_kw_varying
    | Tk_kw_centroid
    -- 
    | Tk_kw_mat2x2 
    | Tk_kw_mat2x3 
    | Tk_kw_mat2x4
    | Tk_kw_mat3x2 
    | Tk_kw_mat3x3 
    | Tk_kw_mat3x4
    | Tk_kw_mat4x2 
    | Tk_kw_mat4x3 
    | Tk_kw_mat4x4
    --
    | Tk_kw_sampler1D 
    | Tk_kw_sampler2D 
    | Tk_kw_sampler3D 
    | Tk_kw_samplerCube
    | Tk_kw_sampler1DShadow 
    | Tk_kw_sampler2DShadow
    --
    | Tk_kw_struct
    | Tk_kw_void 
    | Tk_kw_while


    | Tk_ident String
    | Tk_tyname String
    | Tk_lit_int Integer
    | Tk_lit_float String
    | Tk_lit_bool Bool
    
    | Tk_kw_invariant

    -- operators
    | Tk_left_op
    | Tk_right_op
    | Tk_inc_op
    | Tk_dec_op
    | Tk_le_op
    | Tk_ge_op
    | Tk_eq_op
    | Tk_ne_op
    | Tk_and_op
    | Tk_or_op
    | Tk_xor_op
    | Tk_mul_assign
    | Tk_div_assign
    | Tk_add_assign
    | Tk_mod_assign
    | Tk_left_assign
    | Tk_right_assign
    | Tk_and_assign
    | Tk_xor_assign
    | Tk_or_assign
    | Tk_sub_assign
    
    -- punctuators
    | Tk_p_left_paren
    | Tk_p_right_paren
    | Tk_p_left_bracket
    | Tk_p_right_bracket
    | Tk_p_left_brace
    | Tk_p_right_brace
    | Tk_p_dot
    | Tk_p_comma
    | Tk_p_colon
    | Tk_p_equal
    | Tk_p_semicolon
    | Tk_p_bang
    | Tk_p_dash
    | Tk_p_tilde
    | Tk_p_plus 
    | Tk_p_star
    | Tk_p_slash 
    | Tk_p_percent
    | Tk_p_left_angle
    | Tk_p_right_angle
    | Tk_p_vertical_bar
    | Tk_p_caret
    | Tk_p_ampersand
    | Tk_p_question
    -- EOF
    | Tk_EOF
  deriving (Eq,Show)



        