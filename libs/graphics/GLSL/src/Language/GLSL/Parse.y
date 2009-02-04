--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Parse
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Lexer
-- 
--------------------------------------------------------------------------------

{
{-# OPTIONS -Wall #-}

module Language.GLSL.Parse where

import Language.GLSL.Lex
import Language.GLSL.ParseMonad
import Language.GLSL.Syntax
import Language.GLSL.Tokens


import Control.Monad.Identity



parseGlsl :: FilePath -> String -> Integer
parseGlsl path contents  = 
  case runIdentity (runParseT glslParser path contents) of
    Left err -> error "ERRR"
    Right a -> a 

}

%name glslParser

%lexer { glslLex } { L _ Tk_EOF }
%monad { ParseM } { >>= } { return }
%error { parseError }


%tokentype { Lexeme }

%token 
  ':'       { L _ Tk_Period }
  INT       { L _ (Tk_Integer $$) }

  'attribute'         { L _ Tk_kw_attribute       }
  'const'             { L _ Tk_kw_const           }
  'uniform'           { L _ Tk_kw_uniform         } 
  'varying'           { L _ Tk_kw_varying         }
  'centroid'          { L _ Tk_kw_centroid        }
  'break'             { L _ Tk_kw_break           }
  'continue'          { L _ Tk_kw_continue        }
  'do'                { L _ Tk_kw_do              }
  'for'               { L _ Tk_kw_for             }
  'while'             { L _ Tk_kw_while           }
  'if'                { L _ Tk_kw_if              }
  'else'              { L _ Tk_kw_else            }
  'in'                { L _ Tk_kw_in              }
  'out'               { L _ Tk_kw_out             }
  'inout'             { L _ Tk_kw_inout           }
  'float'             { L _ Tk_kw_float           }
  'int'               { L _ Tk_kw_int             }
  'void'              { L _ Tk_kw_void            }
  'bool'              { L _ Tk_kw_bool            }
  'true'              { L _ Tk_kw_true            }
  'false'             { L _ Tk_kw_false           }
  'invariant'         { L _ Tk_kw_invariant       }
  'discard'           { L _ Tk_kw_discard         }
  'return'            { L _ Tk_kw_return          }
  'mat2'              { L _ Tk_kw_mat2            }
  'mat3'              { L _ Tk_kw_mat3            }
  'mat4'              { L _ Tk_kw_mat4            }
  'mat2x2'            { L _ Tk_kw_mat2x2          }
  'mat2x3'            { L _ Tk_kw_mat2x3          }
  'mat2x4'            { L _ Tk_kw_mat2x4          }
  'mat3x2'            { L _ Tk_kw_mat3x2          }
  'mat3x3'            { L _ Tk_kw_mat3x3          }
  'mat3x4'            { L _ Tk_kw_mat3x4          }
  'mat4x2'            { L _ Tk_kw_mat4x2          }
  'mat4x3'            { L _ Tk_kw_mat4x3          } 
  'mat4x4'            { L _ Tk_kw_mat4x4          }
  'vec2'              { L _ Tk_kw_vec2            }
  'vec3'              { L _ Tk_kw_vec3            }
  'vec4'              { L _ Tk_kw_vec4            }
  'ivec2'             { L _ Tk_kw_ivec2           }
  'ivec3'             { L _ Tk_kw_ivec3           }
  'ivec4'             { L _ Tk_kw_ivec4           } 
  'bvec2'             { L _ Tk_kw_bvec2           }
  'bvec3'             { L _ Tk_kw_bvec3           } 
  'bvec4'             { L _ Tk_kw_bvec4           }
  'sampler1D'         { L _ Tk_kw_sampler1D       } 
  'sampler2D'         { L _ Tk_kw_sampler2D       } 
  'sampler3D'         { L _ Tk_kw_sampler3D       } 
  'samplerCube'       { L _ Tk_kw_samplerCube     }
  'sampler1DShadow'   { L _ Tk_kw_sampler1DShadow } 
  'sampler2DShadow'   { L _ Tk_kw_sampler2DShadow }
  'struct'            { L _ Tk_kw_struct          }
     


  
%%

int :: { Integer }
  : INT             { $1 }

parameter_qualifier :: { SlParamQual }
  : 'in'            { In }
  | 'out'           { Out }
  | 'inout'         { Inout }


type_qualifier :: { SlTypeQual }
  : 'const'                             { Const }
  | 'attribute'                         { Attribute }
  | 'varying'                           { Varying [] }
  | 'centroid'  'varying'               { Varying [Centroid] }
  | 'invariant' 'varying'               { Varying [Invariant] }
  | 'invariant' 'centroid' 'varying'    { Varying [Invariant, Centroid] }
  | 'uniform'                           { Uniform }
  
  
  
{

-- some haskell



}  