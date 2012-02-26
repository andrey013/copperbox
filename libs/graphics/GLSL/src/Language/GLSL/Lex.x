

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.GLSL.Lex
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


module Language.GLSL.Lex where

import Language.GLSL.ParseMonad
import Language.GLSL.Token

import Control.Applicative
import Control.Monad.Identity

}



$white                  = [\ \t\v\f\r\n]

$nondigit               = [A-Za-z_]
$digit                  = 0-9
$nonzero_digit          = [1-9]
$octal_digit            = [0-7]
$hex_digit              = [0-9A-Fa-f]

$identifier_nondigit    = $nondigit
$identifier             = [$nondigit $digit]


@hex_prefix         = 0x | 0X


@integer = $digit+
@float   = $digit+ \. $digit+

glsl :-

$white ;

-- Comment - cpp -E seems to fill in with lines like `# 1 "<command line>"`
<0> {
  ^\# [.]*     { skip }
  ^\/\/[.]*    { skip }
}
  


-- keywords  

<0> {
  "attribute"         { keyword Tk_kw_attribute       }
  "const"             { keyword Tk_kw_const           }
  "bool"              { keyword Tk_kw_bool            }
  "float"             { keyword Tk_kw_float           }
  "int"               { keyword Tk_kw_int             }

  "break"             { keyword Tk_kw_break           }
  "continue"          { keyword Tk_kw_continue        }
  "do"                { keyword Tk_kw_do              }
  "else"              { keyword Tk_kw_else            }
  "for"               { keyword Tk_kw_for             }
  "if"                { keyword Tk_kw_if              }
  "discard"           { keyword Tk_kw_discard         }
  "return"            { keyword Tk_kw_return          }

  "bvec2"             { keyword Tk_kw_bvec2           }
  "bvec3"             { keyword Tk_kw_bvec3           }
  "bvec4"             { keyword Tk_kw_bvec4           }
  "ivec2"             { keyword Tk_kw_ivec2           }
  "ivec3"             { keyword Tk_kw_ivec3           }
  "ivec4"             { keyword Tk_kw_ivec4           }
  "vec2"              { keyword Tk_kw_vec2            }
  "vec3"              { keyword Tk_kw_vec3            }
  "vec4"              { keyword Tk_kw_vec4            }
  
  "mat2"              { keyword Tk_kw_mat2            }
  "mat3"              { keyword Tk_kw_mat3            }
  "mat4"              { keyword Tk_kw_mat4            }
  "in"                { keyword Tk_kw_in              }
  "out"               { keyword Tk_kw_out             }
  "inout"             { keyword Tk_kw_inout           }
  "uniform"           { keyword Tk_kw_uniform         }
  "varying"           { keyword Tk_kw_varying         }
  "centroid"          { keyword Tk_kw_centroid        }

  "mat2x2"            { keyword Tk_kw_mat2x2          }
  "mat2x3"            { keyword Tk_kw_mat2x3          }
  "mat2x4"            { keyword Tk_kw_mat2x4          }
  "mat3x2"            { keyword Tk_kw_mat3x2          }
  "mat3x3"            { keyword Tk_kw_mat3x3          }
  "mat3x4"            { keyword Tk_kw_mat3x4          }
  "mat4x2"            { keyword Tk_kw_mat4x2          }
  "mat4x3"            { keyword Tk_kw_mat4x3          }
  "mat4x4"            { keyword Tk_kw_mat4x4          }

  "sampler1D"         { keyword Tk_kw_sampler1D       }
  "sampler2D"         { keyword Tk_kw_sampler2D       }
  "sampler3D"         { keyword Tk_kw_sampler3D       }
  "samplerCube"       { keyword Tk_kw_samplerCube     }
  "sampler1DShadow"   { keyword Tk_kw_sampler1DShadow }
  "sampler2DShadow"   { keyword Tk_kw_sampler2DShadow }
  
  "struct"            { keyword Tk_kw_struct          }
  "void"              { keyword Tk_kw_void            }
  "while"             { keyword Tk_kw_while           }
  
  "invariant"         { keyword Tk_kw_invariant       }

} 

-- identifier
<0> {
  $identifier_nondigit $identifier*     { identifier }
}

-- operators
<0> {
  
  "<<"            { operator Tk_left_op           }
  ">>"            { operator Tk_right_op          }
  "++"            { operator Tk_inc_op            }
  "--"            { operator Tk_dec_op            }
  
  "<="            { operator Tk_le_op             } 
  ">="            { operator Tk_ge_op             }
  "=="            { operator Tk_eq_op             }
  "!="            { operator Tk_ne_op             }
  
  \&              { operator Tk_and_op            }
  \|              { operator Tk_or_op             }
  \^              { operator Tk_xor_op            }
  "*="            { operator Tk_mul_assign        }
  "/="            { operator Tk_div_assign        }
  
  "+="            { operator Tk_add_assign        }
  "%="            { operator Tk_mod_assign        }
  "<<="           { operator Tk_left_assign       }
  ">>="           { operator Tk_right_assign      }
  "&="            { operator Tk_and_assign        }
  "^="            { operator Tk_xor_assign        }
  "|="            { operator Tk_or_assign         }
  "-="            { operator Tk_sub_assign        }
}


-- punctuators
<0> {

  \(              { punctuator Tk_p_left_paren    }
  \)              { punctuator Tk_p_right_paren   }
  \[              { punctuator Tk_p_left_bracket  }
  \]              { punctuator Tk_p_right_bracket }
  \{              { punctuator Tk_p_left_brace    }
  \}              { punctuator Tk_p_right_brace   }
  \.              { punctuator Tk_p_dot           }
  
  \,              { punctuator Tk_p_comma         }
  \:              { punctuator Tk_p_colon         }
  \=              { punctuator Tk_p_equal         }
  \;              { punctuator Tk_p_semicolon     }
  \!              { punctuator Tk_p_bang          }
  \-              { punctuator Tk_p_dash          }
  \~              { punctuator Tk_p_tilde         }
  \+              { punctuator Tk_p_plus          }
  \*              { punctuator Tk_p_star          }
  \/              { punctuator Tk_p_slash         }
  \%              { punctuator Tk_p_percent       }

  \<              { punctuator Tk_p_left_angle    }
  \>              { punctuator Tk_p_right_angle   }
  \|              { punctuator Tk_p_vertical_bar  }
  \^              { punctuator Tk_p_caret         }
  \&              { punctuator Tk_p_ampersand     }
  \?              { punctuator Tk_p_question      }
  
                
}

-- integer constants
<0> {
  
  $nonzero_digit $digit*      { intLiteral }
  
  0 $octal_digit*             { octLiteral }
  
  @hex_prefix $hex_digit*     { hexLiteral }

} 

-- float constants
<0> { 

   @float                     { floatLiteral }
}


{

type ParseM a = ParseT Identity a

alexEOF :: (Functor m, Monad m) => ParseT m Lexeme
alexEOF = (\pos -> L pos Tk_EOF) <$> getPosition



  

intLiteral :: AlexInput -> Int -> ParseM Lexeme
intLiteral = usingInput L (Tk_lit_int . read)

-- TODO - watch out for an error on read 
octLiteral :: AlexInput -> Int -> ParseM Lexeme
octLiteral = usingInput L (Tk_lit_int . read . traf) 
  where
    traf ('0':s) = '0':'o':s
    traf s       = s    -- this will probably cause a read error which is bad

hexLiteral :: AlexInput -> Int -> ParseM Lexeme
hexLiteral = usingInput L (Tk_lit_int . read)


floatLiteral :: AlexInput -> Int -> ParseM Lexeme
floatLiteral = usingInput L Tk_lit_float

boolLiteral :: AlexInput -> Int -> ParseM Lexeme
boolLiteral = usingInput L (Tk_lit_bool . fn) 
  where 
    fn "true"   = True
    fn "false"  = False



keyword :: GlslToken -> AlexInput -> Int -> ParseM Lexeme
keyword kw = usingInput L (const kw) 

punctuator :: GlslToken -> AlexInput -> Int -> ParseM Lexeme
punctuator pr = usingInput L (const pr)

operator :: GlslToken -> AlexInput -> Int -> ParseM Lexeme
operator op = usingInput L (const op)


identifier :: AlexInput -> Int -> ParseM Lexeme 
identifier = usingInput L Tk_ident


glslLex :: (Lexeme -> ParseM a) -> ParseM a
glslLex k = lexToken >>= k


--------------------------------------------------------------------------------
-- 

-- These functions are generic, but as the type AlexReturn(..) is
-- generated by Alex we can't move them into ParseMonad without a circular
-- dependency. 
-- Seemingly we have to give them the most specfic type as well. 

lexToken :: ParseM Lexeme
lexToken = do 
    inp <- getLexerState
    case alexScan inp (start_code inp) of
      AlexEOF                     -> alexEOF
      AlexError _inp'             -> lexError "lexical error"
      AlexSkip  inp' _len         -> do 
          setLexerState inp'
          lexToken
      AlexToken inp' len action   -> do 
          setLexerState inp'
          action inp len

begin :: Int -> LexerState -> Int -> ParseM Lexeme 
begin code input len = do setLexerStateStartCode code; lexToken

andBegin :: (LexerState -> Int -> ParseM Lexeme) 
                -> Int -> LexerState -> Int -> ParseM Lexeme
andBegin action code input len = do 
    setLexerStateStartCode code
    action input len        


skip :: LexerState -> Int -> ParseM Lexeme 
skip _input _len = lexToken


}


