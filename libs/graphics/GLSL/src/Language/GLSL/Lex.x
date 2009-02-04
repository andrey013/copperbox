

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
import Language.GLSL.Tokens

import Control.Applicative
import Control.Monad.Identity

}



$white = [\ \t\v\f\r\n]

$nondigit           = [A-Za-z_]
$digit              = 0-9
$nonzero_digit      = [1-9]
$octal_digit        = [0-7]
$hex_digit          = [0-9A-Fa-f]

@hex_prefix         = 0x | 0X



@integer = $digit+

glsl :-

$white ;


-- keywords  

<0> {
  "attribute"         { keyword Tk_kw_attribute       }
  "const"             { keyword Tk_kw_const           } 
  "uniform"           { keyword Tk_kw_uniform         }
  "varying"           { keyword Tk_kw_varying         }
  "centroid"          { keyword Tk_kw_centroid        }
  "break"             { keyword Tk_kw_break           }
  "continue"          { keyword Tk_kw_continue        }
  "do"                { keyword Tk_kw_do              }
  "for"               { keyword Tk_kw_for             }
  "while"             { keyword Tk_kw_while           }
  "if"                { keyword Tk_kw_if              }
  "else"              { keyword Tk_kw_else            }
  "in"                { keyword Tk_kw_in              }
  "out"               { keyword Tk_kw_out             }
  "inout"             { keyword Tk_kw_inout           }
  "float"             { keyword Tk_kw_float           }
  "int"               { keyword Tk_kw_int             }
  "void"              { keyword Tk_kw_void            }
  "bool"              { keyword Tk_kw_bool            }
  "true"              { keyword Tk_kw_true            }
  "false"             { keyword Tk_kw_false           }
  "invariant"         { keyword Tk_kw_invariant       }
  "discard"           { keyword Tk_kw_discard         }
  "return"            { keyword Tk_kw_return          }
  "mat2"              { keyword Tk_kw_mat2            }
  "mat3"              { keyword Tk_kw_mat3            }
  "mat4"              { keyword Tk_kw_mat4            }
  "mat2x2"            { keyword Tk_kw_mat2x2          }
  "mat2x3"            { keyword Tk_kw_mat2x3          }
  "mat2x4"            { keyword Tk_kw_mat2x4          }
  "mat3x2"            { keyword Tk_kw_mat3x2          }
  "mat3x3"            { keyword Tk_kw_mat3x3          }
  "mat3x4"            { keyword Tk_kw_mat3x4          }
  "mat4x2"            { keyword Tk_kw_mat4x2          }
  "mat4x3"            { keyword Tk_kw_mat4x3          }
  "mat4x4"            { keyword Tk_kw_mat4x4          }
  "vec2"              { keyword Tk_kw_vec2            }
  "vec3"              { keyword Tk_kw_vec3            }
  "vec4"              { keyword Tk_kw_vec4            }
  "ivec2"             { keyword Tk_kw_ivec2           }
  "ivec3"             { keyword Tk_kw_ivec3           }
  "ivec4"             { keyword Tk_kw_ivec4           }
  "bvec2"             { keyword Tk_kw_bvec2           }
  "bvec3"             { keyword Tk_kw_bvec3           }
  "bvec4"             { keyword Tk_kw_bvec4           }
  "sampler1D"         { keyword Tk_kw_sampler1D       }
  "sampler2D"         { keyword Tk_kw_sampler2D       }
  "sampler3D"         { keyword Tk_kw_sampler3D       }
  "samplerCube"       { keyword Tk_kw_samplerCube     }
  "sampler1DShadow"   { keyword Tk_kw_sampler1DShadow }
  "sampler2DShadow"   { keyword Tk_kw_sampler2DShadow }
  "struct"            { keyword Tk_kw_struct          }

} 


-- punctuators
<0> {
  
  \+              { punctuator Tk_p_plus          }
  \-              { punctuator Tk_p_dash          }
  \!              { punctuator Tk_p_bang          }
  \~              { punctuator Tk_p_tilde         }
  
  \*              { punctuator Tk_p_star          }
  \/              { punctuator Tk_p_divide        }
  "<<"            { punctuator Tk_p_shiftl        }
  ">>"            { punctuator Tk_p_shiftr        }
  \<              { punctuator Tk_p_less          } 
  \>              { punctuator Tk_p_greater       }
  "<="            { punctuator Tk_p_lesseq        }    
  ">="            { punctuator Tk_p_greatereq     }
  "=="            { punctuator Tk_p_equality      }
  "!="            { punctuator Tk_p_notequal      }
  \&              { punctuator Tk_p_ampersand     }
  \^              { punctuator Tk_p_caret         }
  \|              { punctuator Tk_p_bar           }
  "&&"            { punctuator Tk_p_dblampersand  }
  "^^"            { punctuator Tk_p_dblcaret      }
  "||"            { punctuator Tk_p_dblbar        }

  \=              { punctuator Tk_p_eq            }
  "*="            { punctuator Tk_p_stareq        }
  "/="            { punctuator Tk_p_divideeq      }
  "%="            { punctuator Tk_p_percenteq     }
  "+="            { punctuator Tk_p_pluseq        }
  "-="            { punctuator Tk_p_minuseq       }
  "<<="           { punctuator Tk_p_shiftleq      }
  ">>="           { punctuator Tk_p_shiftreq      }
  "&="            { punctuator Tk_p_ampersandeq   }
  "^="            { punctuator Tk_p_careteq       }
  "|="            { punctuator Tk_p_bareq         }
              

  
}

-- integer constants
<0> {
  
  $nonzero_digit $digit*      { intLiteral }
  
  0 $octal_digit*             { octLiteral }
  
  @hex_prefix $hex_digit*     { hexLiteral }

} 


{

type ParseM a = ParseT Identity a

alexEOF :: Monad m => ParseT m Lexeme
alexEOF = (\pos -> L pos Tk_EOF) <$> getPosition



  
data Lexeme = L SrcPosn GlslToken
  deriving (Eq, Show)

intLiteral :: Monad m => AlexInput -> Int -> ParseT m Lexeme
intLiteral = usingInput L (Tk_Integer . read)

-- TODO - watch out for an error on read 
octLiteral :: Monad m => AlexInput -> Int -> ParseT m Lexeme
octLiteral = usingInput L (Tk_Integer . read . traf) where
  traf ('0':s) = '0':'o':s
  traf s       = s    -- this will probably cause a read error which is bad

hexLiteral :: Monad m => AlexInput -> Int -> ParseT m Lexeme
hexLiteral = usingInput L (Tk_Integer . read)



keyword :: Monad m => GlslToken -> AlexInput -> Int -> ParseT m Lexeme
keyword kw = usingInput L (const kw) 

punctuator :: Monad m => GlslToken -> AlexInput -> Int -> ParseT m Lexeme
punctuator pr = usingInput L (const pr)

    
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


}


