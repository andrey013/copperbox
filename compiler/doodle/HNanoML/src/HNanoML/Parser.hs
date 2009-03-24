{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module      : HMinCaml.Parser
-- License     : BSD-style (see the LICENSE file in the distribution)
-- Copyright   : 2009 Stephen Tetley
--
-- Maintainer  : Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   : unstable
-- Portability : ghc & uuag
--
-- Parser
--

module HNanoML.Parser where

import HNanoML.Syntax
import HNanoML.Type

import Control.Applicative 
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap

type FreshParser a = GenParser Char Int a

freshVar :: FreshParser Type
freshVar = do 
    i <- getState
    updateState (+1) 
    return $ TyVar i
  
parseNanoML :: FilePath -> IO (Either String Expr)
parseNanoML path = do 
  text <- readFile path
  return $ parseWithState path text

parseNanoML' :: String -> Either String Expr
parseNanoML' text =  parseWithState "" text
    
parseWithState :: FilePath -> String -> Either String Expr
parseWithState filepath text = either (Left . show) (Right) $ 
    runParser expr 0 filepath text

        

expr :: FreshParser Expr
expr = buildExpressionParser table term >>= termk
  where
    termk e = maybe e id <$> optionMaybe (choice [appExpr, semiExpr])
      where                 
        appExpr     = (\arglist -> App e arglist)  <$> actualArgs 
        semiExpr    = (\e2 fresh -> Let (Decl "" fresh e) e2) <$> 
                          (reservedOp ";"  *> expr) <*> freshVar
        
        
simpleExpr :: FreshParser Expr
simpleExpr = choice [ parenExp, boolExp, intExp, ident ]
  where
    parenExp    = parens expr
    boolExp     = CBool  <$> choice [ True   <$ reserved "true", 
                                      False  <$ reserved "false"]
    intExp      = CInt   <$> int
    ident       = Var   <$> identifier


    
    

term :: FreshParser Expr
term = choice [ simpleExpr, ifThenElse , letExpr ]
  where
    ifThenElse  = If  <$> (reserved "if"   *> expr) 
                      <*> (reserved "then" *> expr)
                      <*> (reserved "else" *> expr)
    

    letExpr     = do reserved "let" 
                     a <- optionMaybe (reserved "rec")
                     case a of
                        Just _  -> letrec
                        Nothing -> dolet
      where
        dolet         = (\decl e -> Let decl e) <$>
                            letdecl <*> (reserved   "in" *> expr)
        letrec        = Letrec <$> fundef <*> (reserved "in" *> expr)
        
letdecl :: FreshParser Decl
letdecl = Decl <$> identifier <*> freshVar <*> (reservedOp "="  *> expr)                          
                                    
        
table :: OperatorTable Char st Expr
table = [ [ ]
        , [ binary  "+"   Plus                    AssocLeft 
          , binary  "-"   Minus                   AssocLeft
          , binary  "="   Eq                      AssocLeft 
          , binary  "<"   Less                    AssocLeft 
          ]
        ]
        
        

binary    :: String   -> (a -> a -> a) -> Assoc -> Operator Char st a
binary    s fun assoc = Infix    (fun <$ reservedOp s) assoc


    
fundef :: FreshParser Fundef
fundef = do 
    name  <- identifier
    fty   <- freshVar
    args  <- many1 identifier    
    atys  <- replicateM (length args) freshVar
    e     <- reservedOp "=" *> expr
    return $ Fundef name fty args atys e


actualArgs :: FreshParser [Expr]
actualArgs = many1 simpleExpr  


--------------------------------------------------------------------------------
-- Lexer

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef
  { identStart        = choice [letter, char '_' ]
  , identLetter       = choice [alphaNum, oneOf "_'" ]
  , caseSensitive     = True
  , commentStart      = "(*"
  , commentEnd        = "*)"
  , opStart           = opLetter emptyDef
  , opLetter          = oneOf "+-=<;"  
  , reservedNames     = [ "if", "then", "else"
                        , "let", "in", "rec"
                        , "true", "false"
                        ]
                        
  , reservedOpNames   = [ "+", "-", "=", "<"
                        , ";"
                        ]
  }

identifier      :: CharParser st String
identifier      = P.identifier lexer

reservedOp      :: String -> CharParser st ()
reservedOp      = P.reservedOp lexer

commaSep        :: CharParser st a -> CharParser st [a]
commaSep        = P.commaSep lexer

int             :: CharParser st Int
int             = P.integer lexer >>= return . fromIntegral

float           :: CharParser st Float
float           = P.float lexer >>= return . realToFrac

reserved       :: String -> CharParser st ()
reserved        = P.reserved lexer

symbol          :: String -> CharParser st String
symbol          = P.symbol lexer

parens          :: CharParser st a -> CharParser st a
parens          = P.parens lexer




    