

module S1.Parser where

import S1.Syntax

import Control.Applicative hiding ( (<|>), many )
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap

class Par x where
  pp :: Parser x  -- compound
  ppS :: Parser x -- simple

parseLam :: FilePath -> IO (Either String Expr)
parseLam path = do 
  ans <- parseFromFile expr path
  case ans of 
    Left err    -> return $ Left (show err)
    Right a     -> return $ Right a


expr :: Parser Expr
expr = buildExpressionParser table term >>= termk
  where
    termk e = maybe e id <$> optionMaybe (choice [appExpr])
      where                 
        appExpr     = (\e' -> App e e')  <$> term 
        

table :: OperatorTable Char () Expr
table = [ [ ]
        , [ binary  "+"   (binExpr Add)     AssocLeft 
          , binary  "-"   (binExpr Sub)     AssocLeft
          , binary  "*"   (binExpr Mul)     AssocLeft 
          , binary  "/"   (binExpr Div)     AssocLeft
          , binary  "%"   (binExpr Rem)     AssocLeft   
          ]
        ]
  where
    binExpr op e e' = Bin e op e' 

        
binary    :: String   -> (a -> a -> a) -> Assoc -> Operator Char () a
binary    s fun assoc = Infix    (fun <$ reservedOp s) assoc

term :: Parser Expr
term = choice [ simpleExpr, fun, ifThenElse, letExpr, unitOrExpr ]

ifThenElse :: Parser Expr
ifThenElse  = If  <$> 
    (reserved "if" *> expr) <*> (reserved "then" *> expr) 
                            <*> (reserved "else" *> expr)

    
letExpr :: Parser Expr
letExpr = Let <$> 
    (reserved "let" *> identifier) <*> (reservedOp "="  *> expr) 
                                   <*> (reserved   "in" *> expr)
                                   <*   reserved   "end" 
                                 
                      
simpleExpr :: Parser Expr
simpleExpr = choice [constant, var]

unitOrExpr :: Parser Expr
unitOrExpr = maybe Unit id <$> parens (optionMaybe expr)

fun :: Parser Expr
fun = Fn <$> (reservedOp "fn" *> identifier) <*> (reservedOp "=>" *> expr)

var :: Parser Expr
var = Var <$> identifier

constant :: Parser Expr
constant = choice [pBool, pReal]
  where
    pBool = Bool <$> choice [ True  <$ reserved "true", 
                              False <$ reserved "false"] 
    pReal = Real <$> number

number :: Parser Double
number = choice [ try double, int]
  where
    int :: Parser Double
    int = realToFrac <$> integer


--------------------------------------------------------------------------------
-- lexer

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $ emptyDef
  { identStart        = letter
  , identLetter       = choice [letter, digit]
  , caseSensitive     = True
  , opStart           = oneOf "+-*/%="
  , opLetter          = oneOf "=>"  
  , reservedNames     = [ "fn", "if", "then", "else", "let", "in",
                          "true", "false" ]
                        
  , reservedOpNames   = [ "=>", "+", "-", "*", "/", "%" ]
  }

double          :: CharParser () Double
double          = P.float lexer

integer         :: CharParser () Integer
integer         = P.integer lexer

reserved       :: String -> CharParser () ()
reserved        = P.reserved lexer

reservedOp      :: String -> CharParser () ()
reservedOp      = P.reservedOp lexer

identifier      :: CharParser () String
identifier      = P.identifier lexer

lexeme          :: CharParser () a -> CharParser () a
lexeme          = P.lexeme lexer

parens          :: CharParser () a -> CharParser () a
parens          = P.parens lexer

lparen          :: CharParser () Char
lparen          = lexeme $ char '('

rparen          :: CharParser () Char
rparen          = lexeme $ char ')'

