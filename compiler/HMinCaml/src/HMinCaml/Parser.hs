{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module: HMinCaml.Parser
-- License: as per original MinCaml
--
-- Maintainer: Stephen Tetley <stephen.tetley@gmail.com>
-- Stability: unstable
-- Portability: ghc
--
-- Parser
--

module HMinCaml.Parser where

import HMinCaml.Id
import HMinCaml.Lexer
import HMinCaml.Syntax
import HMinCaml.Type

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
parseMinCaml :: FilePath -> IO (Either String Expr)
parseMinCaml path = do 
  ans <- parseFromFile expr path
  case ans of 
    Left err    -> return $ Left (show err)
    Right a     -> return $ Right a
    
    
  
addtyp :: Id -> (Id,Type)
addtyp x = (x, gentyp)

simpleExpr :: Parser Expr
simpleExpr = arrExpr =<< choice [ parene, boole, inte, floate, 
                                  ident, unitOrExpr]
  where
    parene      = parens expr
    boole       = Bool  <$> choice [ True   <$ reserved "true", 
                                     False  <$ reserved "false"]
    inte        = Int   <$> int
    floate      = Float <$> float
    ident       = Var   <$> identifier
    unitOrExpr  = maybe Unit id <$> parens (optionMaybe expr)

    arrExpr e   = maybe (return e) (arrSk e) =<< optionMaybe (dot *> parens expr)
    arrSk e1 e2 = maybe (Get e1 e2) (\e3 -> Put e1 e2 e3) <$> 
                      optionMaybe (reservedOp "<-" *> expr)
    
    
expr :: Parser Expr
expr = buildExpressionParser table term >>= termk
  where
    termk e = maybe e id <$> optionMaybe (choice [appExpr, semiExpr])
      where                 
        appExpr     = (\arglist -> App e arglist)  <$> actualArgs 
        semiExpr    = (\e2 -> Let ("",TUnit) e e2) <$> (reservedOp ";"  *> expr)
        
term :: Parser Expr
term = choice [ simpleExpr, notExpr,  ifThenElse
                , letExpr, tupleExpr, arrayCreate ]
  where
    notExpr     = Not <$> (reserved "not" *> expr)
    
    ifThenElse  = If  <$> (reserved "if"   *> expr) 
                      <*> (reserved "then" *> expr)
                      <*> (reserved "else" *> expr)
    


    tupleExpr   = Tuple <$> elems
    
    letExpr     = do reserved "let" 
                     a <- optionMaybe (reserved "rec")
                     case a of
                        Just _  -> letrec
                        Nothing -> do b <- optionMaybe (parens pat)
                                      maybe dolet lettuple b
      where
        dolet         = (\s e1 e2 -> Let (addtyp s) e1 e2) <$>
                            identifier <*> (reservedOp "="  *> expr) 
                                       <*> (reserved   "in" *> expr)
        letrec        = LetRec <$> fundef <*> (reserved "in" *> expr)
        
        lettuple pt   = (\e1 e2 -> LetTuple pt e1 e2) <$>
                            (reservedOp "=" *> expr) <*> (reserved "in" *> expr)
    
    arrayCreate = Array <$> 
                      (reserved "Array.create" *> simpleExpr) <*> simpleExpr                             
                                    
        
table :: OperatorTable Char () Expr
table = [ [ prefixf   (reserved "not")      exprNegate 
          , prefixOp  "-."                  FNeg
          ]
        , [ binary  "+"   Add                     AssocLeft 
          , binary  "-"   Sub                     AssocLeft
          , binary  "="   Eq                      AssocLeft 
          , binary  "<>"  ((Not .) . Eq)          AssocLeft
          , binary  "<"   ((Not .) . (flip LE))   AssocLeft
          , binary  ">"   ((Not .) . LE)          AssocLeft
          , binary  "<="  LE                      AssocLeft
          , binary  ">="  (flip LE)               AssocLeft
          , binary  "*."  FMul                    AssocLeft
          , binary  "/."  FDiv                    AssocLeft
          , binary  "+."  FAdd                    AssocLeft
          , binary  "+-"  Sub                     AssocLeft
   
          ]
        ]
        
        
exprNegate :: Expr -> Expr 
exprNegate (Float d)  = Float $ negate d
exprNegate e          = Neg e

binary    :: String   -> (a -> a -> a) -> Assoc -> Operator Char () a
prefixf   :: Parser b -> (a -> a)      -> Operator Char () a
prefixOp  :: String   -> (a -> a)      -> Operator Char () a
postfix   :: String   -> (a -> a)      -> Operator Char () a

binary    s fun assoc = Infix    (fun <$ reservedOp s) assoc
prefixf   p fun       = Prefix   (fun <$ p)
prefixOp  s fun       = Prefix   (fun <$ reservedOp s)
postfix   s fun       = Postfix  (fun <$ reservedOp s)


    
fundef :: Parser Fundef
fundef = Fundef <$> 
    (addtyp <$> identifier) <*> formalArgs <*> (reservedOp "=" *> expr)

formalArgs :: Parser [(Id, Type)]
formalArgs = many1 (addtyp <$> identifier)

actualArgs :: Parser [Expr]
actualArgs = many1 simpleExpr  
    
elems :: Parser [Expr]
elems = commaSep expr

pat :: Parser [(Id,Type)]
pat = commaSep (addtyp <$> identifier)


    