{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Parser
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser.
--
--------------------------------------------------------------------------------

module HMinCaml.Parser 
  (
   
    parseMinCaml
  , parseMinCamlString

  ) where

import HMinCaml.Id
import HMinCaml.Lexer
import HMinCaml.Syntax
import qualified HMinCaml.Type as T

import Control.Applicative
import Text.Parsec
import Text.Parsec.Expr

import Data.Functor.Identity

-- Need a Parser with Int state...

  
parseMinCaml :: FilePath -> IO (Either String Expr)
parseMinCaml path = parseMinCamlString <$> readFile path 
    


parseMinCamlString :: String -> Either String Expr
parseMinCamlString ss = 
  case runParser expr 1 "" ss of
    Left err    -> Left (show err)
    Right a     -> Right a
    
  
nextloc :: MLParser T.Type
nextloc = getState >>= \i -> putState (i+1) >> return (T.TypeLoc i)


expr :: MLParser Expr
expr = buildExpressionParser table term >>= termk
  where
    termk e = maybe e id <$> optionMaybe (choice [appExpr, semiExpr])
      where                 
        appExpr     = (\arglist -> App e arglist)  <$> actualArgs 
        semiExpr    = (\e2 -> Let ("",T.Unit) e e2) <$> (reservedOp ";"  *> expr)
        
        
simpleExpr :: MLParser Expr
simpleExpr = arrExpr =<< choice [ parene, boole, inte, floate, 
                                  ident, unitOrExpr]
  where
    parene      = parens expr
    boole       = Bool  <$> choice [ True   <$ reserved "true", 
                                     False  <$ reserved "false"]
    inte        = Int   <$> int
    floate      = Float <$> double
    ident       = Var   <$> identifier
    unitOrExpr  = maybe Unit id <$> parens (optionMaybe expr)

    arrExpr e   = maybe (return e) (arrSk e) =<< optionMaybe (dot *> parens expr)
    arrSk e1 e2 = maybe (Get e1 e2) (\e3 -> Put e1 e2 e3) <$> 
                      optionMaybe (reservedOp "<-" *> expr)
    
    

term :: MLParser Expr
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
        dolet         = (\tid e1 e2 -> Let tid e1 e2) <$>
                            typedId <*> (reservedOp "="  *> expr) 
                                    <*> (reserved   "in" *> expr)
        letrec        = LetRec <$> fundef <*> (reserved "in" *> expr)
        
        lettuple pt   = (\e1 e2 -> LetTuple pt e1 e2) <$>
                            (reservedOp "=" *> expr) <*> (reserved "in" *> expr)
    
    arrayCreate = Array <$> 
                      (reserved "Array.create" *> simpleExpr) <*> simpleExpr

        
table :: OperatorTable String Int Identity Expr
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

binary    :: String     -> (a -> a -> a) -> Assoc -> Operator String Int Identity a
prefixf   :: MLParser b -> (a -> a)               -> Operator String Int Identity a
prefixOp  :: String     -> (a -> a)               -> Operator String Int Identity a
-- postfix   :: String   -> (a -> a)              -> Operator String Int Identity a

binary    s fun assoc = Infix    (fun <$ reservedOp s) assoc
prefixf   p fun       = Prefix   (fun <$ p)
prefixOp  s fun       = Prefix   (fun <$ reservedOp s)
-- postfix   s fun       = Postfix  (fun <$ reservedOp s)


    
fundef :: MLParser Fundef
fundef = Fundef <$> typedId <*> formalArgs <*> (reservedOp "=" *> expr)

formalArgs :: MLParser [(IdS, T.Type)]
formalArgs = many1 typedId

actualArgs :: MLParser [Expr]
actualArgs = many1 simpleExpr  
    
elems :: MLParser [Expr]
elems = commaSep expr

pat :: MLParser [(IdS,T.Type)]
pat = commaSep typedId


typedId :: MLParser (IdS, T.Type)
typedId = (,) <$> identifier <*> nextloc