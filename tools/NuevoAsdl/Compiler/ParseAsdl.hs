

module Compiler.ParseAsdl 
  ( parseAsdl
  ) where


import Compiler.Util

import qualified Base.AsdlConcreteSyn as CS

import Control.Monad.Trans
import Data.Char
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

 
 

parseAsdl :: (MonadIO m) => FilePath -> m (ResultE [CS.AltThree])
parseAsdl file = do
  ans <- liftIO $ parseFromFile parseAsdlSpec file
  case ans of
    Left err -> return (Left (show err))
    Right def -> return (Right def)

parseAsdlSpec :: Parser [CS.AltThree]
parseAsdlSpec = whiteSpace >> many alt
  where
    alt :: Parser CS.AltThree
    alt =     do { a <- parsePrimModule ; return (CS.Alt2 a) }
          <|> do { b <- parseModule ; return (CS.Alt1 b) }
          <|> do { c <- parseView ; return (CS.Alt3 c) }



-- this should be alright for zsuif.asdl
-- the 'module StdTypes <= suppress : false' etc lines 
-- are part of the view decls
parseModule :: Parser CS.ModuleDefn
parseModule = do
  reserved "module"
  modname <- identifier
  imports <- option [] (parens (many parseImport))
  typedecls <- braces (many parseTypeDecl)
  return (CS.Module modname imports typedecls)
  <?> "Module"

parsePrimModule :: Parser CS.PrimModule
parsePrimModule = do
  reserved "primitive"
  reserved "module"
  modname <- identifier
  prims <- braces (many identifier)
  return (CS.PrimModule modname prims)
  <?> "PrimitiveModule"
    
parseView :: Parser CS.ViewDefn
parseView = do
  reserved "view"
  langname <- identifier
  elts <- braces (many viewDecl)
  return (CS.View langname elts)
  <?> "View"


parseImport :: Parser CS.ImportStmt
parseImport = do
  reserved "imports" 
  modname <- identifier
  return (CS.ImportStmt modname)
  



parseTypeDecl :: Parser CS.Definition
parseTypeDecl = do
  lhs <- identifier
  reserved "="
  rhs <- parseProductType  <|> parseSumType
  return (CS.Def lhs rhs)
  <?> "TypeDecl"




parseSumType :: Parser CS.AsdlType
parseSumType = do
  (constrs,oattrs) <- sumDef -- need to acount for attribs
  return (CS.Sum constrs oattrs)
  <?> "SumType"
  where
    sumDef = do{ fs <- sepBy parseConstructor (reserved "|") 
               ; as <- optParser parseAttributes
               ; return (fs,as)
               }  
    


parseProductType :: Parser CS.AsdlType
parseProductType = do
  fields <- parens (commaSep parseField)
  return (CS.Prod fields)
  <?> "ProductType"


parseConstructor :: Parser CS.Constr
parseConstructor = do
  name <- identifier
  fields <- option [] (parens (commaSep parseField))
  return (CS.Constr name fields)
  <?> "Constructor"
  

  
parseField :: Parser CS.Field
parseField = do
  (opt_qual,typ) <- parseFieldType
  card <- parseCardinality
  label <- optParser identifier
  return (CS.Field opt_qual typ card label)
  <?> "Field"

parseAttributes :: Parser [CS.Field]
parseAttributes = do
  reserved "attributes"
  fields <- parens (commaSep parseField)
  return fields
  <?> "Fields"

parseFieldType :: Parser (CS.OptQualifier, CS.AsdlPrim)
parseFieldType = 
      do{ p <- parsePrimitive; return (Nothing,p)}
  <|> parseQualType

parsePrimitive :: Parser CS.AsdlPrim
parsePrimitive =
      do {reserved "string";      return CS.TyString }
  <|> do {reserved "int";         return CS.TyInt }
  <|> do {reserved "identifier";  return CS.TyIdentifier }



parseQualType :: Parser (CS.OptQualifier, CS.AsdlPrim)
parseQualType = do
  id1 <- identifier 
  dot <- option False (do { dot ; return True })
  case dot of 
    False -> return (Nothing, CS.TyRef id1)
    True -> do { id2 <- identifier
               ; return (Just id1, CS.TyRef id2) }

   <?> "Qualified Type"


    
parseCardinality :: Parser CS.Cardinality
parseCardinality =
      do { reserved "?"; return CS.Opt }
  <|> do { reserved "*"; return CS.Zom }
  <|> return CS.One

  <?> "Tycon"


viewDecl :: Parser CS.ViewDecl
viewDecl = do
  e_ent <- singleViewEntity `eitherParser` multipleViewEntity
  reserved "<="
  e_kv <- singlePropValue `eitherParser` multiplePropValue
  return (buildView e_ent e_kv)
  
buildView :: Either CS.ViewEntity [CS.ViewEntity] -> Either CS.ViewPair [CS.ViewPair]
              ->  CS.ViewDecl
buildView (Left x)    (Left y)    = CS.View_Plain x y 
buildView (Right xs)  (Left y)    = CS.View_Many_to_One xs y 
buildView (Left x)    (Right ys)  = CS.View_One_to_Many x ys              
buildView (Right xs)  (Right ys)  = CS.View_Many_to_Many xs ys    

singleViewEntity :: Parser CS.ViewEntity
singleViewEntity = moduleViewEntity <|> qualifiedViewEntity

multipleViewEntity :: Parser [CS.ViewEntity]
multipleViewEntity = braces (many1 qualifiedViewEntity)


moduleViewEntity :: Parser CS.ViewEntity
moduleViewEntity = do
  reserved "module" 
  name <- identifier 
  return (CS.ModuleView name)
  

-- Parses either a constructor or a type viewEntity
-- depending on the case of the identifier
qualifiedViewEntity :: Parser CS.ViewEntity
qualifiedViewEntity = do
  mqual <- identifier
  dot
  name <- identifier
  return $ mkEntity name mqual
  
  where mkEntity name@(a:_) qual | isUpper a    = CS.ConstrView name qual
        mkEntity name       qual                = CS.TypeView name qual
        
  
singlePropValue :: Parser CS.ViewPair
singlePropValue = do
  prop <- identifier
  text <- viewText
  return (prop,text)

multiplePropValue :: Parser [CS.ViewPair]
multiplePropValue = braces (many1 singlePropValue)
    
  
    
viewText :: Parser String
viewText = singleLineText <|> multiLineText

singleLineText :: Parser String
singleLineText = do
  colon
  text <- untilNewline
  whiteSpace          -- consume the next lines trailing whitespace
  return text

untilNewline :: Parser String
untilNewline = manyTill anyChar (try $ char '\n')

multiLineText :: Parser String
multiLineText = do
  reserved "%%"
  xs <- manyTill anyChar (try (reserved "%%"))
  return xs

optParser :: Parser a -> Parser (Maybe a)
optParser p = option Nothing (do { ans <- p ; return (Just ans) })
 
eitherParser :: Parser a -> Parser b -> Parser (Either a b)
eitherParser p p' =     do { a <- p ; return (Left a) }
                    <|> do { b <- p' ; return (Right b) }
    

  
justParser :: Parser a -> Parser (Maybe a)
justParser p = do { ans <- p ; return (Just ans) }




-------------------------------------------------------------------------------
--Tokens
-------------------------------------------------------------------------------

asdlLex             = P.makeTokenParser asdlDef


asdlDef = emptyDef
  { commentLine     = "--"
  , commentStart    = "--*"
  , commentEnd      = "--*"
  , reservedNames   = [ "module", "view", "primitive", "attributes", "imports"
                      , "string", "int", "identifier"  ]
  , reservedOpNames = ["=", "|", "?", "!", "*", "<=", ":", "%%"]                                  
  , identStart      = letter 
  , identLetter     = alphaNum <|> oneOf "_"
  , caseSensitive   = True                                    
  } 

whiteSpace        = P.whiteSpace asdlLex  
reserved          = P.reserved asdlLex 
symbol            = P.symbol asdlLex     
identifier        = P.identifier asdlLex    
parens            = P.parens asdlLex 
braces            = P.braces asdlLex 
squares           = P.squares asdlLex    
stringLiteral     = P.stringLiteral asdlLex  
integer           = P.integer asdlLex 
commaSep          = P.commaSep asdlLex
dot               = P.dot asdlLex
colon             = P.colon asdlLex
