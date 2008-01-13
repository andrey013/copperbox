
--------------------------------------------------------------------------------
-- |
-- Module      :  Format.JSON.JSON
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Serialize as JSON 
-- |
--------------------------------------------------------------------------------


module Format.JSON.JSON ( 
  -- * Datatpye of JSON values
  Value(..),
  
  parseValue,
  
  printValue
  
  ) where


import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char (isDigit)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.HughesPJ as P


instance Applicative ReadP where
  pure = return
  (<*>) = ap




--------------------------------------------------------------------------------
-- Datatype
--------------------------------------------------------------------------------

type Dict = M.Map String Value  
  
data Value = String String
           | Number Double
           | Object Dict
           | Array [Value]
           | Boolean Bool
           | Null
           deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------



parseValue :: String -> Value
parseValue str = 
  let ans = readP_to_S readObject str in
  case ans of 
    [(a,"")] -> a
    _ -> error "parse failure"
           

           
readObject :: ReadP Value
readObject = mk <$ skipSpaces <*> body
  where body = between lbrace rbrace (sepBy nvpair comma)
        mk = Object . M.fromList
        
        nvpair :: ReadP (String,Value)
        nvpair = (,) <$> readString <*> (colon *> readValue )


readStringValue = String <$> readString

readString :: ReadP String
readString = between dblquote dblquote (many schar)
  where schar = satisfy (\c -> c /= '"' && c /= '\\')

readNumberValue :: ReadP Value 
readNumberValue = Number <$> readDouble

readDouble :: ReadP Double
readDouble = mkNumber <$> (option "" minus) 
                      <*> int
                      <*> (option "" frac)
                      <*> (option "" expn) 
                  
  where
    mkNumber s i f e = read $ concat [s,i,f,e]
    
    minus = string "-"
    
    int = string "0" +++ ((:) <$> digit19 <*> many digit)
    
    frac = (:) <$> decimalPoint <*> many1 digit 
    
    expn = (++) <$> choice (map string ["e+","e-","E+","E-","e","E"])
                <*> many1 digit
        
    decimalPoint = char '.' 
    zero         = string "0"
    digit19      = satisfy (\c -> c >= '1' && c <= '9')
    digit        = satisfy isDigit
    
     


readArrayValue :: ReadP Value
readArrayValue = Array <$> between lbracket rbracket (sepBy readValue comma)


readValue :: ReadP Value
readValue = choice [s,d,o,a,t,f,n]
  where 
    s = readStringValue
    d = readNumberValue
    o = readObject
    a = readArrayValue
    t = Boolean True  <$ true  
    f = Boolean False <$ false
    n = Null          <$ nulltok

-- Tokens 
token p = p <* skipSpaces



dblquote  = token (char '"')
lbrace    = token (char '{')
rbrace    = token (char '}')
lbracket  = token (char '[')
rbracket  = token (char ']')
colon     = token (char ':')
comma     = token (char ',')

true      = token (string "true")
false     = token (string "false")
nulltok   = token (string "null")


--------------------------------------------------------------------------------
-- Pretty print
--------------------------------------------------------------------------------
printValue :: Value -> String 
printValue v = P.renderStyle sty (valuePP v)
  where sty = P.Style P.PageMode 80 0.8

valuePP :: Value -> P.Doc

valuePP (String s)       = P.doubleQuotes $ P.text s
valuePP (Number d)       = P.double d 
valuePP (Object dict)    = P.braces $ P.sep $ P.punctuate P.comma elems
  where elems = M.foldWithKey fn [] dict
        fn k a acc = let d = (P.doubleQuotes $ P.text k) P.<+> P.colon P.<+> valuePP a
                     in d : acc
valuePP (Array vs)       = P.brackets $ P.sep $ P.punctuate P.comma (map valuePP vs)

valuePP (Boolean True)   = P.text "true"
valuePP (Boolean False)  = P.text "false"
valuePP Null             = P.text "null"


           