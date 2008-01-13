
module Sound.Base.JSON where


import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char (isDigit)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP


instance Applicative ReadP where
  pure = return
  (<*>) = ap

type Dict = M.Map String Value  
  
data Value = String String
           | Number Double
           | Object Dict
           | Array [Value]
           | Boolean Bool
           | Null
           deriving (Eq,Show)

parse = readP_to_S

           
object :: ReadP Value
object = mk <$ skipSpaces <*> body
  where body = between lbrace rbrace (sepBy nvpair comma)
        mk = Object . M.fromList


stringValue = String <$> str

str :: ReadP String
str = between dblquote dblquote (many schar)
  where schar = satisfy (\c -> c /= '"' && c /= '\\')

number :: ReadP Value 
number = mkNumber <$> (option "" minus) 
                  <*> int
                  <*> (option "" frac)
                  <*> (option "" expn) 
                  
  where
    mkNumber s i f e = Number $ read $ concat [s,i,f,e]
    
    minus = string "-"
    
    int = string "0" +++ ((:) <$> digit19 <*> many digit)
    
    frac = (:) <$> decimalPoint <*> many1 digit 
    
    expn = (++) <$> choice (map string ["e+","e-","E+","E-","e","E"])
                <*> many1 digit
        
    decimalPoint = char '.' 
    zero         = string "0"
    digit19      = satisfy (\c -> c >= '1' && c <= '9')
    digit        = satisfy isDigit
    
     
nvpair :: ReadP (String,Value)
nvpair = (,) <$> str <*> (colon *> value )

array :: ReadP Value
array = Array <$> between lbracket rbracket (sepBy value comma)


value :: ReadP Value
value = choice [s,d,o,a,t,f,n]
  where 
    s = stringValue
    d = number
    o = object
    a = array
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








           