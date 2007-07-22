-- Empty Biased (EB) Pretty printer



module Language.C.Pretty.EBPretty (
  Doc, 
  cat, char, text, empty, group, line,
  linebreak, nest, column,
  
  blank, dfold, caten,

  (>+<), (>~<), (>+), (>~),  (^+^), (^^^),
  (<~|), (<+|), (|~>), (|+>),
  
  paragraph,
  
  string, bool, int, integer, float, double, rational,
  
  semi, colon, dot, comma, backslash, equals, space, squote, dquote,
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,

  enclose, parens, angles, brackets, braces, squotes, dquotes, dparens,

  encloseE, parensE, anglesE, bracketsE, bracesE, squotesE, dquotesE, dparensE,

  optE, 

  prefixes, suffixes,
  
  sepSep, spaceSep, commaSep,
  encloseSep, encloseSepE,
  
  linefeed, linesep,
  
  renderPretty, displayIO, putDoc, hPutDoc

  ) where


import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import System.IO (Handle, hPutStr, stdout)


data Doc = Empty
         | Char Char
         | Text !Int String
         | Cat Doc Doc
         | Union Doc Doc
         | Line Bool
         | Nest !Int Doc 
         | Column Doc
         deriving (Eq,Show)
         






cat             = Cat    
char            = Char
text s          = Text (length s) s
empty           = Empty
group x         = Union (flatten x) x

line            = Line False
linebreak       = Line True
nest            = Nest
column          = Column

-- | Provide blank as a predicate to test for emptiness
blank :: Doc -> Bool
blank Empty = True
blank _     = False

dfold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
dfold f []      = empty
dfold f ds      = foldr1 f ds

caten :: [Doc] -> Doc
caten           = dfold cat



-- Construct documents from non-empty documents
-- if either side is empty take the opposite
infixr 6 >+<, >~<

x >+< y   | blank x   = y
          | blank y   = x
          | otherwise = x `cat` space `cat` y 
           

x >~< y   | blank x   = y
          | blank y   = x
          | otherwise = x `cat` y 

-- Left-biased versions - the left side is ignored if blank
-- but the right is used regardless
infixr 6 >+, >~
x >+ y    | blank x   = y
          | otherwise = x `cat` space `cat` y 

x >~ y    | blank x   = y
          | otherwise = x `cat` y 


infixr 5 ^+^, ^^^
-- | Below
x ^+^ y     | blank x   = y
            | blank y   = x
            | otherwise = x `cat` line `cat` y 

x ^^^ y     | blank x   = y
            | otherwise = x `cat` line `cat` y 


-- suffix iff the left doc is not empty
-- otherwise empty
infixl 3 <~|, <+|

x <~| y   | blank x   = empty
          | otherwise = x `cat` y
            
x <+| y   | blank x   = empty
          | otherwise = x `cat` space `cat` y
                        
-- prefix iff the right doc is not empty
-- otherwise empty
infixr 4 |~>, |+>

x |~> y   | blank y   = empty
          | otherwise = x `cat` y
            
x |+> y   | blank y   = empty
          | otherwise = x `cat` space `cat` y            



catl Empty b     = b         
catl a     b     = Cat a b

catr a Empty     = a         
catr a b         = Cat a b





softline        = group line

infixr 5 |%|
x |%| y         = x >~< softline >~< y


-- | paragraph is fillSep
paragraph       = dfold (|%|)






-----------------------------------------------------------
-- Combinators for prelude types (from PPrint)
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"
string ""       = empty
string ('\n':s) = line >~< string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs >~< string ys
                    
                    
                                      
bool :: Bool -> Doc
bool          = text . show

int :: Int -> Doc                  
int           = text . show

integer :: Integer -> Doc
integer       = text . show

float :: Float -> Doc
float         = text . show

double :: Double -> Doc
double        = text . show

rational :: Rational -> Doc
rational      = text . show





-----------------------------------------------------------
-- punctuation
-----------------------------------------------------------
semi        = char ';'
colon       = char ':'
dot         = char '.'
comma       = char ','
backslash   = char '\\'
equals      = char '='
space       = char ' '
squote      = char '\''
dquote      = char '"'

-----------------------------------------------------------
-- brackets, braces, etc
-----------------------------------------------------------
lparen      = char '('
rparen      = char ')'
langle      = char '<'
rangle      = char '>'
lbrace      = char '{'
rbrace      = char '}'
lbracket    = char '['
rbracket    = char ']'



-----------------------------------------------------------
-- bracketing, quoting, etc (largely from PPrint)
-----------------------------------------------------------
enclose l r d = l >~< d >~< r

parens      = enclose lparen rparen
angles      = enclose langle rangle
brackets    = enclose lbracket rbracket
braces      = enclose lbrace rbrace
squotes     = enclose squote squote
dquotes     = enclose dquote dquote     

-- | dparens - double parens
dparens     = enclose (text "((") (text "))")

--------------------------------------------------------------------------------
-- empty variants for bracketing, quoting, etc 
--------------------------------------------------------------------------------
encloseE f d      | blank d     = d
                  | otherwise   = f d

parensE     = encloseE parens
anglesE     = encloseE angles
bracketsE   = encloseE brackets
bracesE     = encloseE braces
squotesE    = encloseE squotes
dquotesE    = encloseE dquotes
dparensE    = encloseE dparens



optE :: Maybe Doc -> Doc
optE = fromMaybe empty


-- | prefix or suffix each member in a list
prefixes, suffixes :: Doc -> [Doc] -> Doc
prefixes p = caten . map (cat p) 
suffixes p = caten . map ((flip cat) p)   


sepSep sep = caten . (intersperse sep)

spaceSep = sepSep space
commaSep = sepSep comma







encloseSep left right sep ds
    = case ds of
        []  -> left >~< right
        [d] -> left >~< d >~< right
        _   -> let fn = column . caten . (intersperse sep) 
               in left >~< (fn ds) >~< right 
                    

encloseSepE l r s [] = empty
encloseSepE l r s ds = encloseSep l r s ds 
        



-- | add n line feeds
linefeed :: Int -> Doc
linefeed n = foldr cat empty (replicate n line)

-- | separate with n blank lines
linesep :: Int -> [Doc] -> Doc
linesep n ds = caten $ intersperse (linefeed (n+1)) ds




  
  
flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Union x y)     = flatten x
flatten (Line break)    | break == True   = Empty 
                        | otherwise       = text " "
flatten (Nest i x)      = Nest i (flatten x)
flatten (Column x)      = Column (flatten x)
flatten other           = other   


data Docs   = Nil
            | Cons !Int Doc Docs
            deriving (Show)
            
renderPretty rfrac pwidth x = best 0 0 (Cons 0 x Nil)                
  where

    -- best :: n = indentation of current line
    --         k = current column  
    --        (ie. (k >= n) && (k - n == count of inserted characters)
    best n k Nil      = SEmpty
    best n k (Cons i d ds)  
      = case d of
          Empty       -> best n k ds 
          Char c      -> let k' = k+1 in seq k' (SText 1 [c] (best n k' ds))
          Text l s    -> let k' = k+l in seq k' (SText l s (best n k' ds)) 
          Line _      -> SBreak i (best i i ds) 
          Cat x y     -> best n k (Cons i x (Cons i y ds))
          Union x y   -> nicest rfrac pwidth n k (best n k (Cons i x ds))                
                                                 (best n k (Cons i y ds))
          Nest j x    -> best n k (Cons (i+j) x ds)                                                                           
          Column x    -> best n k (Cons (i+k) x ds)                           
                                    
--nicest :: rfrac = ribbon width, pwidth = page width, 
--          n = indentation of current line, k = current column
--          x and y, the (simple) documents to chose from.
--          precondition: first lines of x are longer than the first lines of y.                                      
nicest rfrac pwidth n k x y 
  | fits w x      = x
  | otherwise     = y
  where r  = max 0 (min pwidth (round (fromIntegral pwidth * rfrac)))
        w = min (pwidth - k) (r - k + n)
                          
                                                                            





data SDoc = SEmpty
          | SChar Char SDoc 
          | SText !Int String SDoc
          | SBreak !Int SDoc

          
stext s = SText (length s) s
sbreak = SBreak

showSimple :: SDoc -> Int ->  ShowS
showSimple e margin  = snd $ printing e margin 0 (margin,id)
  where     
    newl n (i,k) = let s = '\n' : replicate n ' '
                    in (i, k . showString s)                     
  
    printing :: SDoc -> Int -> Int -> (Int, ShowS) -> (Int, ShowS) 
    printing SEmpty _ _  (i,k) = (i,k)
      
    printing (SText l s x) bsp aft (i,k) = 
      printing x bsp aft (i - l, k . showString s)
          
    printing (SBreak l x) bsp aft (i,k) =
      let cont = newl l (i,k)        
      in printing x bsp aft cont 



instance Show SDoc where
  showsPrec d doc       = showSimple doc 80
  

-----------------------------------------------------------
-- IO interface as per PPrint ...
-----------------------------------------------------------

  
displayIO :: Handle -> SDoc -> IO ()
displayIO h sdoc = let sfn = showSimple sdoc 80 in hPutStr h (sfn [])

putDoc :: Doc -> IO ()
putDoc doc              = hPutDoc stdout doc

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc handle doc      = displayIO handle (renderPretty 0.8 80 doc)






  
{-

breakdist (SEmpty)      a = a 
breakdist (SText l s x) a = l + breakdist x a
breakdist (SBreak _ _)  a = 0
                        

          
size (SBlock l _ _) = l
size (SText l s)    = l
size (SBreak l)     = l


-}

fits w x              | w < 0       = False
fits w (SChar l x)                  = fits (w-1) x
fits w (SText l _ x)  | l <= w      = fits (w-l) x
                      | otherwise   = False
fits w (SBreak _ _)                 = True
fits w (SEmpty)                     = True



test doc = showSimple (renderPretty 0.8 30 doc) 30 [] 
 
testS doc = showSimple doc 15 []

doc1 = stext "hello" (sbreak 1 (stext "world" SEmpty))
 

