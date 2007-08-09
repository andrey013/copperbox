-- A pretty printer that `compacts` empty docs


module Language.C.Pretty.CompactPretty where
{-
module Language.C.Pretty.AEBPrint (
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
-}

import Control.Applicative
import Control.Monad
import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust, isJust)
import System.IO (Handle, hPutStr, stdout)

iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy ae at af = cond <$> ae <*> at <*> af
  where cond e t f = if e then t else f

andA :: Applicative f => f Bool -> f Bool -> f Bool
andA aa ab = (&&) <$> aa <*> ab  

orA :: Applicative f => f Bool -> f Bool -> f Bool
orA aa ab = (||) <$> aa <*> ab

infixl 3 ~>
infixl 2 |>

(~>) :: (Applicative f) => f Bool -> f a -> f (Maybe a)
ae ~> at = cond <$> ae <*> at
  where cond e t = if e then (Just t) else Nothing

(|>) :: (Applicative f) => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
aa |> ab = cond <$> aa <*> ab
  where cond (Just a) _ = Just a
        cond Nothing b = b
  

conditional :: (Applicative f) => f (Maybe a) -> f a
conditional e = ans <$> e
  where ans (Just a) = a
        ans Nothing = error "conditional failure"

truth :: (Applicative f) => f Bool
truth = pure True
  
squeeze :: (Applicative f) => (f a -> Doc) -> (f a -> Doc) -> f a -> Doc
x `squeeze` y = conditional $ 
      (blankA x) ~> y 
   |> (blankA y) ~> x 
   |> truth      ~> x `cat` y


data Doc = EmptyDoc
         | Char Char
         | Text !Int String
         | Cat Doc Doc
         | Union Doc Doc
         | Line Bool
         | Nest !Int Doc 
         | Column Doc
         deriving (Eq,Show)
         

demo = doc1 id
doc1 = group (char 'a' `cat` text "bb") 

beside          :: Applicative f => (f a -> Doc) -> (f a -> Doc) -> f a -> Doc
beside a b      = Cat <$> a <*> b

cat             :: Applicative f => (f a -> Doc) -> (f a -> Doc) -> f a -> Doc
cat             = beside

char            :: Applicative f => Char -> f a -> Doc    
char            = pure . Char

text            :: Applicative f => String -> f a -> Doc
text s          = pure (Text (length s) s)

nulldoc         :: Applicative f => f a -> Doc
nulldoc         = pure EmptyDoc

group           :: Applicative f => (f a -> Doc) -> f a -> Doc
group x         = Union <$> (x >>= flatten) <*> x

line            :: Applicative f => f a -> Doc
line            = pure (Line False)

linebreak       :: Applicative f => f a -> Doc
linebreak       = pure (Line True)

nest            :: Applicative f => Int -> (f a -> Doc) -> f a -> Doc
nest i d        = Nest i <$> d

column          :: Applicative f => (f a -> Doc) -> f a -> Doc
column d        = Column <$> d

-- | Provide blank as a predicate to test for emptiness
blank EmptyDoc = True
blank _        = False

blankA          :: Applicative f => f Doc -> f Bool
blankA          = liftA blank

isJustA         :: Applicative f => f (Maybe Doc) -> f Bool
isJustA         = liftA isJust


dfold           :: Applicative f =>
  ((f a -> Doc) -> (f a -> Doc) -> f a -> Doc) -> [f a -> Doc] -> f a -> Doc
dfold f []      = nulldoc
dfold f ds      = foldr1 f ds


caten           :: Applicative f => [f a -> Doc] -> f a -> Doc
caten           = dfold cat



-- Construct documents from non-empty documents
-- if either side is empty take the opposite
infixr 6 >+<, >~<

(>+<)           :: Applicative f => (f a -> Doc) -> (f a -> Doc) -> f a -> Doc
x >+< y = conditional $ 
     blankA x ~> y 
  |> blankA y ~> x 
  |> truth    ~> x `cat` space `cat` space `cat` y


          

x >~< y = conditional $ 
     blankA x ~> y
  |> blankA y ~> x
  |> truth    ~> x `cat` y 

 

-- Left-biased versions - the left side is ignored if blank
-- but the right is used regardless
infixr 6 >+, >~
x >+ y  = conditional $ 
     blankA x  ~> y
  |> truth     ~> x `cat` space `cat` y 

x >~ y  = conditional $ 
     blankA x  ~> y
  |> truth     ~> x `cat` y 




infixr 5 ^+^, ^^^
-- | Below
x ^+^ y = conditional $ 
     blankA x   ~> y
  |> blankA y   ~> x
  |> truth     ~> x `cat` line `cat` y 

x ^^^ y = conditional $ 
     blankA x  ~> y
  |> truth     ~> x `cat` line `cat` y 



-- suffix iff the left doc is not empty
-- otherwise empty
infixl 3 <~|, <+|

x <~| y = conditional $ 
     blankA x  ~> empty
  |> truth     ~> x `cat` y
            
x <+| y = conditional $ 
     blankA x  ~> empty
  |> truth     ~> x `cat` space `cat` y
                        
-- prefix iff the right doc is not empty
-- otherwise empty
infixr 4 |~>, |+>

x |~> y = conditional $ 
     blankA y  ~> empty
  |> truth     ~> x `cat` y
            
x |+> y = conditional $ 
     blankA y  ~> empty
  |> truth     ~> x `cat` space `cat` y            



catl a b = conditional $
     blankA a  ~> b         
  |> truth     ~> a `cat` b

catr a b = conditional $
     blankA b  ~> a         
  |> truth     ~> a `cat` b




softline        :: Applicative f => f a -> Doc
softline        = group line

infixr 5 |%|
x |%| y         = x >~< softline >~< y


-- | paragraph is fillSep
paragraph       :: Applicative f => [f a -> Doc] -> f a -> Doc
paragraph       = dfold (|%|)






-----------------------------------------------------------
-- Combinators for prelude types (from PPrint)
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"
string          :: Applicative f => String -> f a -> Doc
string ""       = nulldoc
string ('\n':s) = line >~< string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs >~< string ys
                    
                    
bool          :: Applicative f => Bool -> f a -> Doc                                  
bool          = text . show


int           :: Applicative f => Int -> f a -> Doc                   
int           = text . show

integer       :: Applicative f => Integer -> f a -> Doc 
integer       = text . show

float         :: Applicative f => Float -> f a -> Doc 
float         = text . show

double        :: Applicative f => Double -> f a -> Doc 
double        = text . show

rational      :: Applicative f => Rational -> f a -> Doc 
rational      = text . show





--------------------------------------------------------------------------------
-- punctuation
--------------------------------------------------------------------------------
semi, colon, dot, comma, backslash, equals, space, squote, dquote                  
            :: Applicative f => f a -> Doc


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

lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket
            :: Applicative f => f a -> Doc
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
enclose     :: Applicative f => 
                (f a -> Doc) -> (f a -> Doc) -> (f a -> Doc) -> f a -> Doc
enclose l r d = l >~< d >~< r

parens, angles, brackets, braces, squotes, dquotes, dparens     
            :: Applicative f => (f a -> Doc) -> f a -> Doc

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

encloseE     :: Applicative f => 
                ((f a -> Doc) -> f a -> Doc) -> (f a -> Doc) -> f a -> Doc
encloseE f d = conditional $
     blankA d  ~> d
  |> truth     ~> f d



parensE, anglesE, bracketsE, bracesE, squotesE, dquotesE, dparensE     
            :: Applicative f => (f a -> Doc) -> f a -> Doc
                        
parensE     = encloseE parens
anglesE     = encloseE angles
bracketsE   = encloseE brackets
bracesE     = encloseE braces
squotesE    = encloseE squotes
dquotesE    = encloseE dquotes
dparensE    = encloseE dparens


{-
optE :: Applicative f => (f a -> (Maybe Doc)) -> f a -> Doc
optE d = conditional $ 
     isJustA d ~> d
  |> truth     ~> nulldoc
-}  
  


{-
-- | prefix or suffix each member in a list
prefixes, suffixes 
            :: Applicative f => (f a -> f Doc) -> [f a -> f Doc] -> f a -> Doc
prefixes p  = caten . fmap (cat p) 
suffixes p  = caten . fmap ((flip cat) p)   


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



-}

flatten :: Applicative f => Doc -> f a -> Doc
flatten (Cat x y) = Cat <$> (flatten x) <*> (flatten y)
flatten (Union x y)     = flatten x
flatten (Line break)    | break == True   = pure EmptyDoc 
                        | otherwise       = text " "
flatten (Nest i x)      = Nest i <$> (flatten x)
flatten (Column x)      = Column <$> (flatten x)
flatten other           = pure other   
  





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
          EmptyDoc       -> best n k ds 
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

doc2 = stext "hello" (sbreak 1 (stext "world" SEmpty))
 

