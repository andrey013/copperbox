{-# OPTIONS_GHC -fglasgow-exts #-}

-- A pretty printer that `compacts` empty docs


module Language.C.Pretty.CompactPretty (
  Doc(..), 
  cat, char, text, nulldoc, group, line,
  linebreak, nest, column,
  
  blankA, dfold, catenate,

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
  
  renderPretty, displayIO, putDoc, hPutDoc,
  
  Pretty(..), docs

  ) where


import Control.Applicative
import Control.Monad
import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust, isJust)
import System.IO (Handle, hPutStr, stdout)


sequenceA :: Applicative f => [f a] -> f [a]
sequenceA []     = pure []
sequenceA (c:cs) = (:) <$> c <*> sequenceA cs





data Doc = EmptyDoc
         | Char Char
         | Text !Int String
         | Cat Doc Doc
         | Union Doc Doc
         | Line Bool
         | Nest !Int Doc 
         | Column Doc
         deriving (Eq,Show)
         

mkText :: String -> Doc
mkText s = Text (length s) s



beside          :: Applicative f => f Doc -> f Doc -> f Doc
beside a b      = Cat <$> a <*> b

cat             :: Applicative f => f Doc -> f Doc -> f Doc
cat             = beside

char            :: Applicative f => Char -> f Doc    
char            = pure . Char

text            :: Applicative f => String -> f Doc
text            = pure . mkText

nulldoc         :: Applicative f => f Doc
nulldoc         = pure EmptyDoc

group           :: Applicative f => f Doc -> f Doc
group x         = f <$> x
  where f a = Union (flatten a) a

line            :: Applicative f => f Doc
line            = pure (Line False)

linebreak       :: Applicative f => f Doc
linebreak       = pure (Line True)

nest            :: Applicative f => Int -> f Doc -> f Doc
nest i d        = Nest i <$> d

column          :: Applicative f => f Doc -> f Doc
column d        = Column <$> d

-- | Provide blank as a predicate to test for emptiness

blankA          :: Applicative f => f Doc -> f Bool
blankA          = liftA blank

blank           :: Doc -> Bool
blank EmptyDoc  = True
blank _         = False



isJustA         :: Applicative f => f (Maybe Doc) -> f Bool
isJustA         = liftA isJust


dfold           :: Applicative f => (f Doc -> f Doc -> f Doc) -> [f Doc] -> f Doc
dfold f []      = nulldoc
dfold f ds      = foldr1 f ds


catenate        :: Applicative f => [f Doc] -> f Doc
catenate        = dfold cat

squashSep a b sep | blank a   = b
                  | blank b   = a
                  | otherwise = a `Cat` sep `Cat` b
                  
squash      a b | blank a   = b
                | blank b   = a
                | otherwise = a `Cat` b                  

leftSquash a b | blank a   = b
               | otherwise = a `Cat` b  

leftSquashSep a b sep | blank a   = b
                      | otherwise = a `Cat` sep `Cat` b  

leftHole a b | blank a = EmptyDoc
             | otherwise = a `Cat` b
             
leftHoleSep a b sep | blank a = EmptyDoc
                    | otherwise = a `Cat` sep `Cat` b   
             
                                         
rightHole a b | blank b = EmptyDoc
              | otherwise = a `Cat` b
             
rightHoleSep a b sep | blank b = EmptyDoc
                     | otherwise = a `Cat` sep `Cat` b                 
-- Construct documents from non-empty documents
-- if either side is empty take the opposite
infixr 6 >+<, >~<
(>+<), (>~<)    :: Applicative f => f Doc -> f Doc -> f Doc
x >+< y         = squashSep <$> x <*> y <*> space

x >~< y         = squash <$> x <*> y

                  

 

-- Left-biased versions - the left side is ignored if blank
-- but the right is used regardless
infixr 6 >+, >~
(>+), (>~)      :: Applicative f => f Doc -> f Doc -> f Doc
x >+ y          = leftSquashSep <$> x <*> y <*> space

x >~ y          = leftSquash <$> x <*> y 





infixr 5 ^+^, ^^^
-- | Below
(^+^), (^^^)    :: Applicative f => f Doc -> f Doc -> f Doc
x ^+^ y         = squashSep <$> x <*> y <*> line 

x ^^^ y         = leftSquashSep <$> x <*> y <*> line 


-- suffix iff the left doc is not empty
-- otherwise empty
infixl 3 <~|, <+|
(<~|), (<+|)    :: Applicative f => f Doc -> f Doc -> f Doc
x <~| y         = leftHole <$> x <*> y
            
x <+| y         = leftHoleSep <$> x <*> y <*> space
                        
-- prefix iff the right doc is not empty
-- otherwise empty
infixr 4 |~>, |+>
(|~>), (|+>)    :: Applicative f => f Doc -> f Doc -> f Doc
x |~> y         = rightHole <$> x <*> y
            
x |+> y         = rightHoleSep <$> x <*> y <*> space          


catl, catr      :: Applicative f => f Doc -> f Doc -> f Doc
catl x y        = f <$> x <*> y 
  where f a b   | blank a   = b
                | otherwise = a `Cat` b

catr x y        = f <$> x <*> y 
  where f a b   | blank b   = a
                | otherwise = a `Cat` b




softline        :: Applicative f => f Doc
softline        = group line

infixr 5 |%|
x |%| y         = x >~< softline >~< y


-- | paragraph is fillSep
paragraph       :: Applicative f => [f Doc] -> f Doc
paragraph       = dfold (|%|)






-----------------------------------------------------------
-- Combinators for prelude types (from PPrint)
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"
string          :: Applicative f => String -> f Doc
string ""       = nulldoc
string ('\n':s) = line >~< string s
string s        = case (span (/='\n') s) of
                    (xs,ys) -> text xs >~< string ys
                    
                    
bool          :: Applicative f => Bool -> f Doc                                  
bool          = text . show


int           :: Applicative f => Int -> f Doc                   
int           = text . show

integer       :: Applicative f => Integer -> f Doc 
integer       = text . show

float         :: Applicative f => Float -> f Doc 
float         = text . show

double        :: Applicative f => Double -> f Doc 
double        = text . show

rational      :: Applicative f => Rational -> f Doc 
rational      = text . show





--------------------------------------------------------------------------------
-- punctuation
--------------------------------------------------------------------------------
semi, colon, dot, comma, backslash, equals, space, squote, dquote                  
            :: Applicative f => f Doc


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
            :: Applicative f => f Doc
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
                f Doc -> f Doc -> f Doc -> f Doc
enclose l r d = l >~< d >~< r

parens, angles, brackets, braces, squotes, dquotes, dparens     
            :: Applicative f => f Doc -> f Doc

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

blanky      :: Doc -> Doc -> Doc
blanky EmptyDoc d = EmptyDoc
blanky _        d = d

encloseE     :: Applicative f => 
                (f Doc -> f Doc) -> f Doc -> f Doc
encloseE f d = blanky <$> d <*> f d



parensE, anglesE, bracketsE, bracesE, squotesE, dquotesE, dparensE     
            :: Applicative f => f Doc -> f Doc 
                        
parensE     = encloseE parens
anglesE     = encloseE angles
bracketsE   = encloseE brackets
bracesE     = encloseE braces
squotesE    = encloseE squotes
dquotesE    = encloseE dquotes
dparensE    = encloseE dparens



optE        :: (Applicative f) => Maybe (f Doc) -> f Doc
optE (Just d) = d
optE Nothing  = nulldoc
  
  



-- | prefix or suffix each member in a list
prefixes, suffixes 
            :: Applicative f => f Doc -> [f Doc] -> f Doc
           
prefixes p  = catenate <$> map (cat p) 
suffixes p  = catenate <$> map ((flip cat) p)   

sepSep      :: Applicative f => f Doc -> [f Doc] -> f Doc
sepSep sep  = catenate <$> (intersperse sep)

spaceSep, commaSep
            :: Applicative f => [f Doc] -> f Doc
spaceSep    = sepSep space
commaSep    = sepSep comma






encloseSep  :: Applicative f => 
    f Doc -> f Doc -> f Doc -> [f Doc] -> f Doc
encloseSep left right sep ds
    = case ds of
        []  -> left >~< right
        [d] -> left >~< d >~< right
        _   -> let fn = column . catenate . (intersperse sep) 
               in left >~< (fn ds) >~< right 
                    
encloseSepE  :: Applicative f => 
    f Doc -> f Doc -> f Doc -> [f Doc] -> f Doc
encloseSepE l r s [] = nulldoc   
encloseSepE l r s ds = encloseSep l r s ds 




-- | add n line feeds
linefeed    :: Applicative f => Int ->  f Doc
linefeed n  = foldr cat nulldoc (replicate n line)


-- | separate with n blank lines
linesep     :: Applicative f => Int -> [f Doc] -> f Doc
linesep n ds = catenate $ intersperse (linefeed (n+1)) ds


flatten                 :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Union x y)     = flatten x
flatten (Line break)    
  | break == True       = EmptyDoc 
  | otherwise           = mkText " "
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


-----------------------------------------------------------
-- The pretty type class as per PPrint
-----------------------------------------------------------
class (Applicative f) => Pretty f a where
  pp :: a -> f Doc  
  
  ppl :: [a] -> f Doc
  
  ppo :: Maybe a -> f Doc
  
  ppl ds  = catenate $ map pp ds

   
  ppo Nothing  = nulldoc
  ppo (Just a) = pp a



instance (Applicative f) => Pretty f Doc where
  pp        = pure . id  
  
instance (Applicative f) => Pretty f () where
  pp ()     = text "()"

instance (Applicative f) => Pretty f Bool where
  pp        = text . show
 
instance (Applicative f) => Pretty f Char where
  pp c      = char c
  ppl s     = string s
   
instance (Applicative f) => Pretty f Int where
  pp i      = int i
  
instance (Applicative f) => Pretty f Integer where
  pp i      = integer i

instance (Applicative f) => Pretty f Float where
  pp f      = float f

instance (Applicative f) => Pretty f Double where
  pp d      = double d
  
docs :: (Applicative f, Pretty f a) => [a] -> [f Doc]
docs = (map pp)

  
  


fits w x              | w < 0       = False
fits w (SChar l x)                  = fits (w-1) x
fits w (SText l _ x)  | l <= w      = fits (w-l) x
                      | otherwise   = False
fits w (SBreak _ _)                 = True
fits w (SEmpty)                     = True



test doc = showSimple (renderPretty 0.8 30 doc) 30 [] 
 
testS doc = showSimple doc 15 []

doc2 = stext "hello" (sbreak 1 (stext "world" SEmpty))
 

