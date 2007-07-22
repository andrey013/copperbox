

module Language.C.Pretty.MonadicEBP where

-- import Language.C.Pretty.EBPretty hiding (caten)
import qualified Language.C.Pretty.EBPretty as P

import Control.Monad.Reader


-- | The pretty printer is just a reader monad...
type PrettyM a = Reader PPStyle a

data PPStyle = PPStyle {
  block_indent :: Int
  }
  
type PDoc =  PrettyM P.Doc
type PDocs = PrettyM [P.Doc]

type Doc = PDoc  


plainStyle = PPStyle { 
  block_indent = 4 
  }

displayIO       = P.displayIO
renderPretty    = P.renderPretty

runPretty       = runReader   


caten :: [PDoc] -> PDoc
caten dl = sequence dl >>= return . P.caten

catenx :: PrettyM [P.Doc] -> PDoc
catenx dl = do dl' <- dl 
               return (P.caten dl')



lap1 :: (Monad m) => (a -> b) -> m a -> m b
lap1 f am = do { a <- am ; return (f a) }

lap2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
lap2 op am bm = do { a <- am ; b <- bm ; return (op a b) } 

lap3 :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lap3 f am bm cm = do { a <- am ; b <- bm ;  c <- cm ; return (f a b c) }

lap4 :: (Monad m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lap4 f am bm cm dm = 
  do { a <- am ; b <- bm ;  c <- cm ; d <- dm ; return (f a b c d) }

cat             :: PDoc -> PDoc -> PDoc
cat             = lap2 P.cat   
  
char            :: Char -> PDoc
char            = return . P.char

text            :: String -> PDoc
text            = return . P.text

empty           :: PDoc
empty           = return P.empty

group           :: PDoc -> PDoc
group           = lap1 P.group

line            :: PDoc
line            = return P.line

linebreak       :: PDoc
linebreak       = return P.linebreak

nest            :: Int -> PDoc -> PDoc
nest i          = lap1 (P.nest i)

column          :: PDoc -> PDoc
column          = lap1 P.column
  
  
  
(>+<), (>~<), (>+), (>~),  (^+^), (^^^),
  (<~|), (<+|), (|~>), (|+>) :: PDoc -> PDoc -> PDoc
  
(>+<)   = lap2 (P.>+<)
(>~<)   = lap2 (P.>~<)
(>+)    = lap2 (P.>+)
(>~)    = lap2 (P.>~)
(^+^)   = lap2 (P.^+^)
(^^^)   = lap2 (P.^^^)
(<+|)   = lap2 (P.<+|)
(<~|)   = lap2 (P.<+|)
(|+>)   = lap2 (P.|+>)
(|~>)   = lap2 (P.|~>)

string        :: String -> PDoc
string        = return . P.string

bool          :: Bool -> PDoc
bool          = return . P.bool

int           :: Int -> PDoc
int           = return . P.int

integer       :: Integer -> PDoc
integer       = return . P.integer

float         :: Float -> PDoc
float         = return . P.float

double        :: Double -> PDoc
double        = return . P.double

rational      :: Rational -> PDoc
rational      = return . P.rational



semi, colon, dot, comma, backslash, equals, space, squote, dquote,
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket :: PDoc

semi        = return P.semi
colon       = return P.colon
dot         = return P.dot
comma       = return P.comma
backslash   = return P.backslash
equals      = return P.equals
space       = return P.space
squote      = return P.squote
dquote      = return P.dquote
lparen      = return P.lparen
rparen      = return P.rparen
langle      = return P.langle
rangle      = return P.rangle
lbrace      = return P.lbrace
rbrace      = return P.rbrace
lbracket    = return P.lbracket
rbracket    = return P.rbracket
  
enclose     :: PDoc -> PDoc -> PDoc -> PDoc
enclose     = lap3 P.enclose

parens      :: PDoc -> PDoc
parens      = lap1 P.parens 

angles      :: PDoc -> PDoc
angles      = lap1 P.angles

brackets    :: PDoc -> PDoc
brackets    = lap1 P.brackets

braces      :: PDoc -> PDoc
braces      = lap1 P.braces

squotes     :: PDoc -> PDoc
squotes     = lap1 P.squotes

dquotes     :: PDoc -> PDoc
dquotes     = lap1 P.dquotes

dparens     :: PDoc -> PDoc
dparens     = lap1 P.dparens 

                 
               
prefixes, suffixes :: PDoc -> PDocs -> PDoc
prefixes    = lap2 P.prefixes 
suffixes    = lap2 P.suffixes  



sepSep      :: PDoc -> PDocs -> PDoc
sepSep      = lap2 P.sepSep

spaceSep    :: PDocs -> PDoc
spaceSep    = lap1 P.spaceSep

commaSep    :: PDocs -> PDoc
commaSep    = lap1 P.commaSep

encloseSep  :: PDoc -> PDoc -> PDoc -> PDocs -> PDoc
encloseSep  = lap4 P.encloseSep

encloseSepE :: PDoc -> PDoc -> PDoc -> PDocs -> PDoc
encloseSepE = lap4 P.encloseSepE

linefeed    :: Int -> PDoc
linefeed i  = return (P.linefeed i)


linesep     :: Int -> PDocs -> PDoc
linesep i   = lap1 (P.linesep i)
                 

-----------------------------------------------------------
-- The pretty type class as per PPrint
-----------------------------------------------------------
class Pretty a where
  pp :: a -> PDoc
  ppl :: [a] -> PDoc
  ppo :: Maybe a -> PDoc
  
  ppl ds  = mapM pp ds >>= \ds' -> return (P.caten ds')

  
  ppo Nothing  = empty
  ppo (Just a) = pp a
   
  
instance Pretty P.Doc where
  pp        = return . id  
  
instance Pretty () where
  pp ()     = text "()"

instance Pretty Bool where
  pp        = text . show

  
instance Pretty Char where
  pp c      = char c
  ppl s     = string s
    
instance Pretty Int where
  pp i      = int i
  
instance Pretty Integer where
  pp i      = integer i

instance Pretty Float where
  pp f      = float f

instance Pretty Double where
  pp d      = double d
  


docs :: Pretty a => [a] -> PrettyM [P.Doc]
docs = mapM pp



  


attribSep   = encloseSep lparen rparen comma
attribSepE  = encloseSepE lparen rparen comma

enumSep     = encloseSep lbrace rbrace comma 
enumSepE    = encloseSepE lbrace rbrace comma 

structSep   = encloseSep lbrace rbrace semi 
structSepE  = encloseSepE lbrace rbrace semi 

tupleSep    = encloseSep lparen rparen comma
tupleSepE   = encloseSepE lparen rparen comma


-- | subscript - array subscripting
subscript e e' = e >~< char '[' >~< e' >~< char ']'


