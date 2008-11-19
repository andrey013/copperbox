--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Document
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- A specialized miniature PPrint like module.
-- Formatting is largely explicit so fitting is more limited.   
--------------------------------------------------------------------------------

module HNotate.Document (
  ODoc, ODocS,
  
  emptyDoc, isEmpty,
  text, char, string, fillString, int,
  ( <> ), ( <+> ), ( <&\> ),
  hcat, hsep, vsep,
  punctuate,
  enclose, encloseSpace, dup,
  line, space, comma, colon, dot, dash, equals, prime,
  
  quotes, dblquotes,
  lbrace, rbrace, braces, braces',
  lparen, rparen, parens, parens',
  lbracket, rbracket, brackets, brackets',
  langle, rangle, angles, angles', dblangles, dblangles',
  lbanana, rbanana, bananas, bananas',
  llens, rlens, lenses, lenses',
  indent,
  list, tupled,
  underline,
  command, command1, command2,
  
  lineS,
  
  PP(..),
  output, formatted, quickOutput, unformatted 
  ) where

import Control.Monad.State
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Sequence hiding (length)
import Prelude hiding (null)


newtype ODoc = ODoc { getODoc :: Seq Particle }
  deriving Show

type ODocS = ODoc -> ODoc
  
data Particle = Text Int String
              | Space           -- maybe rendered as space, maybe newline
              | LineBreak
              | IndentStart Int
              | IndentEnd 
  deriving Show

spacejoin :: ODoc -> ODoc -> ODoc
spacejoin a b = ODoc $ (getODoc a |> Space) >< getODoc b

nobreakjoin :: ODoc -> ODoc -> ODoc
nobreakjoin a b = ODoc $ getODoc a  >< getODoc b

breakjoin :: ODoc -> ODoc -> ODoc
breakjoin a b = ODoc $ (getODoc a |> LineBreak) >< getODoc b


-- an alias (useful if also importing Data.Sequence etc.)
emptyDoc :: ODoc
emptyDoc = ODoc $ empty


isEmpty :: ODoc -> Bool
isEmpty = null . getODoc 

text :: String -> ODoc
text s = ODoc $ singleton $ Text (length s) s

char :: Char -> ODoc
char c = text [c]


int :: Int -> ODoc
int  = text . show

string :: String -> ODoc
string = build . map text . lines
  where
    build [x]     = x
    build (x:xs)  = foldl breakjoin x xs

fillString :: Int -> String -> ODoc
fillString i s = let l = length s in
  if i > l then string $ s ++ replicate (i - l) ' '  else string s 

spacing :: Int -> ODoc
spacing i = text $ replicate i ' '

infixr 6 <>,<+>
(<>) :: ODoc -> ODoc -> ODoc
a <> b | isEmpty a    = b
       | isEmpty b    = a
       | otherwise    = nobreakjoin a b

(<+>) :: ODoc -> ODoc -> ODoc
a <+> b | isEmpty a   = b
        | isEmpty b   = a
        | otherwise   = spacejoin a b

(<&\>) :: ODoc -> ODoc -> ODoc
a <&\> b | isEmpty a   = b
         | isEmpty b   = a
         | otherwise   = breakjoin a b
        
        
hcat :: [ODoc] -> ODoc
hcat = foldl (<>) emptyDoc        

hsep :: [ODoc] -> ODoc
hsep = foldl (<+>) emptyDoc 

vsep :: [ODoc] -> ODoc
vsep = foldl (<&\>) emptyDoc 

punctuate :: ODoc -> [ODoc] -> [ODoc] 
punctuate = intersperse

enclose :: ODoc -> ODoc -> ODoc -> ODoc
enclose l r d = l <> d <> r

encloseSpace :: ODoc -> ODoc -> ODoc -> ODoc
encloseSpace l r d = (postSp l) <> d <> (preSp r)

encloseDup :: ODoc -> ODoc -> ODoc -> ODoc
encloseDup l r d = (dup l) <> d <> (dup r)

encloseDupSpace :: ODoc -> ODoc -> ODoc -> ODoc
encloseDupSpace l r d = (postSp $ dup l) <> d <> (preSp $ dup r)


dup :: ODoc -> ODoc
dup d = d <> d

postSp :: ODoc -> ODoc
postSp = (<> space)

preSp :: ODoc -> ODoc
preSp = (space <>)



-----
line :: ODoc
line = char '\n'

space :: ODoc
space = char ' '

comma     :: ODoc
comma     = char ','

colon     :: ODoc
colon     = char ':'

dot       :: ODoc
dot       = char '.'
 
dash      :: ODoc
dash      = char ':'

equals    :: ODoc
equals    = char '='

prime     :: ODoc
prime     = char '\''

quotes        :: ODoc -> ODoc
quotes        = enclose prime prime

dblquotes     :: ODoc -> ODoc
dblquotes     = enclose (char '"') (char '"')


-- ---
lbrace        :: ODoc  
lbrace        = char '{'

rbrace        :: ODoc 
rbrace        = char '}'

braces        :: ODoc -> ODoc
braces        = enclose lbrace rbrace

braces'       :: ODoc -> ODoc
braces'       = encloseSpace lbrace rbrace

-- ---
lparen        :: ODoc  
lparen        = char '('

rparen        :: ODoc 
rparen        = char ')'

parens        :: ODoc -> ODoc
parens        = enclose lparen rparen

parens'       :: ODoc -> ODoc
parens'       = encloseSpace lparen rparen


-- ----
lbracket      :: ODoc  
lbracket      = char '['

rbracket      :: ODoc 
rbracket      = char ']'

brackets      :: ODoc -> ODoc
brackets      = enclose lbracket rbracket


brackets'     :: ODoc -> ODoc
brackets'     = encloseSpace lbracket rbracket

-- ---
langle        :: ODoc  
langle        = char '<'

rangle        :: ODoc 
rangle        = char '>'

angles        :: ODoc -> ODoc
angles        = enclose langle rangle

angles'       :: ODoc -> ODoc
angles'       = encloseSpace langle rangle

dblangles     :: ODoc -> ODoc
dblangles     = encloseDup langle rangle

dblangles'    :: ODoc -> ODoc
dblangles'    = encloseDupSpace langle rangle 

-- ----
lbanana       :: ODoc
lbanana       = text "(|"

rbanana       :: ODoc
rbanana       = text "|)"

bananas       :: ODoc -> ODoc
bananas       = enclose lbanana rbanana

bananas'      :: ODoc -> ODoc
bananas'      = encloseSpace lbanana rbanana

llens         :: ODoc
llens         = text "[("

rlens         :: ODoc
rlens         = text ")]"

lenses        :: ODoc -> ODoc
lenses        = enclose llens rlens

lenses'       :: ODoc -> ODoc
lenses'       = encloseSpace llens rlens


indent :: Int -> ODoc -> ODoc
indent i d = start <> d <> end where
  start = ODoc $ singleton $ IndentStart i
  end   = ODoc $ singleton $ IndentEnd



list :: [ODoc] -> ODoc
list = brackets . foldr fn emptyDoc
    where fn a d | isEmpty d  = a
                 | otherwise  = a <> comma <+> d 

tupled :: [ODoc] -> ODoc     
tupled = parens .  foldr fn emptyDoc
    where fn a d | isEmpty d  = a
                 | otherwise  = a <> comma <+> d 

underline :: String -> ODoc
underline s = text s <&\> text (replicate (length s) '-') <> line 


command :: String -> ODoc
command = text . ('\\':)

command1 :: String -> ODoc -> ODoc
command1 s d = command s <+> d

command2 :: String -> ODoc -> ODoc -> ODoc
command2 s d1 d2 = command s <+> d1 <+> d2

--------------------------------------------------------------------------------
-- ODocS variants 

lineS :: ODoc -> ODocS
lineS d = (d <&\>)


--------------------------------------------------------------------------------
-- class 

class PP a where 
  pp    :: a -> ODoc
  ppSeq :: Seq a -> ODoc
  
  ppSeq = bananas' . F.foldr fn emptyDoc
    where fn a d | isEmpty d  = pp a
                 | otherwise  = pp a <> comma <+> d 

instance PP Bool where pp = text . show
instance PP Char where pp = char
instance PP Int where pp = int
instance PP Integer where pp = text . show


--------------------------------------------------------------------------------
-- rendering
        
quickOutput :: ODoc -> ShowS
quickOutput = F.foldl fn id . getODoc
  where
    fn f (Text _ s)   = f . showString s
    fn f (Space)      = f . showChar ' '
    fn f (LineBreak)  = f . showChar '\n'


unformatted :: ODoc -> String 
unformatted = quickOutput `flip` "\n"

formatted :: Int -> Int -> ODoc -> String 
formatted left_col right_col  = (output left_col right_col) `flip` "\n"  
   

type IndentStack = [Int]
data LineCont    = BREAK | NO_BREAK
type Width       = Int
type OutputState = (IndentStack, LineCont, Width, ShowS)

-- Note IndentStack should never be empty
-- It is created with 1 element, and push & pop are created symmetrically 
-- by the \indent\ function.

output :: Int -> Int -> ODoc -> ShowS
output left_col right_col = 
    fourth . F.foldl' out state0 . getODoc
  where
    state0          = ([left_col], NO_BREAK, 0, indentS left_col)  
    
    fits :: Width -> IndentStack -> Bool  
    fits i (x:_)    = i <= right_col - x
    fits i []       = i <= right_col - left_col -- should be unreachable
    
    out :: OutputState -> Particle -> OutputState
    -- not a break, so we carry on on the same line regardless of 
    -- whether it the line actually fits between the left and right columns
    out (stk, NO_BREAK, w, f) (Text i s) 
                            = (stk, NO_BREAK, w+i, f . showString s)

    -- spacing break - print space if the line fits, 
    -- otherwise go on a new line     
    out (stk, BREAK,    w, f) (Text i s)
        | fits (w+i) stk    = (stk, NO_BREAK, w+i, f . spaceS . showString s)
        | otherwise         = (stk, NO_BREAK, i,   f . newlineS 
                                                     . indentS (depth stk)
                                                     . showString s)

    -- Space encountered - just change the state (don't add it to the output)           
    out (stk, _,        w, f) Space      
                            = (stk, BREAK,    w+1, f)
    
    -- linebreak - add it to the doc and reset the state
    out (stk, _,        w, f) LineBreak  
                            = (stk, NO_BREAK, 0,   f . newlineS 
                                                     . indentS (depth stk))
 
    -- must push (w+i) - curent width (w) and the indent level (i) 
    out (stk, _,        w, f) (IndentStart i)   
                            = let stk' = push (w+i) stk
                              in (stk', NO_BREAK, w+i, f . indentS i)
                            
    out (stk, _,        w, f) IndentEnd   
                            = (pop stk, NO_BREAK, w, f)                       
        
-- output helpers

push :: Int -> IndentStack -> IndentStack
push i xs       = i:xs

pop :: IndentStack -> IndentStack
pop (x:xs)      = xs

depth :: IndentStack -> Int
depth xs = sum xs 


indentS :: Int -> ShowS  
indentS x   = showString (replicate x ' ')
    
first :: (a,b,c,d) -> a
first (a,_,_,_) = a

fourth :: (a,b,c,d) -> d
fourth (_,_,_,d) = d

newlineS    = showChar '\n'
spaceS      = showChar ' '

  
    