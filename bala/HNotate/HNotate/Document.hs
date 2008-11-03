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
  ODoc,
  
  emptyDoc, isEmpty,
  text, char, string, fillString, int,
  ( <> ), ( <+> ), ( <&\> ),
  hcat, hsep, vsep,
  punctuate,
  enclose, encloseSpace, dup,
  line, space, comma, colon, dot, dash, equals, prime,
  
  lbrace, rbrace, braces, braces',
  lparen, rparen, parens, parens',
  lbracket, rbracket, brackets, brackets',
  langle, rangle, angles, angles', dblangles, dblangles',
  lbanana, rbanana, bananas, bananas',
  llens, rlens, lenses, lenses',
  indent, nest,   --- ?? 
  list, tupled,
  underline,
  command, command1, command2,
  PP(..),
  output, formatted, quickOutput, unformatted 
  ) where

import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Sequence hiding (length)
import Prelude hiding (null)


newtype ODoc = ODoc { getODoc :: Seq Particle }
  deriving Show
  
data Particle = Text Int String
              | Space           -- maybe rendered as space, maybe newline
              | LineBreak 
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


-- can we do anything for indent and nest?
indent :: Int -> ODoc -> ODoc
indent i d = d

nest :: Int -> ODoc -> ODoc
nest i d = d

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
formatted indent_level width  = (output indent_level width) `flip` "\n"  

output :: Int -> Int -> ODoc -> ShowS
output indent_level width = 
    first . F.foldl out (indent,False,0) . getODoc
  where     
    out (f,sp,j) (Text i s)
          -- not a spacing break so we carry on on the same line 
          -- regardless of whether it actually 'fits'.                       
        | not sp           = (f . showString s, False, i+j) 
          -- spacing break - so it can either be a ' ' if the 
          -- line fits...           
        | fits (j + i)     = (f . spaceS . showString s, False, i+j)
          -- ...or a newline if it doesn't.           
        | otherwise        = (f . nlS . indent . showString s, False, i) 

          -- Space encountered - just change the state 
          -- (don't add it to the output)           
    out (f,_ ,j) (Space)      = (f, True, j+1)
    
          -- linebreak - add it to the doc and reset the state
    out (f,_ ,j) (LineBreak)  = (f . nlS . indent, False, 0)
    
    -- helpers
    indent = showString (replicate indent_level ' ') 
    fits i = i <= width - indent_level


spaceS, nlS :: ShowS
spaceS  = showChar ' '
nlS     = showChar '\n'

first :: (a,b,c) -> a
first (a,_,_) = a
    
    

  
    