{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Hawa.StringPostScript
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Generate strings of PostScript commands 
-- completely untyped, easy to make garbage, hard to pretty print
--
--------------------------------------------------------------------------------

module Graphics.Hawa.StringPostScript where

type PostScript = String
type Code = (String -> String)
type Stack = (String -> String) 

begin ::                 (Stack -> r) -> r
end   :: Stack -> Code

begin k = k $ id
end stk = stk . id


-- begin is necessary to create a typeable expression
-- e1 :: (Stack -> r) -> r
-- e1 = begin lit (0.0::Float) setgray 

-- e2 :: WON'T TYPECHECK
-- e2 = lit (0.0::Float) setgray 


runCode :: Code -> PostScript
runCode f = f []


class Lit a where
  lit :: Stack -> a -> (Stack -> r) -> r 

instance Lit Int where
  lit stk x k = k (stk . showChar ' ' . shows x) 

instance Lit Float where
  lit stk x k = k (stk . showChar ' ' . shows x) 
  
-- alternatively
litI :: Stack -> Int -> (Stack -> r) -> r 
litI = lit

litF :: Stack -> Float -> (Stack -> r) -> r 
litF = lit

pop         :: Stack ->        (Stack -> r) -> r
pop stk k   = k (stk . showString " pop")  

exch        :: Stack ->        (Stack -> r) -> r
exch stk k  = k (stk . showString " exch") 

-- getting boring, define a short cut
rator :: String -> Stack -> (Stack -> r) -> r
rator str = \stk k -> k (stk . showString (' ':str))

dup         :: Stack ->        (Stack -> r) -> r
dup         = rator "dup"

-- copy could be typed :: Stack -> Int -> (Stack -> r) -> r
-- with it always using a literal Int ...

copy        :: Stack ->        (Stack -> r) -> r
copy        = rator "copy"

index       :: Stack ->        (Stack -> r) -> r
index       = rator "index"

roll        :: Stack ->        (Stack -> r) -> r
roll        = rator "roll"

clear       :: Stack ->        (Stack -> r) -> r
clear       = rator "clear"

count       :: Stack ->        (Stack -> r) -> r
count       = rator "count"

mark        :: Stack ->        (Stack -> r) -> r
mark        = rator "mark"

cleartomark :: Stack ->        (Stack -> r) -> r
cleartomark = rator "cleartomark"

counttomark :: Stack ->        (Stack -> r) -> r
counttomark = rator "counttomark"


-- arithmetic and math ops 
add         :: Stack ->        (Stack -> r) -> r
add         = rator "add" 

div         :: Stack ->        (Stack -> r) -> r
div         = rator "div"

mod         :: Stack ->        (Stack -> r) -> r
mod         = rator "mod"

mul         :: Stack ->        (Stack -> r) -> r
mul         = rator "mul"

sub         :: Stack ->        (Stack -> r) -> r
sub         = rator "sub"

abs         :: Stack ->        (Stack -> r) -> r
abs         = rator "abs"

neg         :: Stack ->        (Stack -> r) -> r
neg         = rator "neg"

ceiling     :: Stack ->        (Stack -> r) -> r
ceiling     = rator "ceiling"

floor       :: Stack ->        (Stack -> r) -> r
floor       = rator "floor"

round       :: Stack ->        (Stack -> r) -> r
round       = rator "round"

truncate    :: Stack ->        (Stack -> r) -> r
truncate    = rator "truncate"

sqrt        :: Stack ->        (Stack -> r) -> r
sqrt        = rator "sqrt"

atan        :: Stack ->        (Stack -> r) -> r
atan        = rator "atan"

cos         :: Stack ->        (Stack -> r) -> r
cos         = rator "cos"

sin         :: Stack ->        (Stack -> r) -> r
sin         = rator "sin"

exp         :: Stack ->        (Stack -> r) -> r
exp         = rator "exp"

ln         :: Stack ->        (Stack -> r) -> r
ln          = rator "ln"

log         :: Stack ->        (Stack -> r) -> r
log         = rator "log"

rand        :: Stack ->        (Stack -> r) -> r
rand        = rator "rand"

srand       :: Stack ->        (Stack -> r) -> r
srand       = rator "srand"

rrand       :: Stack ->        (Stack -> r) -> r
rrand       = rator "rrand"



  
setgray   :: Stack ->        (Stack -> r) -> r
setgray stk k = k $ stk . showString " setgray"







  

  