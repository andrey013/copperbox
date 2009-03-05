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
-- More typed than StringPostScript
--
--------------------------------------------------------------------------------

module Graphics.Hawa.TypedPostScript where

import Prelude hiding ( mod, abs )
import qualified Prelude as Pre

type PostScript = String

type Code = (String -> String)
type Stack a = (String -> String,a) 

begin :: (Stack () -> r) -> r
begin k = k $ (id,())

end   :: Stack a -> Stack a
end (f,a) = (f,a)

runCode :: Stack a -> PostScript
runCode c = (fst c) []

runStack :: Stack a -> a
runStack c = snd c

-- In PostScript literals are automatically pused on the stack,
-- here we use an explicit push
lit :: Show z => Stack a -> z -> (Stack (z,a) -> r) -> r 
lit (f, a) x k = k (f . showChar ' ' . shows x, (x,a))

pushi :: Stack a -> Int -> (Stack (Int,a) -> r) -> r 
pushi = lit

pushf :: Stack a -> Float -> (Stack (Float,a) -> r) -> r 
pushf = lit


pop         :: Stack (a,s) -> (Stack s -> r) -> r
pop (f, (_,s)) k   = k (f . showString " pop", s)  


exch        :: Stack (a,(b,s)) -> (Stack (b,(a,s)) -> r) -> r
exch (f, (a,(b,s))) k  = k (f . showString " exch", (b,(a,s))) 



dup         :: Stack (a,s) -> (Stack (a,(a,s)) -> r) -> r
dup (f, (a,s)) k  = k (f . showString " dup", (a,(a,s)))
 

-- Fiendish! 
-- A typed implementation of these stack ops would need type-level 
-- numerals (I think). Instead they send the appropriate command
-- to the PostScript stream, but don't touch the stack.
-- Not brilliant, but the supposed /typefulness/ of the stack is
-- there to help PostScript generation and not be and end in itself.   

-- copy does pop the top, but doesn't actually copy
copy        :: Stack (Int,s) -> (Stack s -> r) -> r
copy (f, (_,s)) k = k (f . showString " copy", s)

-- index does pop the top, but doesn't actually copy
index       :: Stack (Int,s) -> (Stack s -> r) -> r
index (f, (_,s)) k = k (f . showString " copy", s)

-- roll does pop the top, but doesn't actually copy
roll        :: Stack (Int,(Int,s)) -> (Stack s -> r) -> r
roll (f, (_,(_,s))) k = k (f . showString " index", s)


-- clear does work!
clear       :: Stack a -> (Stack () -> r) -> r
clear (f, _) k = k (f . showString " clear", ())

-- count doesn't actually count the stack, though it could.
-- (Counting would waste time when we are really interested in the output)
count       :: Stack s -> (Stack (Int,s) -> r) -> r
count (f, s) k = k (f . showString " clear", (0,s))

type Mark = ()


mark        :: Stack s -> (Stack (Mark,s) -> r) -> r
mark (f, s) k = k (f . showString " mark", ((),s))


cleartomark :: Stack s -> (Stack s -> r) -> r
cleartomark (f, s) k = k (f . showString " cleartomark", s)

counttomark :: Stack s -> (Stack (Int,s) -> r) -> r
counttomark (f, s) k = k (f . showString " counttomark", (0,s))


-- arithmetic and math ops
-- Actually PostScript appears to do type coercion at will (not especially 
-- surprising as it is (presumably) dynamically typed).
-- Maybe the types here are too restrictive.

 
add :: Num a => Stack (a,(a,st)) -> (Stack (a,st) -> r) -> r
add (f, (a,(b,s))) k = k (f . showString " add", (a+b,s))

divi :: Stack (Int,(Int,st)) -> (Stack (Float,st) -> r) -> r
divi (f, (a,(b,s))) k = 
    k (f . showString " div", (fromIntegral a / fromIntegral b,s))

divf :: Stack (Float,(Float,st)) -> (Stack (Float,st) -> r) -> r
divf (f, (a,(b,s))) k = k (f . showString " div", (a / b,s))

idiv :: Stack (Int,(Int,st)) -> (Stack (Int,st) -> r) -> r
idiv (f, (a,(b,s))) k = k (f . showString " idiv", (a `div` b,s))


mod         :: Stack (Int,(Int,st)) -> (Stack (Int,st) -> r) -> r
mod (f, (a,(b,s))) k = k (f . showString " mod", (a `Pre.mod` b,s))


mul :: Num a => Stack (a,(a,st)) -> (Stack (a,st) -> r) -> r
mul (f, (a,(b,s))) k = k (f . showString " mul", (a*b,s))


sub :: Num a => Stack (a,(a,st)) -> (Stack (a,st) -> r) -> r
sub (f, (a,(b,s))) k = k (f . showString " sub", (a-b,s))


abs :: Num a => Stack (a,s) -> (Stack (a,s) -> r) -> r
abs (f, (a,s)) k = k (f . showString " abs", (Pre.abs a,s)) 

neg :: Num a => Stack (a,s) -> (Stack (a,s) -> r) -> r
neg (f, (a,s)) k = k (f . showString " neg", (negate a,s)) 


ceilingf :: Stack (Float,s) -> (Stack (Int,s) -> r) -> r
ceilingf (f, (a,s)) k = k (f . showString " ceiling", (Pre.ceiling a,s)) 


ceilingi :: Stack (Int,s) -> (Stack (Int,s) -> r) -> r
ceilingi (f, (a,s)) k = k (f . showString " ceiling", (a,s)) 


floorf :: Stack (Float,s) -> (Stack (Int,s) -> r) -> r
floorf (f, (a,s)) k = k (f . showString " floor", (floor a,s)) 

floori :: Stack (Int,s) -> (Stack (Int,s) -> r) -> r
floori (f, (a,s)) k = k (f . showString " floor", (a,s)) 


roundf :: Stack (Float,s) -> (Stack (Int,s) -> r) -> r
roundf (f, (a,s)) k = k (f . showString " round", (round a,s)) 

roundi :: Stack (Int,s) -> (Stack (Int,s) -> r) -> r
roundi (f, (a,s)) k = k (f . showString " round", (a,s)) 


{-

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

-}





  

  