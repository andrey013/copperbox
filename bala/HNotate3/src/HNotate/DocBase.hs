{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.DocBase
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Primitives for 'state passing' document building
--
--------------------------------------------------------------------------------


module HNotate.DocBase where


import qualified Text.PrettyPrint.Leijen as PP

type Doc = PP.Doc


type DocK st r = (Doc -> st -> r) -> st -> r

-- @local@ runs a computation with a local copy of the state
-- c.f. @withState@ the Reader monads @local@ operation. 
local :: ((Doc -> st -> Doc) -> st -> Doc)
            -> (Doc -> st -> Doc) -> st -> Doc
local f k = \st -> f (\s -> \_ -> k s st) st  

-- @update@ applies the function f to the state
-- @update@ is essential, it allows us to handle document combinators 
-- which produce out put and change the state e.g. a /meter/ combinator
-- would print the new time signature in a score but also change the 
-- /meter/ value in the state so subsequent tunes would be evaluated 
-- with the new time signature.
-- If @update@ wasn't essential (giving us an environment rather than 
-- state) we could simply use the (->) applicative functor.
--
-- (Note, this module could be reimplemented using a state monad instead 
-- of CPS...)
update :: (st -> st) -> (Doc -> st -> r) -> st -> r
update f k = \st -> k PP.empty (f st) 

current :: (st -> PP.Doc) -> (Doc -> st -> r) -> st -> r
current f k = \st -> k (f st) st


-- @caten@ is a general function for combining two /CPS Docs/.
--  
caten :: (Doc -> Doc  -> Doc) ->
         ((Doc -> st -> b) -> st -> a) ->
         ((Doc -> st -> c) -> st -> b) ->
         ((Doc -> st -> c) -> st -> a)
caten op f1 f2 k = \st -> 
                  f1 (\d1 st1 -> 
                    f2 (\d2 st2 -> k (d1 `op` d2) st2) st1) st 

-- @document@ lifts a Doc to a /CPS Doc/.
document :: Doc -> (Doc -> st -> r) -> st -> r
document d k = k d

-- @printState@ - print the current state when it is an instance of Show. 
printState :: Show st => (Doc -> st -> r) -> st -> r
printState k = \st -> k (PP.string $ show st) st

-- @empty@ - CPS version of PP.empty
empty :: (Doc -> st -> r) -> st -> r
empty k = k (PP.empty)

-- @text@ - CPS version of PP.text
text :: String -> (Doc -> st -> r) -> st -> r
text x k = k (PP.text x)

-- @string@ - CPS version of PP.string
string :: String -> (Doc -> st -> r) -> st -> r
string x k = k (PP.string x)

-- @int@ - CPS version of PP.int
int :: Int -> (Doc -> st -> r) -> st -> r
int i k = k (PP.int i)

-- @integer@ - CPS version of PP.integer
integer :: Integer -> (Doc -> st -> r) -> st -> r
integer i k = k (PP.integer i)


-- @char@ - CPS version of PP.char
char :: Char -> (Doc -> st -> r) -> st -> r
char c k = k (PP.char c)

eol :: (Doc -> st -> a) -> st ->  a
eol k = k (PP.line)


(<$>) :: ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)
(<$>) = caten (PP.<$>)

(<+>) :: ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)
(<+>) = caten (PP.<+>)
  
(<>) :: ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)
      -> ((Doc -> st -> Doc) -> st -> Doc)  
(<>)  = caten (PP.<>)
  
  
colon :: (Doc -> st -> r) -> st -> r
colon = document (PP.colon)

line :: (Doc -> st -> r) -> st -> r
line = document (PP.line)


sprintf :: st -> ((Doc -> st -> Doc) -> st -> r) -> r
sprintf st p = p (\s _st -> s) st

pprender :: PP.Doc -> String
pprender doc = PP.displayS (PP.renderPretty 0.4 80 doc) ""


                                          