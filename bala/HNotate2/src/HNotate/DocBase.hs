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
-- Primitives for 'environment passing' doucument building
--
--------------------------------------------------------------------------------


module HNotate.DocBase where


import qualified Text.PrettyPrint.Leijen as PP

type Doc = PP.Doc


type DocK env r = (Doc -> env -> r) -> env -> r

-- @local@ runs a computation with a local copy of the env
-- c.f. the Reader monads local operation. 
local :: ((Doc -> env -> Doc) -> env -> Doc)
            -> (Doc -> env -> Doc) -> env -> Doc
local f k = \env -> f (\s -> \_ -> k s env) env  

-- @update@ applies the function f to the enviroment
update :: (env -> env) -> (Doc -> env -> r) -> env -> r
update f k = \env -> k PP.empty (f env) 

current :: (env -> PP.Doc) -> (Doc -> env -> r) -> env -> r
current f k = \env -> k (f env) env


-- @caten@ is the general function for combining two /CPS Docs/.
--  
caten :: (Doc -> Doc  -> Doc) ->
         ((Doc -> env -> b) -> env -> a) ->
         ((Doc -> env -> c) -> env -> b) ->
         ((Doc -> env -> c) -> env -> a)
caten op f1 f2 k = \env -> 
                  f1 (\d1 env1 -> 
                    f2 (\d2 env2 -> k (d1 `op` d2) env2) env1) env 

-- @document@ lifts a Doc to a /CPS Doc/.
document :: Doc -> (Doc -> env -> r) -> env -> r
document d k = k d

-- @printEnv@ - print the current enviroment when it is an instance of Show. 
printEnv :: Show env => (Doc -> env -> r) -> env -> r
printEnv k = \env -> k (PP.string $ show env) env

-- @empty@ - CPS version of PP.empty
empty :: (Doc -> env -> r) -> env -> r
empty k = k (PP.empty)

-- @text@ - CPS version of PP.text
text :: String -> (Doc -> env -> r) -> env -> r
text x k = k (PP.text x)

-- @string@ - CPS version of PP.string
string :: String -> (Doc -> env -> r) -> env -> r
string x k = k (PP.string x)

-- @int@ - CPS version of PP.int
int :: Int -> (Doc -> env -> r) -> env -> r
int i k = k (PP.int i)

-- @integer@ - CPS version of PP.integer
integer :: Integer -> (Doc -> env -> r) -> env -> r
integer i k = k (PP.integer i)


-- @char@ - CPS version of PP.char
char :: Char -> (Doc -> env -> r) -> env -> r
char c k = k (PP.char c)

eol :: (Doc -> env -> a) -> env ->  a
eol k = k (PP.line)


(<$>) :: ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)
(<$>) = caten (PP.<$>)

(<+>) :: ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)
(<+>) = caten (PP.<+>)
  
(<>) :: ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)
      -> ((Doc -> env -> Doc) -> env -> Doc)  
(<>)  = caten (PP.<>)
  
  
colon :: (Doc -> env -> r) -> env -> r
colon = document (PP.colon)

line :: (Doc -> env -> r) -> env -> r
line = document (PP.line)


sprintf :: env -> ((Doc -> env -> Doc) -> env -> r) -> r
sprintf env p = p (\s _env -> s) env

pprender :: PP.Doc -> String
pprender doc = PP.displayS (PP.renderPretty 0.4 80 doc) ""


                                          