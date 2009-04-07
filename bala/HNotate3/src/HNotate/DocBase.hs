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
-- Build documents with a state monad
--
--------------------------------------------------------------------------------


module HNotate.DocBase where


import Control.Monad.State
import qualified Text.PrettyPrint.Leijen as PP

type Doc = PP.Doc
 
type DocSt st = State st Doc 


update :: (st -> st) -> DocSt st 
update f = get >>= \st -> put (f st) >> return PP.empty


local :: DocSt st -> DocSt st
local f = get >>= \st -> return $ evalState f st 



current :: (st -> PP.Doc) -> DocSt st
current f = get >>= \st -> return (f st)

-- @document@ lifts a Doc to a /CPS Doc/.
document :: Doc -> DocSt st
document = return

-- @caten@ is the general function for combining two /DocSt/.
--  
caten :: (Doc -> Doc  -> Doc) ->
         DocSt st -> DocSt st -> DocSt st 
caten op f g = f >>= \a -> g >>= \b -> return $ a `op` b


-- @printEnv@ - print the current enviroment when it is an instance of Show. 
printEnv :: Show st => DocSt st
printEnv = get >>= return . PP.string . show


-- @empty@ - applicative env passing version of PP.empty
empty :: DocSt st
empty = return PP.empty


-- @text@ - applicative env passing PP.text
text :: String -> DocSt st
text = return . PP.text

-- @string@ - applicative env passing PP.string
string :: String -> DocSt st
string = return . PP.string

-- @int@ - applicative env passing PP.int
int :: Int -> DocSt st
int  = return . PP.int

-- @integer@ - applicative env passing PP.integer
integer :: Integer -> DocSt st
integer = return . PP.integer


-- @char@ - applicative env passing PP.char
char :: Char -> DocSt st
char = return . PP.char

eol :: DocSt st
eol = return PP.line


(<$>) :: DocSt st -> DocSt st -> DocSt st
(<$>) = caten (PP.<$>)

(<+>) :: DocSt st -> DocSt st -> DocSt st
(<+>) = caten (PP.<+>)
  
(<>) :: DocSt st -> DocSt st -> DocSt st  
(<>)  = caten (PP.<>)
  
  
colon :: DocSt st
colon = return PP.colon

line :: DocSt st
line = return PP.line


sprintf :: st -> DocSt st -> Doc
sprintf st p = evalState p st

pprender :: PP.Doc -> String
pprender doc = PP.displayS (PP.renderPretty 0.4 80 doc) ""


                                          