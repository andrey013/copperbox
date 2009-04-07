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

-- @caten@ is the general function for combining two /DocSt/ functions.
--  
caten :: (Doc -> Doc  -> Doc) ->
         DocSt st -> DocSt st -> DocSt st 
caten op f g = f >>= \a -> g >>= \b -> return $ a `op` b


-- @printState@ - print the current state when it is an instance of Show. 
printState :: Show st => DocSt st
printState = get >>= return . PP.string . show


-- @empty@ - DocSt version of PP.empty
empty :: DocSt st
empty = return PP.empty


-- @text@ - DocSt version of PP.text
text :: String -> DocSt st
text = return . PP.text

-- @string@ - DocSt version of PP.string
string :: String -> DocSt st
string = return . PP.string

-- @int@ - DocSt version of PP.int
int :: Int -> DocSt st
int  = return . PP.int

-- @integer@ - DocSt version of PP.integer
integer :: Integer -> DocSt st
integer = return . PP.integer


-- @char@ - DocSt version of PP.char
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
pprender doc = PP.displayS (PP.renderPretty 0.8 80 doc) ""


                                          