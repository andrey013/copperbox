{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ParsecScreener
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Extra \'control\' for Parsec, inspired by the concept of a
-- \'screener\' between lexical analysis and parsing.
-- 
--
--------------------------------------------------------------------------------


module Text.ParserCombinators.ParsecScreener
  (
    withinLine
  , withinLines
  , withIndent
  , advanceLines

  ) where

import Text.ParserCombinators.Parsec
-- import Text.ParserCombinators.Parsec.Pos

import Control.Monad
import Data.Char


withinLine :: GenParser Char st a -> GenParser Char st a
withinLine = genWithin lineSplit movePosOneLine
   

-- if i > num-lines-in-remaining-input parse will go ahead 
-- regardless...
--
withinLines :: Int -> GenParser Char st a -> GenParser Char st a
withinLines i = genWithin (lineSplitN i)  movePosOneLine

   
-- withIndent uses the indent level of the next line as the measure
--
withIndent :: GenParser Char st a -> GenParser Char st a
withIndent = genWithin (threeJoin . threeSplit) movePosOneLine



-- careful with file pos regarding eof errors...
advanceLines :: Int -> GenParser Char st a -> GenParser Char st a
advanceLines i p = liftM (lineSplitN i) getInput >>= \(_,rest) -> 
   if (null rest) 
     then setInput ""   >> modifyPos (movePosNLines i) >> p 
     else setInput rest >> modifyPos (movePosNLines i) >> p 

     -- Setting the input to "" will cause the parse to fail,
     -- but it will generate the user codes error message

--------------------------------------------------------------------------------

genWithin :: (String -> (String,String)) 
          -> (SourcePos -> SourcePos)
          -> GenParser Char st a 
          -> GenParser Char st a
genWithin splitter updPos p = liftM splitter getInput >>= \(front,rest) ->
    bracketM_ (setInput front) (setInput rest >> modifyPos updPos) p
   
lineSplit :: String -> (String,String)
lineSplit = fn . break (=='\n') where
   fn (xs,[])   = (xs,[])
   fn (xs,_:ys) = (xs,ys)               -- snd is always ('\n':ys)

  
lineSplitN :: Int -> String -> (String,String)
lineSplitN i str = unPair $ apo phi (i,str) where
  phi (_,[])             = Nothing
  phi (n,ss) | n <= 0    = Nothing
             | otherwise = let (l,ls) = lineSplit ss in Just (l, (n-1,ls))
  
  unPair (chops,(_,rest)) = (unlines chops,rest)


threeSplit :: String -> (String, Maybe (String,String)) 
threeSplit ss = let (front,rest) = lineSplit ss in
    case rest of 
      []  -> (front, Nothing)
      ss' -> (front, Just $ lineSplit ss')

threeJoin :: (String, Maybe (String,String)) -> (String,String)
threeJoin (front, Nothing)              = (front,"")
threeJoin (front, Just (line_two,rest)) = (unlines $ front:indents,final)
  where
    width           = indentLevel line_two
    (indents,final) = apo phi (unlines [line_two,rest])
 
    phi []          = Nothing
    phi str         = let (l,ls) = lineSplit str 
                          w      = indentLevel l
                      in if (blankLine l) || (w >= width)
                           then Just (l,ls)
                           else Nothing 


indentLevel :: String -> Int
indentLevel = step 0 where
  step i ('\t':xs)             = step (i+8) xs
  step i (x:xs)    | isSpace x = step (i+1) xs
  step i _                     = i

blankLine :: String -> Bool
blankLine []                 = True
blankLine (x:xs) | isSpace x = blankLine xs 
                 | otherwise = False


modifyPos :: (SourcePos -> SourcePos) -> GenParser tok st ()
modifyPos fn = getPosition >>= (setPosition . fn)

-- parsing inside a 'reified' region will have increased the 
-- position anyway, all that is needed here is to set the position 
-- to the start of the next line
--
movePosOneLine :: SourcePos -> SourcePos 
movePosOneLine = (incSourceLine `flip` 1) . (setSourceColumn `flip` 1)


movePosNLines :: Int -> SourcePos -> SourcePos 
movePosNLines i = (incSourceLine `flip` i) . (setSourceColumn `flip` 1)


--------------------------------------------------------------------------------
-- Utils


-- degenerate apomorphism - returns the state in a tuple, rather 
-- than flushes it.
--

apo :: (st -> Maybe (a,st)) -> st -> ([a],st)
apo phi st = case phi st of
               Just (a,st') -> a `cons` apo phi st'
               Nothing      -> ([],st)
  where cons a (as,b) = (a:as,b)

 
bracketM_ :: Monad m => m a -> m b -> m c -> m c
bracketM_ pre post mf = pre >> mf >>= \ans -> post >> return ans
