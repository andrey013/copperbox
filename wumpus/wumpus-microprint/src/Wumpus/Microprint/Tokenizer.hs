{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Microprint.Tokenizer
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple tokenizing builder.
--
--------------------------------------------------------------------------------

module Wumpus.Microprint.Tokenizer
  ( 
    TokenizerConfig(..)
  , haskellTokenizer
  , runTokenizer 

  ) where


import Wumpus.Microprint.Datatypes

import Wumpus.Basic.Utils.HList			-- package: wumpus-basic
import Wumpus.Core				-- package: wumpus-core

import Control.Applicative
import Control.Monad
import Data.Char ( isSpace )
import Data.List

data TokenizerConfig = TokenizerConfig
      { standard_colour		:: RGBi
      , sgl_comment_start 	:: String   -- note - can be a prefix of a word 
      , comment_start	  	:: String
      , comment_end		:: String
      , comment_colour		:: RGBi
      }


haskellTokenizer :: RGBi -> RGBi -> TokenizerConfig
haskellTokenizer std_rgb comment_rgb = TokenizerConfig 
      { standard_colour		= std_rgb
      , sgl_comment_start 	= "--"  
      , comment_start	  	= "{-"
      , comment_end		= "-}"
      , comment_colour		= comment_rgb
      }
 

data TokState = CommentML | CommentSL | Normal
  deriving (Eq,Ord,Show)

data St = St TokState (H Tile)

newtype Lexer a  = Lexer { getLexer :: TokenizerConfig -> St -> (a,St) }

instance Functor Lexer where
  fmap f m = Lexer $ \r s -> let (a,s1) = getLexer m r s in (f a, s1)

instance Applicative Lexer where
  pure a    = Lexer $ \_ s -> (a,s)
  mf <*> ma = Lexer $ \r s -> let (f,s1) = getLexer mf r s 
     	      	      	    	  (a,s2) = getLexer ma r s1
			      in (f a, s2)

instance Monad Lexer where
  return a = Lexer $ \_ s -> (a,s)
  m >>= k  = Lexer $ \r s -> let (a,s1) = getLexer m r s
    	     	     	      in (getLexer . k) a r s1


tellSpaces :: Int -> Lexer ()
tellSpaces i = Lexer $ \_ (St ts ac) -> 
    let ac1 = snocH ac (Space i) in ((), St ts ac1) 


tellChars :: Int -> RGBi -> Lexer ()
tellChars i rgb = Lexer $ \_ (St ts ac) -> 
    let ac1 = snocH ac (Word rgb i) in ((), St ts ac1)

askColour :: Lexer RGBi
askColour = Lexer $ \r s@(St ts _) -> 
    case ts of
      Normal -> (standard_colour r, s)
      _      -> (comment_colour r,  s)


asksTC :: (TokenizerConfig -> a) -> Lexer a
asksTC fn = Lexer $ \r s -> (fn r, s) 

setTokState :: TokState -> Lexer ()
setTokState st = Lexer $ \_ (St _ ac) -> ((),St st ac)  

getTokState :: Lexer TokState 
getTokState = Lexer $ \_ s@(St st _) -> (st,s)

runTokenizer :: TokenizerConfig -> String -> GreekText
runTokenizer cfg input = step Normal $ lines input
  where
    step _  []      = (0,[])
    step st (s:ss)  = let (st1,l1) = lexLine cfg st s
    	 	   	  (h,rest) = step st1 ss
                      in (h+1,l1:rest) 

lexLine :: TokenizerConfig -> TokState -> String -> (TokState,[Tile])
lexLine cfg st ss = 
    let (_,St st1 hf) = runLexer cfg st ss in (st1, toListH hf) 
  

runLexer :: TokenizerConfig -> TokState -> String -> ((),St)
runLexer cfg ts ss = getLexer (lexer ss) cfg (St (normalize ts) emptyH) 
  where
    normalize CommentSL = Normal
    normalize a         = a

lexer :: String -> Lexer ()
lexer (' ':xs)      = spaces 1 xs
lexer ('\t':xs)     = spaces 8 xs
lexer xs            = word xs


spaces :: Int -> String -> Lexer ()
spaces n (' ':xs)   = spaces (n+1) xs
spaces n ('\t':xs)  = spaces (n+8) xs
spaces n xs         = tellSpaces n >> word xs

word :: String -> Lexer ()
word []	= return ()
word xs	= let (pre,rest) = break isSpace xs in do
    st <- getTokState
    when (st==Normal)  (testPrefix pre)
    rgb <- askColour
    tellChars (length pre) rgb
    when (st==CommentML) (testSuffix pre)
    spaces 0 rest

testPrefix :: String -> Lexer ()
testPrefix ss = 
    asksTC sgl_comment_start >>= \a -> 
    if isPrefixOf a ss then setTokState CommentSL
      else asksTC comment_start >>= \b ->
           if isPrefixOf b ss then setTokState CommentML
	      		      else return ()

testSuffix :: String -> Lexer ()
testSuffix ss = 
    asksTC comment_end >>= \a ->
    if isSuffixOf a ss then setTokState Normal
                       else return ()
      