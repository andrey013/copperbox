{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Utils.Pretty
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Document representation and extra pretty print combinators.
--
--------------------------------------------------------------------------------

module Neume.Core.Utils.Pretty
  ( 

  -- * Extra pretty printers
    DocS
  , PPrint(..)
  , dshow
  , doclines
  , sepList
  , angles
  , dblangles
  , sglLine
  , doubleQuote
  , emptyDoc
  , spaceBraces
  , nestBraces
  , optDoc
  , mbDoc   
  , ppCommand 

  , writeDoc
  , printDoc
  , renderDocEighty
  , renderDocSixty

  ) where


import Text.PrettyPrint.HughesPJ

import Data.List ( foldl' )


---------------------------------------------------------------------------------
-- PPrint extras 



type DocS = Doc -> Doc


class PPrint a where
  pprint :: a -> Doc

dshow :: Show a => a -> Doc
dshow = text . show

-- This function is primarily for Abc bar printing, where the number
-- of bars on a line in the input score is reflected by the number of
-- bars on a line in the output.

doclines :: [Int] -> [Doc] -> Doc
doclines widths = vsep . step widths 
  where
    step _      []  = []
    step []     ds  = [hsep ds]
    step (n:ns) ds  = hsep ls : step ns rs where (ls,rs) = splitAt n ds
    
    vsep (d:ds) = foldl' ($+$) d ds
    vsep []     = empty

sepList :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
sepList op = step where
  step []     = empty
  step [x]    = x
  step (x:xs) = x `op` step xs


angles :: Doc -> Doc 
angles d = char '<' <> d <> char '>'


dblangles :: Doc -> Doc 
dblangles d = text "<<" <> d <> text ">>"



sglLine :: Doc -> Doc 
sglLine d = d $+$ empty


doubleQuote :: String -> Doc
doubleQuote = doubleQuotes . text

emptyDoc :: Doc
emptyDoc = empty

spaceBraces :: Doc -> Doc
spaceBraces d =  char '{' <+> d <+> char '}'


-- | Enclose expression within braces @{ ... }@. The open brace
-- is printed on the current line, then a line break, then the  
-- expression is printed with indent level two. The closing brace
-- is printed on a new line.
nestBraces :: Doc -> Doc
nestBraces e = lbrace $+$ nest 2 e $+$ rbrace 



optDoc :: Bool -> Doc -> Doc
optDoc b doc = if b then doc else empty

mbDoc ::  (a -> Doc) -> Maybe a -> Doc
mbDoc f o = maybe empty f o 

ppCommand :: String -> Doc
ppCommand = (char '\\' <>) . text 


writeDoc :: FilePath -> Doc -> IO ()
writeDoc filepath = writeFile filepath . renderDocEighty

printDoc :: Doc -> IO ()
printDoc = putStrLn . renderDocEighty

renderDocEighty :: Doc -> String
renderDocEighty = renderStyle (Style PageMode 80 0.8)

renderDocSixty :: Doc -> String
renderDocSixty = renderStyle (Style PageMode 80 0.8)


