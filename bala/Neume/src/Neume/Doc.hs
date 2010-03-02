{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Doc
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

module Neume.Doc
  ( 

  -- * Extra pretty printers
    doclines
  , sepList
  , dblangles
  , nextLine
  , sglLine
  , doubleQuote
  , emptyDoc
  , spaceBraces
  , optDoc
  , mbDoc   
  , ppCommand 

  , writeDoc
  , renderDocEighty

  ) where

import Neume.Utils

import Text.PrettyPrint.Leijen          -- package: wl-pprint




---------------------------------------------------------------------------------
-- PPrint extras 

-- This function is primarily for Abc bar printing, where the number
-- of bars on a line in the input score is reflected by the number of
-- bars on a line in the output.

doclines :: [Int] -> [Doc] -> Doc
doclines = vsep `oo` step where
    step _      []  = []
    step []     ds  = [hsep ds]
    step (n:ns) ds  = hsep ls : step ns rs where (ls,rs) = splitAt n ds


sepList :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
sepList op = step where
  step []     = empty
  step [x]    = x
  step (x:xs) = x `op` step xs


dblangles :: Doc -> Doc 
dblangles = enclose (text "<< ") (text " >>")


-- an alternative to (<$>) when Control.Applicative is alos imported
infixr 5 `nextLine`
nextLine :: Doc -> Doc -> Doc 
nextLine = (<$>)

sglLine :: Doc -> Doc 
sglLine d = d <> line


doubleQuote :: String -> Doc
doubleQuote = dquotes . string

emptyDoc :: Doc
emptyDoc = empty

spaceBraces :: Doc -> Doc
spaceBraces = enclose (text "{ ") (text " }")

optDoc :: Bool -> Doc -> Doc
optDoc b doc = if b then doc else empty

mbDoc ::  (a -> Doc) -> Maybe a -> Doc
mbDoc f o = maybe empty f o 

ppCommand :: String -> Doc
ppCommand = (char '\\' <>) . text 


writeDoc :: FilePath -> Doc -> IO ()
writeDoc filepath = writeFile filepath . renderDocEighty

renderDocEighty :: Doc -> String
renderDocEighty = (displayS `flip` []) . renderPretty 0.8 80
