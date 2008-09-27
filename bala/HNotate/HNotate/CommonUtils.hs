
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils - mainly for the phantom type wrappers over Doc.
--
--------------------------------------------------------------------------------

module HNotate.CommonUtils where

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence hiding (empty, length)
import System.IO
import Text.PrettyPrint.Leijen

unseq :: Seq a -> [a]
unseq = F.foldr (:) [] 


outputDoc :: FilePath -> Doc -> IO ()
outputDoc filepath doc = do
    h <- openFile filepath WriteMode
    displayIO h (renderPretty 0.7 80 doc)
    hClose h

putDoc80 :: Doc -> IO ()
putDoc80 doc = displayIO stdout (renderPretty 0.7 80 doc)

underline :: String -> Doc
underline s = text s <$> text (replicate (length s) '-') <> line 
    
    
sepSeq :: Pretty a => (Doc -> Doc -> Doc) -> Seq a -> Doc
sepSeq op sq = case viewl sq of
    EmptyL    -> empty
    e :< se   -> trav (pretty e) (viewl se)
  where
    trav doc EmptyL    = doc
    trav doc (e :< se) = trav (doc `op` pretty e) (viewl se)
    


