{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.Score
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Score data type - a header of table defs then a note list.
--
--------------------------------------------------------------------------------

module Majalan.Core.Score
  (

    Score(..)
  , GenStmt(..)

  , buildScoreDoc
 
  ) where

import Majalan.Core.NoteList
import Majalan.Core.Utils.DocExtras

import Text.PrettyPrint.HughesPJ 

data Score = Score { tables :: [GenStmt], note_list :: [CsEvent] }

data GenStmt = GenStmt
       { table_assign_num     :: Int  -- corresponds to table-ref in orch
       , table_size           :: Int
       , table_genroutine     :: Int
       , table_args           :: [CsValue]
       }
  deriving (Eq,Ord,Show)


buildScoreDoc :: ColumnSpecs -> Score -> Doc
buildScoreDoc cols (Score ts ns) = tables $+$ notes
  where
    tables = vconcat $ map fmtGenStmt ts
    notes  = printEvents ns cols

fmtGenStmt :: GenStmt -> Doc
fmtGenStmt stmt = 
      char 'f' <+> paddedTextR (show $ table_assign_num stmt) 5
               <+> int 0
               <+> int (table_size stmt)
               <+> int (table_genroutine stmt)
               <+> hsep (map valueZ $ table_args stmt)

