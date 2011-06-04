{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- CsoundScore
--
--------------------------------------------------------------------------------

module ZScore.CsoundScore
  (

    CsoundType(..)

  , Section(..)
  
  -- * Statements
  , InstStmt(..)
  , TableStmt(..)
  , DummyStmt(..)
  , SectionStmt(..)
  , EndStmt(..)
   
  -- * Definitions
  , PFieldName
  , InstDefn(..)
 
  ) where

import ZScore.Utils.FormatCombinators

data CsoundType = CsDouble Double
                | CsInt    Int
                | CsString String
  deriving (Eq,Ord,Show)


data Section = Section
      { section_istmts   :: [InstStmt]
      }



data InstStmt = InstStmt
      { inst_num        :: Int
      , inst_start      :: Double
      , inst_dur        :: Double
      , inst_pfields    :: [Double]
      }
  deriving (Eq,Ord,Show)


data TableStmt = TableStmt
      { table_num       :: Int
      , table_atime     :: Double
      , table_size      :: Int
      , gen_routine     :: Int
      , table_args      :: [CsoundType]
      }
  deriving (Eq,Ord,Show)


-- | This is "f0"
--
data DummyStmt = DummyStmt
       { ds_atime       :: Double }
  deriving (Eq,Ord,Show)


data SectionStmt = SectionStmt
  deriving (Eq,Ord,Show)

data EndStmt = EndStmt
  deriving (Eq,Ord,Show)

type PFieldName = String

newtype InstDefn = InstDefn { getInstDefn :: [PFieldName] }
  deriving (Eq,Ord,Show)


--------------------------------------------------------------------------------
-- Format instances

instance Format CsoundType where
  format (CsDouble d) = dtrunc d
  format (CsInt i)    = int i
  format (CsString s) = dquotes $ text s
  

instance Format Section where
  format (Section is) = vcat (map format is) `vconcat` char 's'

instance Format InstStmt where
  format (InstStmt i s d xs) = 
      char 'i' <+> padr 6 (int i) <+> padl 10 (dtrunc s) <+> padl 10 (dtrunc d)
               <+> hsep (map (padl 10 . dtrunc) xs)


instance Format TableStmt where
  format (TableStmt i s sz n xs) = 
      char 'f' <+> padr 6 (int i) <+> padl 10 (dtrunc s) <+> padl 6 (int sz)
               <+> padl 6 (int n) <+> hsep (map (padl 4 . format) xs)

instance Format DummyStmt where
  format (DummyStmt s) = text "f0" <+> padl 10 (dtrunc s)

instance Format SectionStmt where
  format _ = char 's'

instance Format EndStmt where
  format _ = char 'e'



instance Format InstDefn where
  format (InstDefn xs) = 
      char ';' <+> padr 6  (text "ins")  <+> padl 10 (text "st")
               <+> padl 10 (text "dur")  <+> hsep (map (padl 10 . text) xs)
