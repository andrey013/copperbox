--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Diff.DiffKit.Datatypes
-- Copyright   :  (c) Stephen Tetley 2007
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  portable
--
-- Datatypes for Unified Diff format

--------------------------------------------------------------------------------


module Text.Diff.DiffKit.Datatypes where


import System.Time

data DiffFormat = Normal | Context | Unified
  deriving (Eq,Show)


data DiffFile = DiffFile {
    opt_header :: Maybe FileHeader,
    body       :: [Hunk]
  }
  deriving (Show)
  
data FileHeader = FileHeader {
    from_file     :: FileStats,
    to_file       :: FileStats
  }
  deriving (Show)  

type SourceFile = String

data FileStats = FileStats {
    source_name :: SourceFile,
    timestamp   :: TimeStamp
  }
  deriving (Show)
  


type Range = (Int,Int)

data Hunk = Hunk Header [HunkLine]
  deriving Show
  
data Header = UdHunkHeader Range Range
  deriving Show


type Line = String

-- a changed line in a unified file is removed then added
data HunkLine
  = Added     Line          -- '+' prefix
  | Removed   Line          -- '-' prefix 
  | Changed   Line
  | Common    Line          -- {space} prefix
  | Incomplete              -- \ No newline at end of file
  deriving Show  
  
data TimeStamp = InterpretedTime CalendarTime
               | StringTime String
  deriving Show 


  