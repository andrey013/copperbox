-- Datatypes for Unified Diff format

module Text.Diff.Unified.Datatypes where


-- import System.Time




type SrcFileName = String

data UdFile = UdFile UdFileHeader [UdHunk]
  deriving (Show)

data UdFileHeader = UdFileHeader 
  { left_name :: SrcFileName
  , left_time :: TimeStamp
  , right_name :: SrcFileName 
  , right_time :: TimeStamp
  }
  deriving Show

type Range = (Int,Int)

data UdHunk = UdHunk UdHunkHeader [UdLine]
  deriving Show
  
data UdHunkHeader = UdHunkHeader Range Range
  deriving Show


type Line = String

data UdLine
  = Added   Line            -- '+' prefix
  | Removed  Line           -- '-' prefix 
  | Common    Line          -- {space} prefix
  | Incomplete              -- \ No newline at end of file
  deriving Show  
  
data TimeStamp = TimeStamp
  { udYear :: Int 
  , udMonth :: Int
  , udDay :: Int  
  , udHour :: Int
  , udMin :: Int
  , udSec :: Int
  , udSecFrac :: Integer
  , udTimeZone :: Int
  }
  deriving Show 

