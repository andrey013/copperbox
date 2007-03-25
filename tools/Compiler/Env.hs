
module Compiler.Env 
  ( CompilerEnv (..)
  , default_env
  , all_targets   
  ) where

import Compiler.Language
import Compiler.Views.ViewBase 


data CompilerEnv = CompilerEnv
  { input_files         :: [FilePath]
  , output_directory    :: String
  , output_prefix       :: String
  , line_width          :: Int
  , targets             :: [Lang]
  , silent_mode         :: Bool
  , view_table          :: ViewTable
  }
  deriving (Show)

default_env :: CompilerEnv
default_env = CompilerEnv
  { input_files           = []
  , output_directory      = "."
  , output_prefix         = "output"
  , line_width            = 80
  , targets               = []
  , silent_mode           = False
  , view_table            = empty_view_table
  }
    

  
  
all_targets :: [Lang]
all_targets = [Asdl_1_2, OCaml , Uuag]

 