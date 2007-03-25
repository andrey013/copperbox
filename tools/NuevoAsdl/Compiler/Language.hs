
module Compiler.Language
  ( Language(..)
  , Lang(..)      -- to remove
--  , ocaml
  , Commenter
  , commentOCaml
  , commentUuag
  , commentAsdl
  , commentHaskell
  )where



-- how necessary???
data Language = Language
  { comment :: Commenter
  , fileSuffix :: String
  }
  
ocaml = Language 
  { comment = comment2 ("(* ", " *)")
  , fileSuffix = ".ml" 
  }

  
  

data Lang = AnyLang
          | NoLang
          | Asdl_1_2 
          | Html 
          | OCaml
          | Uuag
          | Haskell
          deriving (Eq,Ord,Show)
              
              
type Commenter = String -> String


commentOCaml, commentUuag, commentAsdl :: Commenter
commentOCaml      = comment2 ("(* ", " *)")
commentUuag       = comment2 ("-- ", "")
commentAsdl       = comment2 ("-- ", "")
commentHaskell    = comment2 ("-- ", "")



comment2 (start,stop) s = concat $ foldr fn [] (lines s)
  where fn e = (comment' e :)
        comment' s = start ++ s ++ stop ++ "\n"
        

        

