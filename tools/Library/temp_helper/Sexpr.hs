
module Sexpr where

import AsdlBase

type Identifier = String


data Sexpr = Int Int
           | String String
           | Symbol Identifier
           | Cons Sexpr Sexpr
           | Nil
  deriving (Eq,Show)
