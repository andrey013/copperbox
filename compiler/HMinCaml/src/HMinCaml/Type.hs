{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Types
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Types.
--
--------------------------------------------------------------------------------

module HMinCaml.Type 
  (

    Type(..)

  , ppType

  ) where


import Text.PrettyPrint.HughesPJ

data Type = Unit
          | Bool
          | Int
          | Float
          | Fun [Type] Type
          | Tuple [Type]
          | Array Type
          | Var Int
          | TypeLoc Int
  deriving (Eq,Ord,Show)


-- Types should be printed with precedence so tuples
-- get printed properly
--
-- > ((1,2),3) -> (int * int) * int
--
-- > currently -> ((int * int) * int)


ppType :: Type -> Doc
ppType Unit           = text "unit"
ppType Bool           = text "bool"
ppType Int            = text "int"
ppType Float          = text "float"
ppType (Fun ts t)     = pre <+> ppType t
  where
    pre = hsep $ map (\x -> ppType x <+> text "->") ts

ppType (Tuple ts)     = parens (step ts)
  where
    step [x]    = ppType x
    step (x:xs) = ppType x <+> char '*' <+> step xs
    step []     = empty

ppType (Array t)      = ppType t <+> text "array"
 
ppType (Var i)        = char 'a' <> int i <> char '\''
ppType (TypeLoc i)    = char '<' <> text "loc:" <+> int i <> char '>'

