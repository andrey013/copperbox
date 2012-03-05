{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.Id
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Identifiers.
--
--------------------------------------------------------------------------------

module HMinCaml.Id 
  ( 
    IdS
  , IdL

  , idOfType

  ) where


import qualified HMinCaml.Type as T


type IdS = String

-- | Labels 
--
newtype IdL = IdL String
  deriving (Eq,Ord,Show)


idOfType :: T.Type -> Maybe String
idOfType T.Unit         = Just "u"
idOfType T.Bool         = Just "b"
idOfType T.Int          = Just "i"
idOfType T.Float        = Just "d"
idOfType (T.Fun {})     = Just "f"
idOfType (T.Tuple {})   = Just "t"
idOfType (T.Array {})   = Just "a" 
idOfType (T.Var {})     = Nothing
idOfType (T.TypeLoc {}) = Nothing

