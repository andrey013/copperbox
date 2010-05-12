{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.TextOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Print ChangeStats to the console.
--
--------------------------------------------------------------------------------


module Precis.TextOutput
  ( 
    showChangeStats
  , comparingMsg
  ) where

import Precis.Datatypes
import Precis.PPShowS
import Precis.ReportMonad

import Data.Maybe

showChangeStats :: ChangeStats -> ShowS
showChangeStats = vsep . catMaybes . sequence funs 
  where
    funs = [ unparseableModules, removedModules
           , removedExports,     changedExports
           , removedDatatypes,   changedDatatypes 
           , removedTypeSigs,    changedTypeSigs
           , removedInstances,   changedInstances
           ]


unparseableModules :: ChangeStats -> Maybe ShowS
unparseableModules = step . unparseable_modules 
  where
   step []     = Nothing 
   step xs     = Just $ prolog $ vsep $ map fn xs

   fn (NEW s)  = space <> text s <+> parens (text "new")
   fn (OLD s)  = space <> text s <+> parens (text "old")

   prolog k    = text "The following modules could not be parsed: " `line` k


removedModules :: ChangeStats -> Maybe ShowS
removedModules = 
    countMsg "removed" "exposed module" "exposed modules" . removed_modules

removedExports :: ChangeStats -> Maybe ShowS
removedExports = 
    countMsg "removed" "export list item" "export list items" . removed_exports

changedExports :: ChangeStats -> Maybe ShowS
changedExports = 
    countMsg "changed" "export" "exports" . changed_exports


removedDatatypes :: ChangeStats -> Maybe ShowS
removedDatatypes = countMsg "removed" "datatype" "datatypes" . removed_datatypes

changedDatatypes :: ChangeStats -> Maybe ShowS
changedDatatypes = 
    countMsg "changed" "datatype" "datatypes" . changed_datatypes



removedTypeSigs :: ChangeStats -> Maybe ShowS
removedTypeSigs = 
    countMsg "removed" "type signature" "type signatures" . removed_typesigs

changedTypeSigs :: ChangeStats -> Maybe ShowS
changedTypeSigs = 
    countMsg "changed" "type signature" "type signatures" . changed_typesigs


removedInstances :: ChangeStats -> Maybe ShowS
removedInstances = 
    countMsg "removed" "class instance" "class instances" . removed_instances

changedInstances :: ChangeStats -> Maybe ShowS
changedInstances = 
    countMsg "changed" "class instance" "class instances" . changed_instances


countMsg :: String -> String -> String -> Int -> Maybe ShowS
countMsg act single plural n 
    | n <= 0    = Nothing
    | n == 1    = Just $ int 1 <+> text single <+> text act 
    | otherwise = Just $ int n <+> text plural <+> text act 


--------------------------------------------------------------------------------

comparingMsg :: CabalPrecis -> CabalPrecis -> ShowS
comparingMsg new old = suffixEllipses $ hsep $ map text 
    [ "Comparing", package_name new, package_version new
    , "to",        package_name old, package_version old
    ]
  where
    suffixEllipses = (<> text "...")
