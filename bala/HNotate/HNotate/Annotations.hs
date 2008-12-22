{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Annotations
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common annotations for LilyPond and Abc 
--
--------------------------------------------------------------------------------

module HNotate.Annotations where

import HNotate.Document
import HNotate.NoteListDatatypes

import Data.Generics
import Data.Typeable


  
extract :: Typeable a => a -> [WrappedAnno] -> a
extract a []     = error $ "missing annontation for " ++ tyname where
    tyname = (showsTypeRep `flip` "") $ typeOf a
extract a (WrapAnno x:xs) = case cast x of
                              Just b -> b
                              Nothing -> extract a xs

extractMaybe :: Typeable a => a -> [WrappedAnno] -> Maybe a
extractMaybe a []      = Nothing
extractMaybe a (WrapAnno x:xs)   = case cast x of
                                     Just b  -> Just b
                                     Nothing -> extractMaybe a xs

suffix :: ODoc -> (ODoc -> ODoc)
suffix d = (<> d)

prefix :: ODoc -> (ODoc -> ODoc)
prefix d = (d <>)

--------------------------------------------------------------------------------
--

data Staccato = Staccato
  deriving (Data,Eq,Show,Typeable)

-- this is the LilyPond version  
staccato :: ODoc
staccato = string "-."
                        
{-                        
-- Abc
upbow :: Annotation
upbow = abcOnly (prefix $ char 'u')

downbow :: Annotation
downbow = abcOnly (prefix $ char 'v')



-- LilyPond
fermata :: Annotation 
fermata = lyOnly (suffix $ command "fermata")


stringNum :: Int -> Annotation
stringNum i = lyOnly(suffix $ command (show i))


-}