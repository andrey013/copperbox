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

import Control.Applicative
import Data.Generics
import Data.Typeable


  
extract :: Typeable a => a -> [WrappedAnno] -> a
extract a []     = error $ "missing annontation for " ++ tyname where
    tyname = (showsTypeRep `flip` "") $ typeOf a
extract a (WrapAnno x:xs) = case cast x of
                              Just b -> b
                              Nothing -> extract a xs

extractMaybe :: Typeable a => a -> [WrappedAnno] -> Maybe a
extractMaybe _ []      = Nothing
extractMaybe a (WrapAnno x:xs)   = case cast x of
                                     Just b  -> Just b
                                     Nothing -> extractMaybe a xs

suffix :: ODoc -> (ODoc -> ODoc)
suffix d = (<> d)

prefix :: ODoc -> (ODoc -> ODoc)
prefix d = (d <>)


type AnnoF = ((->) [WrappedAnno])

anno :: AnnoF ODocS -> [WrappedAnno] -> ODoc -> ODoc
anno fn a d = (fn a) d


--------------------------------------------------------------------------------
-- LilyPond annotations 

data Staccato = Staccato
  deriving (Data,Eq,Show,Typeable)

-- this is the LilyPond version  
staccato :: ODoc
staccato = string "-."


data StringNumber = StringNumber Int 
  deriving (Data,Eq,Show,Typeable)



stringNumber :: Int -> AnnotationS
stringNumber i = (:) (WrapAnno $ StringNumber i) 


lyStringNumber :: StringNumber -> ODoc
lyStringNumber (StringNumber i) = char '\\' <> int i


evalStringNumAnno :: AnnoEval
evalStringNumAnno = AnnoEval (\_ _ -> id) strNumE strNumA 
  where
    strNumA :: HnAtom -> Annotation -> ODocS
    strNumA HnNote a = anno annoStringNum a
    strNumA _      _ = id
    
    strNumE :: HnElement -> Annotation -> ODocS 
    strNumE HnChord a       = anno annoStringNum a
    strNumE HnGraceNotes a  = anno annoStringNum a
    strNumE HnNplet a       = anno annoStringNum a



annoStringNum :: AnnoF (ODoc -> ODoc)
annoStringNum = (\s -> (<> lyStringNumber s))
    <$> extract (undefined::StringNumber) 
    



data Bowing = UpBow | DownBow
  deriving (Data,Eq,Show,Typeable)

bowing :: Bowing -> ODoc  
bowing UpBow    = text "\\upbow"
bowing DownBow  = text "\\downbow"
                        
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