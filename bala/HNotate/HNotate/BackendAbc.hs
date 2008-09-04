{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.BackendAbc
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Emit Abc from Score representation.
--
--------------------------------------------------------------------------------


module HNotate.BackendAbc (
    translateAbc, AbcNoteList   
  ) where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.NoteListDatatypes
import HNotate.OutputUtils
import HNotate.Pitch
import HNotate.TextAbc
import HNotate.Traversals

import Control.Monad.Identity
import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence
import qualified Text.PrettyPrint.Leijen as PP

  
type AbcNoteList = AbcCxt_Body

type InterimGlyph     = Glyph AbcNote (Maybe AbcDuration)


type InterimNoteList  = ScNoteList InterimGlyph
type InterimBlock     = ScBlock InterimGlyph
type InterimMeasure   = ScMeasure InterimGlyph

translateAbc :: ScoreNoteList -> Duration -> AbcNoteList
translateAbc notes default_duration =
    outputNoteList $ abcForm notes default_duration

    
abcForm :: ScoreNoteList -> Duration -> InterimNoteList
abcForm se default_duration = fn se  
  where
    fn se = let s'  = translateDuration se default_duration
                s'' = traversalIdentity abc_note_Body s'
            in s''

translateDuration :: ScNoteList (Glyph pch Duration)
                  -> Duration
                  -> ScNoteList (Glyph pch (Maybe AbcDuration))
translateDuration strata default_duration = 
    traversalEnv default_encode_Abc strata default_duration 

default_encode_Abc :: Glyph p Duration 
                   -> Duration 
                   -> Glyph p (Maybe AbcDuration)
default_encode_Abc e = \env -> 
    let drn = glyphDuration e
        od  = abcRelativeDuration env drn
    in changeDuration e od
    
    
abc_note_Body :: Glyph Pitch d -> Identity (Glyph AbcNote d)
abc_note_Body e@(GlyNote p _)     = return $ changePitch e (abcNote p)

abc_note_Body (GlyRest d)         = return $ GlyRest d
             
abc_note_Body (GlySpacer d)       = return $ GlySpacer d

abc_note_Body (GlyChord se d)     = 
    let se' = F.foldl (\a e -> a |> abcNote e) mempty se 
    in return $ GlyChord se' d
    
abc_note_Body (GlyGraceNotes se)  = 
    let se' = F.foldl (\a (p,d) -> a |> (abcNote p,d)) mempty se
    in return $ GlyGraceNotes se' 


outputNoteList :: InterimNoteList -> AbcNoteList
outputNoteList (ScNoteList se) = F.foldl outputBlock body se

outputBlock :: AbcNoteList -> InterimBlock -> AbcNoteList
outputBlock cxt (ScSingleBlock i se) = 
    (outputMeasure cxt se) +++ barline

outputBlock cxt (ScPolyBlock i se) = 
    let voices = F.foldr (\e a -> outputMeasure body e : a) [] se
    in voiceOverlay cxt voices 

outputMeasure :: AbcNoteList -> InterimMeasure -> AbcNoteList
outputMeasure cxt (ScMeasure se) = F.foldl outputGlyph cxt se

outputGlyph :: AbcNoteList -> InterimGlyph -> AbcNoteList
outputGlyph cxt (GlyNote n od)      = cxt +++ (n *! od)
    
outputGlyph cxt (GlyRest od)        = cxt +++ (rest *! od) 
    
outputGlyph cxt (GlySpacer od)      = cxt +++ (spacer *! od)
    
outputGlyph cxt (GlyChord se od)    = cxt +++ mkChord od se
  where
    mkChord od = chord . F.foldr (\n a -> (n *! od) : a) []
    
outputGlyph cxt (GlyGraceNotes se)  = cxt +++ mkGrace se
  where 
    mkGrace = gracenotes . F.foldr (\(n,od) a -> (n *! od) : a) []

voiceOverlay :: AbcCxt_Body -> [AbcCxt_Body] -> AbcCxt_Body
voiceOverlay cxt []     = cxt
voiceOverlay cxt [v]    = cxt `mappend` v +++ barline
voiceOverlay cxt (v:vs) = voiceOverlay (cxt &\ v) vs 


    
instance PP.Pretty AbcDuration where
  pretty = getAbc

