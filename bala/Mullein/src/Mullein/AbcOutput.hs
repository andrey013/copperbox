{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcOutput where

import Mullein.CoreTypes
import Mullein.Duration
import qualified Mullein.AbcSyntax as A
import Mullein.Pitch
import Mullein.RS
import Mullein.ScoreSyntax hiding ( Element )

import Control.Applicative hiding ( empty )
import Data.Ratio
import Text.PrettyPrint.Leijen hiding ( (<$>) )

data S = St { current_key :: Key }
data E = Env {}

type M a = RS S E a

class AbcElement e where
  outputAbc :: e -> Doc

instance AbcElement A.Element where
  outputAbc (A.Note p dm)      = note p dm
  outputAbc (A.Rest dm)        = char 'z' <> multiplier dm
  outputAbc (A.Spacer dm)      = char 'x' <> multiplier dm
  outputAbc (A.Chord _ _)      = text "Chord - TODO"
  outputAbc (A.GraceNotes _)   = text "GraceNotes - TODO"

output :: AbcElement e => Key -> Part e -> Doc
output k a = evalRS (outputPart a) s0 e0 where
    s0 = St k
    e0 = Env 


outputPart :: AbcElement e => Part e -> M Doc
outputPart (Part as)          = vsep <$> mapM outputPhrase as

outputPhrase :: AbcElement e => Phrase e -> M Doc
outputPhrase (Phrase a)       = outputMotif a
outputPhrase (Repeated a)     = repeated <$> outputMotif a
outputPhrase (FSRepeat a x y) = fsrepeat <$> outputMotif a
                                         <*> outputMotif x
                                         <*> outputMotif y
                                         
outputMotif :: AbcElement e => Motif e -> M Doc
outputMotif (Motif k _ bs)    = fn <$> keyChange k <*> mapM outputBar bs
  where
    fn True  xs = text "%keychange" <+> (hsep $ punctuate (text " |") xs)
    fn _     xs = hsep $ punctuate (text " |") xs

outputBar :: AbcElement e => Bar e -> M Doc
outputBar (Bar a)             = outputUnison a
outputBar (Overlay a as)      = (\x xs -> overlay $  x:xs) 
                                  <$> outputUnison a 
                                  <*> mapM outputUnison as


outputUnison :: AbcElement e => Unison e -> M Doc
outputUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '-'
                                                           else empty)
                                  <$> mapM outputBracket ps

outputBracket :: AbcElement e => Bracket e -> M Doc
outputBracket (Singleton e)   = pure $ outputAbc e
outputBracket (Bracket es)    = pure $ hcat $ map outputAbc es




--------------------------------------------------------------------------------
-- helpers

keyChange :: Key -> M Bool
keyChange new = do 
    old <- gets current_key 
    if (new==old) 
       then return False
       else do {modify $ \s -> s{current_key=new} ; return True } 

overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

note :: Pitch -> A.Multiplier -> Doc 
note p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)
  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 4     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 5       = text (replicate (i-5) '\'') 
              | i < 4       = text (replicate (4-i) ',')
              | otherwise   = empty


pitchLabel :: PitchLabel -> PitchChar -> Doc
pitchLabel (PitchLabel l a) pc 
    | pc == LOWER   = accidental a <> (char . toLowerLChar) l
    | otherwise     = accidental a <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = empty    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"
    
multiplier :: Duration -> Doc
multiplier dn | dn == 1   = empty
              | otherwise = fn (numerator dn, denominator dn)
  where
    fn (n,1) = integer n
    fn (1,d) = char '/' <> integer d
    fn (n,d) = integer n <> char '/' <> integer d


repeated :: Doc -> Doc
repeated d = text "|:" <+> d <+> text ":|"     

fsrepeat :: Doc -> Doc -> Doc -> Doc
fsrepeat d x y = 
    text "|:" <+> d <+> text "|[1"  <+> x <+> text ":|[2" <+> y <+> text "|]"
