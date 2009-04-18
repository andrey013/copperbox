{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondOutput where

import Mullein.CoreTypes
import Mullein.Duration
import qualified Mullein.LilyPondSyntax as L
import Mullein.Pitch
import Mullein.RS
import Mullein.ScoreSyntax hiding ( Element )
import Mullein.Utils

import Control.Applicative hiding ( empty )
import Text.PrettyPrint.Leijen hiding ( (<$>) ) 
import qualified Text.PrettyPrint.Leijen as PP

data S = St { current_key :: Key }
data E = Env {}

type M a = RS S E a


class LilyPondElement e where
  outputLy :: e -> Doc

instance LilyPondElement L.Element where
  outputLy (L.Note p od)      = note p <> optDuration od
  outputLy (L.Rest od)        = char 'r' <> optDuration od
  outputLy (L.Spacer od)      = char 's' <> optDuration od
  outputLy (L.Chord _ _)      = text "Chord - TODO"
  outputLy (L.GraceNotes _)   = text "GraceNotes - TODO"

output :: LilyPondElement e => Key -> Part e -> Doc
output k a = evalRS (outputPart a) s0 e0 where
    s0 = St k
    e0 = Env 



outputPart :: LilyPondElement e => Part e -> M Doc
outputPart (Part as)          = vsep <$> mapM outputPhrase as

outputPhrase :: LilyPondElement e => Phrase e -> M Doc
outputPhrase (Phrase a)       = outputMotif a
outputPhrase (Repeated a)     = repeated <$> outputMotif a
outputPhrase (FSRepeat a x y) = fsrepeat <$> outputMotif a
                                         <*> outputMotif x
                                         <*> outputMotif y

outputMotif :: LilyPondElement e => Motif e -> M Doc
outputMotif (Motif k _ bs)    = fn <$> keyChange k <*> mapM outputBar bs
  where
    fn True  xs = text "%{ keychange %}" <+> (hsep $ punctuate (text " |") xs)
    fn _     xs = hsep $ punctuate (text " |") xs


outputBar :: LilyPondElement e => Bar e -> M Doc
outputBar (Bar a)             = outputUnison a
outputBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> outputUnison a 
                                  <*> mapM outputUnison as


outputUnison :: LilyPondElement e => Unison e -> M Doc
outputUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '~'
                                                           else empty)
                                  <$> mapM outputBracket ps

outputBracket :: LilyPondElement e => Bracket e -> M Doc
outputBracket (Singleton e)   = return $ outputLy e
outputBracket (Bracket es)    = return $ lyBeam $ map outputLy es




--------------------------------------------------------------------------------
-- helpers

-- ... COMMON ...

keyChange :: Key -> M Bool
keyChange new = do 
    old <- gets current_key 
    if (new==old) 
       then return False
       else do {modify $ \s -> s{current_key=new} ; return True } 



overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\")    


note :: Pitch -> Doc 
note (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty



-- lilypond middle c is c' 
-- HNotate middle c is c4
rescale :: Pitch -> Pitch
rescale (Pitch l a o)   = Pitch l a (o-3)

pitchLabel :: PitchLetter -> Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = empty
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"


optDuration :: Maybe Duration -> Doc
optDuration = maybe empty df where
    df 0   = empty
    df drn = let (n,d,dc) = pdElements $ augDuration drn
             in dots dc $ durn n d

    durn 4 1      = command "longa"
    durn 2 1      = command "breve"
    durn 1 i      = int i
    -- TODO - ideally we shouldn't have 'error' errors here, we should be
    -- using throwError. But that means making a lot of pure code monadic
    -- ... is there another way to do it?
    durn n d      = error $ "lyDuration failed on - " ++ show n ++ "%" ++ show d

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id


lyBeam :: [Doc] -> Doc
lyBeam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
lyBeam []     = empty

command :: String -> Doc
command = (char '\\' <>) . text 

repeated :: Doc -> Doc 
repeated d = command "repeat" <+> text "volta 2" <+> braces d

fsrepeat :: Doc -> Doc -> Doc -> Doc
fsrepeat a x y = 
    command "repeat" <+> text "volta 2" <+> (braces a)
        PP.<$> command "alternative" <+> braces ((braces x) <+> (braces y))
