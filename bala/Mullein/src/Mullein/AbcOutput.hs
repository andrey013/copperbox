{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print Abc
--
--------------------------------------------------------------------------------


module Mullein.AbcOutput where

import Mullein.AbcNoteClass
import Mullein.CoreTypes
import Mullein.Duration
import Mullein.OutputCommon
import Mullein.Pitch
import Mullein.Utils

import Control.Applicative hiding ( empty )
import Control.Monad.State
import Data.Foldable ( foldlM, toList )
import Data.Sequence ( (<|), (><) )
import qualified Data.Sequence as S
import Data.Ratio
import Text.PrettyPrint.Leijen hiding ( (<$>) )



newtype AbcOutput = AbcOutput { getAbcOutput :: Doc }


generateAbc :: AbcNote e => Key -> Meter -> [Int] -> PartP e -> AbcOutput
generateAbc k m ns a = AbcOutput $ postProcess ns $ evalState (oPart a) s0 where
    s0 = St { current_key = k, current_meter = m } 


oPart :: AbcNote e => PartP e -> M (S.Seq OutputFragment)
oPart (Part as)          =
    foldlM (\a e -> (a ><) <$> oPhrase e) S.empty as

oPhrase :: AbcNote e => PhraseP e -> M (S.Seq OutputFragment)
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a  <*> oMotif x <*> oMotif y
                                         
oMotif :: AbcNote e => MotifP e -> M (S.Seq OutputFragment)
oMotif (Motif k m bs)    = motifFragment 
    <$> keyChange k keyField <*> meterChange m meterField <*> mapM oBar bs

oBar :: AbcNote e => BarP e -> M Doc
oBar (Bar a)             = oUnison a
oBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> oUnison a 
                                  <*> mapM oUnison as


oUnison :: AbcNote e => UnisonP e -> M Doc
oUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '-'
                                                           else empty)
                                  <$> mapM oBracket ps

oBracket :: AbcNote e => BracketP e -> M Doc
oBracket (Singleton e)   = pure $ oElement e
oBracket (Bracket es)    = pure $ hcat $ map oElement es



oElement :: AbcNote e => ElementP e -> Doc
oElement (Note p dm)      = abcNote p dm
oElement (Rest dm)        = char 'z' <> multiplier dm
oElement (Spacer dm)      = char 'x' <> multiplier dm
oElement (Chord ps dm)    = brackets $ hcat $ map f ps where
                              f p = abcPitch p <> multiplier dm 
oElement (GraceNotes xs)  = braces $ hcat $ map f xs where
                              f (p,dm) = abcPitch p <> multiplier dm


--------------------------------------------------------------------------------
-- Post process

-- ABC renders to PostScript the same number of bars per line as per the 
-- original .abc file - essentially .abc files are whitespace sensitive. 
-- When generating ABC we must do extra work to accommodate this.



postProcess :: [Int] -> S.Seq OutputFragment -> Doc
postProcess ns = 
    fn ns . intersperseBars . toList . addDblEnd . dropRepStart
  where
    fn _      []              = empty
    fn []     xs              = printFrags xs
    fn (i:is) xs              = printFrags ls `nextLine` fn is xs' where
                                    (ls,xs') = fragSplitAt i xs



fragSplitAt :: Int -> [OutputFragment] -> ([OutputFragment], [OutputFragment])
fragSplitAt i frags = extract $ anaSt phi (i,frags) 
  where
    phi (_, [])                 = Nothing
    phi (n, (BarOutput d : xs))
            | n >  0            = Just (BarOutput d, (n-1,xs))
            | otherwise         = Nothing

    phi (n, (RepStart : xs))
            | n >  0            = Just (RepStart, (n, xs)) -- take prefix (don't decr)
            | otherwise         = Nothing

    -- likewise for RepEnding which is also a prefix
    phi (n, (RepEnding z : xs))
            | n >  0            = Just (RepEnding z, (n, xs))
            | otherwise         = Nothing

   -- can always take a suffix or midtune field
    phi (n, (x:xs))             = Just (x, (n, xs))

    extract (xs,(_,vw))         = (xs,vw)

printFrags :: [OutputFragment] -> Doc
printFrags  = genUnfold phi (<>) empty where
    phi []                    = Nothing
    phi (MidtuneCmd d : xs)   = Just (linecont `nextLine` d 
                                               `nextLine` empty, xs)
    phi (BarOutput d : xs)    = Just (d, xs)
    phi (RepStart : xs)       = Just (text "|:", xs)
    phi (RepEnd : xs)         = Just (text ":|", xs)
    phi (RepEnding n : xs) 
                | n == 1      = Just (text "|[1", xs)
                | otherwise   = Just (text ":|[" <> int n, xs)  -- note, extra colon
    phi (SglBar : xs)         = Just (char '|', xs)
    phi (DblBar : xs)         = Just (text "||", xs)

    linecont = char '\\'




  

--------------------------------------------------------------------------------
-- helpers


infixr 5 `mbCons`
mbCons :: Maybe a -> S.Seq a -> S.Seq a
mbCons Nothing  = id
mbCons (Just a) = (a <|) 



keyField :: Key -> Doc
keyField (Key k m) = field 'K' keyspec where
    keyspec        = pitchLabel k UPPER <> modeSpec m
    -- Don't print \maj\ here
    modeSpec Major = empty
    modeSpec x     = mode x 


meterField :: Meter -> Doc
meterField = field 'M' . f where
    f (TimeSig n d) = integer n <> char '/' <> integer d
    f CommonTime    = text "C"
    f CutTime       = text "C|"


field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d





overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

printNote :: Pitch -> Duration -> Doc 
printNote p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)


-- Mullein - middle c is C5
-- Abc - middle c is 'C' upper case c  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 5     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 6       = text (replicate (i-6) '\'') 
              | i < 5       = text (replicate (5-i) ',')
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

mode :: Mode -> Doc
mode Major        = text "maj"
mode Minor        = text "min"
mode Lydian       = text "lyd"
mode Ionian       = text "ion" 
mode Mixolydian   = text "mix"
mode Dorian       = text "dor"
mode Aeolian      = text "aeo"
mode Phrygian     = text "phr"
mode Locrian      = text "loc"


    
multiplier :: Duration -> Doc
multiplier dn | dn == 1   = empty
              | otherwise = fn (numerator dn, denominator dn)
  where
    fn (n,1) = integer n
    fn (1,d) = char '/' <> integer d
    fn (n,d) = integer n <> char '/' <> integer d


