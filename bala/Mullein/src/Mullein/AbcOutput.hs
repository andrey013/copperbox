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
import Mullein.Pitch
import Mullein.Utils

import Control.Applicative hiding ( empty )
import Control.Monad.State
import Data.Foldable ( foldlM, toList )
import Data.Sequence ( (<|), (|>), (><), ViewL(..), viewl )
import qualified Data.Sequence as S
import Data.Ratio
import Text.PrettyPrint.Leijen hiding ( (<$>) )

data St = St { current_key :: Key, current_meter :: Meter }
data Env = Env {}

type M a = State St a

data AbcFragment = MidtuneField Doc 
                 | BarOutput Doc
                 | Prefix String        -- e.g. "|:" 
                 | Suffix String        -- e.g. "|" or ":|"
  deriving (Show)


-- For ABC, having a type class to print pitches is currently superfluous.
-- Whereas LilyPond has some obviously different pitches (common pitches 
-- c4,d4 etc, and drum pitches) ABC only has common pitches.
-- For the time being ABC follows LilyPond, by the logic that if I ever
-- add annotations they will probably be paired with playable notes 
-- (i.e. pitches).

class AbcPitch e where
  abcNote  :: e -> Duration -> Doc   -- for notes
  abcPitch :: e -> Doc               -- for pitches within chords, graces notes,

instance AbcPitch Pitch where
  abcNote p dm = note p dm
  abcPitch p   = note p 1




outputAbc :: AbcPitch e => Key -> Meter -> [Int] -> PartP e -> Doc
outputAbc k m ns a = postProcess ns $ evalState (oPart a) s0 where
    s0 = St { current_key = k, current_meter = m } 


oPart :: AbcPitch e => PartP e -> M (S.Seq AbcFragment)
oPart (Part as)          =
    foldlM (\a e -> (a ><) <$> oPhrase e) S.empty as

oPhrase :: AbcPitch e => PhraseP e -> M (S.Seq AbcFragment)
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a  <*> oMotif x <*> oMotif y
                                         
oMotif :: AbcPitch e => MotifP e -> M (S.Seq AbcFragment)
oMotif (Motif k m bs)    = 
    mcons2 <$> keyChange k  <*> meterChange m <*> foldlM fn S.empty bs
  where
    fn se a       = (\x -> se |> BarOutput x) <$> oBar a
    mcons2 x y xs = (g x) `mbCons` (g y) `mbCons` xs
    g             = fmap MidtuneField


oBar :: AbcPitch e => BarP e -> M Doc
oBar (Bar a)             = oUnison a
oBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> oUnison a 
                                  <*> mapM oUnison as


oUnison :: AbcPitch e => UnisonP e -> M Doc
oUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '-'
                                                           else empty)
                                  <$> mapM oBracket ps

oBracket :: AbcPitch e => BracketP e -> M Doc
oBracket (Singleton e)   = pure $ oElement e
oBracket (Bracket es)    = pure $ hcat $ map oElement es



oElement :: AbcPitch e => ElementP e -> Doc
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


postProcess :: [Int] -> S.Seq AbcFragment -> Doc
postProcess ns = fn ns . dropRepStart where
    fn _      se  | S.null se   = empty
    fn []     se                = printLine $ toList se
    fn (i:is) se                = printLine xs `nextLine` fn is se' where
                                    (xs,se') = fragSplitAt i se

-- If a tune starts with a repeated, the repeat start sign '|:', doesn't
-- need to be printed
dropRepStart :: S.Seq AbcFragment -> S.Seq AbcFragment
dropRepStart s0 = step $ S.viewl s0 where
    step (Prefix "|:" :< se) = se
    step _                   = s0


-- This isn't right - dropF drops too many and repeats aren't partitioned 
-- correctly
fragSplitAt :: Int 
            -> S.Seq AbcFragment 
            -> ([AbcFragment], S.Seq AbcFragment)
fragSplitAt i se = (\(xs,(_,vw)) -> (xs,g vw)) $  anaSt phi (i,viewl se) 
  where
    phi (_, EmptyL)             = Nothing
    phi (n, BarOutput d :< sa)  
            | n >  0            = Just (BarOutput d, (n-1, viewl sa))
            | otherwise         = Nothing
    phi (n, Prefix s :< sa) 
            | n >  0            = Just (Prefix s, (n, viewl sa)) -- take prefix (don't decr)
            | otherwise         = Nothing

   -- can always take a suffix or midtune field
    phi (n, s :< sa)            = Just (s, (n, viewl sa))

    g EmptyL                    = S.empty
    g (a :< sa)                 = a <| sa

 
printLine :: [AbcFragment] -> Doc
printLine  = step empty . intersperseBars  where
    step acc []                    = acc
    step acc (MidtuneField d : xs) = step (acc <> linecont `nextLine` d 
                                                           `nextLine` empty) xs
    step acc (BarOutput d : xs)    = step (acc <> d) xs
    step acc (Prefix s : xs)       = step (acc <> text s) xs 
    step acc (Suffix s : xs)       = step (acc <> text s) xs

    linecont = char '\\'


intersperseBars :: [AbcFragment] -> [AbcFragment]
intersperseBars (BarOutput d1 : BarOutput d2 : xs) = 
    BarOutput d1 : Suffix "|" : intersperseBars (BarOutput d2 : xs)
intersperseBars (x:xs)      = x : intersperseBars xs
intersperseBars []          = []
  

--------------------------------------------------------------------------------
-- helpers


infixr 5 `mbCons`
mbCons :: Maybe a -> S.Seq a -> S.Seq a
mbCons Nothing  = id
mbCons (Just a) = (a <|) 



keyChange :: Key -> M (Maybe Doc)
keyChange new = do 
    old <- gets current_key 
    if (new==old) 
       then return Nothing
       else do { modify $ \s -> s{current_key=new} 
               ; return $ Just $ keyField new } 

meterChange :: Meter -> M (Maybe Doc)
meterChange new = do 
    old <- gets current_meter
    if (new==old) 
       then return Nothing
       else do { modify $ \s -> s{current_meter=new} 
               ; return $ Just $ meterField new } 



keyField :: Key -> Doc
keyField (Key k m xs) = field 'K' keyspec where
    keyspec        = pitchLabel k UPPER <> modeSpec m <> extras
    -- Don't print \maj\ here
    modeSpec Major = empty
    modeSpec x     = mode x 
    extras         = hsep $ map (pitchLabel `flip` LOWER) xs


meterField :: Meter -> Doc
meterField = field 'M' . f where
    f (TimeSig n d) = integer n <> char '/' <> integer d
    f CommonTime    = text "C"
    f CutTime       = text "C|"


field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d





overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    

note :: Pitch -> Duration -> Doc 
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


repeated :: S.Seq AbcFragment -> S.Seq AbcFragment
repeated se = Prefix "|:" <| (se |> Suffix ":|") 

fsrepeat :: S.Seq AbcFragment 
         -> S.Seq AbcFragment 
         -> S.Seq AbcFragment
         -> S.Seq AbcFragment
fsrepeat se x y = Prefix "|:" <| se >< end where
    end = Prefix "|[1"  <| x >< (Prefix ":|[2" <| (y |> Suffix ":|")) 

