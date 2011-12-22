{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondPretty
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for LilyPond.
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondPretty
  (

    SDoc
  , syntax
  , element
  , parenthesize
  , unSDoc
  , (><)


  , LDoc
  , newline
  , spaced
  , unLDoc
  , mapLDoc

    -- * Printing glyphs
  , note
  , pitch
  , pitchTreble
  , pitchLabel
  , duration
  , rest
  , spacer
  , tie

  , chordForm
  , graceForm
  , beamForm
--  , pletForm

  ) where


import Neume.Core.Duration
-- import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.Utils.Pretty

import qualified Data.Foldable as F
import Data.Monoid
import Data.Sequence ( Seq, ViewL(..), ViewR(..), (<|), (|>) )
import qualified Data.Sequence as S
import Text.PrettyPrint.HughesPJ


-- | Structured (introspective) Doc
--
newtype SDoc = SDoc { getSDoc :: Seq Doc1 }


instance Monoid SDoc where
  mempty  = SDoc mempty
  mappend = (><)

-- | Account for nesting information as labels on notes (or rests).
--
-- > Chords nest /prefix-suffix/ @<c e>@
--
-- > Beamed notes nest /suffix suffix/ @e[ f g a]@
--
data Doc1 = Syntax { _syntax :: Doc }
          | Elem   { _nest_prefix   :: Doc 
                   , _note_symbol   :: Doc
                   , _nest_suffix   :: Doc 
                   }


syntax :: Doc -> SDoc 
syntax = SDoc . S.singleton . Syntax

element :: Doc -> SDoc
element d = SDoc $ S.singleton $ Elem empty d empty


prefixed :: Doc -> Doc -> SDoc
prefixed pre d = SDoc $ S.singleton $ Elem pre d empty

suffixed :: Doc -> Doc -> SDoc
suffixed suf d = SDoc $ S.singleton $ Elem empty d suf


parenthesize :: Doc -> Doc -> SDoc -> SDoc
parenthesize hd tl se = updLastElem tl $ updFirstElem hd se
   
   
-- | Update the suffix nesting of the first note in a sequence.
--
updFirstElem :: Doc -> SDoc -> SDoc
updFirstElem d0 (SDoc ss) = SDoc $ go $ S.viewl ss
  where
    go EmptyL                = S.empty
    go (Elem nl d1 nr :< se) = Elem nl d1 (nr <> d0) <| se
    go (a :< se)             = a <| go (S.viewl se)


-- | Update the suffix nesting of the last note in a sequence.
--
updLastElem :: Doc -> SDoc -> SDoc
updLastElem d0 (SDoc ss) = SDoc $ go $ S.viewr ss
  where
    go EmptyR                = S.empty
    go (se :> Elem nl d1 nr) = se |> Elem nl d1 (nr <> d0)
    go (se :> a)             = (go $ S.viewr se) |> a


unSDoc :: SDoc -> Doc
unSDoc = hsep . map pp . F.toList . getSDoc
  where
    pp (Syntax d)      = d
    pp (Elem nl d0 nr) = nl <> d0 <> nr

infixr 5 ><

(><) :: SDoc -> SDoc -> SDoc
SDoc s1 >< SDoc s2 = SDoc (s1 S.>< s2)

--------------------------------------------------------------------------------

-- | /Left-biased/ Doc.
-- 

data LDoc = LDoc { _ldoc :: Doc, _lcat :: Doc -> Doc -> Doc }

instance Monoid LDoc where
  mempty = LDoc empty (<>)
  LDoc ld lcat `mappend` LDoc rd rcat = LDoc (ld `lcat` rd) rcat


newline :: Doc -> LDoc
newline d = LDoc d ($+$)

spaced :: Doc -> LDoc
spaced d = LDoc d (<+>)

unLDoc :: LDoc -> Doc
unLDoc (LDoc d _) = d

mapLDoc :: (Doc -> Doc) -> LDoc -> LDoc 
mapLDoc f (LDoc d lcat) = LDoc (f d) lcat

--------------------------------------------------------------------------------
-- Printing glyphs


-- | Helper for @note@ build Doc rather than SDoc
--
note_ :: Pitch -> Maybe Duration -> Doc
note_ p md =  pitch p <> maybe empty duration md 


-- | Print a note, the duration is a Maybe value. Nothing indicates
-- that the note has the same duration as the previous glyph.
--
note :: Pitch -> Maybe Duration -> SDoc
note p md = element $ note_ p md

-- | Print a Pitch - middle c is @c'@.
--
pitch :: Pitch -> Doc 
pitch pch@(Pitch _ _ o) = pitchLabel (label pch) <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty

pitchTreble :: Pitch -> Doc
pitchTreble (Pitch l mba o) = pitch $ Pitch l mba (o-3)


-- | Print a 'PitchLabel'.
pitchLabel :: PitchLabel -> Doc
pitchLabel (PitchLabel l ma) = 
    char (toLowerLChar l) <> maybe empty accidental ma
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"

-- | Print a Duration.
duration :: Duration -> Doc
duration = maybe empty df . lyRepresentation
  where
    df (LyCmd ss,dc) = dots dc $ command ss 
    df (LyNum i,dc)  = dots dc $ int i

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id





-- | Print a rest, duration is a Maybe value - Nothing indicates
-- that the rest has the same duration as the previous glyph.  
rest :: Maybe Duration -> SDoc
rest md = element $ char 'r' <> maybe empty duration md


-- | Print an invisible rest, commonly used to align overlayed 
-- bars. Duration is a Maybe value - Nothing indicates
-- that the spacer has the same duration as the previous glyph.  
spacer :: Maybe Duration -> SDoc
spacer md = element $ char 's' <> maybe empty duration md

-- | Print a tie.
tie :: SDoc
tie = syntax $ char '~'


-- | Chords - notes printed inside angle brackets, followed by 
-- duration, e.g.:
--
-- @ 
--  \<c e g\>4
-- @ 
chordForm :: [Pitch] -> Maybe Duration -> SDoc
chordForm []     _  = error "chordForm - empty list"    
chordForm (p:ps) md = prefixed (char '<') (note_ p Nothing) >< go ps
  where
    go [t]    = suffixed (note_ t Nothing) (char '>' <> maybe empty duration md)
    go (t:ts) = note t Nothing >< go ts
    go []     = syntax empty   -- 


-- | Grace notes - @\\grace@ command then expression inside braces,
-- e.g:
--
-- @ 
--  \\grace { f32[ e] }
-- @ 
graceForm :: SDoc -> SDoc
graceForm x = syntax (command "grace") >< syntax lbrace >< x >< syntax rbrace

  
-- | Beams - first element printed outside the square brackets, e.g.:
-- @ 
--  c16 [e g c]
-- @ 
beamForm :: SDoc -> SDoc
beamForm = parenthesize (char '[') (char ']')


-- slurs are printed like beams ( ) with parens.

{-

-- | N-ary plets - @\\times ../..@ command then expression 
-- inside braces,
-- e.g:
--
-- @ 
--  \\times 3/5 { c8[ c c c c] }
-- @ 
pletForm :: PletMult -> [Doc] -> Doc
pletForm (n,d) xs = lyCommand "times" <+> integer n <> char '/' <> integer d
                                      <+> braces (hsep xs)

-}