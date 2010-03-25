{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxStaff
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Music syntax tree for staff notation.
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxStaff
  (
  -- * Phrases and bars
    StaffPhrase(..)
  , StaffBar(..)


  -- * Staff expressions
  , CExpr(..)
  , PletMult

  -- * Staff glyphs
  , Glyph(..)
  , Note(..)
  , Tie
  , ChordPitch(..)


  -- * Synonyms

  , GlyphDur
  , NoteDur

  , GlyphRelDur
  , NoteRelDur

  , StdGlyph
  , AnnoGlyph

  -- * Rewriting types
  , CtxRewrite
  , FreeRewrite

  ) where


import Neume.Core.BeamExtremity
import Neume.Core.Duration
import Neume.Core.Pitch
import Neume.Core.Utils.FunctorN
import Neume.Core.Utils.OneList
import Neume.Core.Utils.StateMap

import Text.PrettyPrint.Leijen          -- package: wl-pprint

--------------------------------------------------------------------------------
-- Phrases and bars 

-- Note - phrases, bars and CExprs are polymorphic on the glyph
-- type. They can use alternatives to the Glyph type. 

newtype StaffPhrase gly = StaffPhrase { extractBars   :: [StaffBar gly] }
newtype StaffBar    gly = StaffBar    { extractNotes  :: [CExpr gly]  } 




--------------------------------------------------------------------------------
-- Staff \Expressions\



-- | Contextual expression. This is a sequence of one or more 
-- notes together with some context to be communicated to the 
-- pretty printer - the context being either that the notes 
-- should be beamed or that they are n-plets (duplets, triplets, 
-- ...). 
--
-- Note this formulation permits beam groups within beam groups.
-- Ideally this would be disallowed, but beam groups may contain
-- n-plets (and n-plets must be recursive).
--

data CExpr gly = Atom               gly 
               | N_Plet  PletMult   [CExpr gly]
               | Beamed             [CExpr gly]
  deriving (Eq,Show)

type PletMult = (Int,Int)


--------------------------------------------------------------------------------
-- Staff Glyphs



data Glyph anno pch dur = GlyNote  (Note anno pch dur) !Tie
                        | Rest     !dur
                        | Spacer   !dur
                        | Chord    (OneList (ChordPitch anno pch)) !dur !Tie
                        | Graces   (OneList (Note anno pch dur)) 
  deriving (Eq,Show)


data Note anno pch dur = Note !anno !pch !dur
  deriving (Eq,Show) 

type Tie = Bool

-- Note dur is not used... 
data ChordPitch anno pch = ChordPitch !anno !pch
  deriving (Eq,Show)


-- Synonyms


type GlyphDur    anno pch   = Glyph anno pch Duration
type NoteDur     anno pch   = Note  anno pch Duration


-- | LilyPond shorthand...
type GlyphRelDur anno pch   = Glyph anno pch (Maybe Duration)
type NoteRelDur  anno pch   = Note  anno pch (Maybe Duration)



type StdGlyph           = Glyph ()   Pitch Duration
type AnnoGlyph anno     = Glyph anno Pitch Duration


--------------------------------------------------------------------------------
-- rewrites

type CtxRewrite  st gly gly' = st -> StaffPhrase gly -> (StaffPhrase gly',st)
type FreeRewrite    gly gly' = StaffPhrase gly -> StaffPhrase gly'


--------------------------------------------------------------------------------
-- Instances

instance Functor StaffPhrase where
  fmap f (StaffPhrase xs) = StaffPhrase $ fmap (fmap f) xs

instance Functor StaffBar where
  fmap f (StaffBar os)    = StaffBar $ map (fmap f) os


instance Functor CExpr where
  fmap f (Atom e)         = Atom $ f e
  fmap f (N_Plet d cexpr) = N_Plet d $ map (fmap f) cexpr
  fmap f (Beamed cexpr)   = Beamed $ map (fmap f) cexpr


-- FMap2 

instance FMap2 ChordPitch where
  fmap2 f g (ChordPitch a p) = ChordPitch (f a) (g p)

-- FMap3
instance FMap3 Note where
  fmap3 f1 f2 f3 (Note a p d) = Note (f1 a) (f2 p) (f3 d)


instance FMap3 Glyph where
  fmap3 f1 f2 f3 (GlyNote n t)  = GlyNote (fmap3 f1 f2 f3 n) t
  fmap3 _  _  f3 (Rest d)       = Rest (f3 d)
  fmap3 _  _  f3 (Spacer d)     = Spacer (f3 d)
  fmap3 f1 f2 f3 (Chord os d t) = Chord (fmap (fmap2 f1 f2) os) (f3 d) t
  fmap3 f1 f2 f3 (Graces os)    = Graces (fmap (fmap3 f1 f2 f3) os)


-- StateMap
instance StateMap StaffPhrase where
  stmap f st (StaffPhrase xs) = (StaffPhrase xs',st') 
                                where (xs',st') = stmap (stmap f) st xs

instance StateMap StaffBar where
  stmap f st (StaffBar os) = (StaffBar os',st') 
    where (os',st') = stmap (stmap f) st os


instance StateMap CExpr where
  stmap f st (Atom  e)     = (Atom e',st')      where (e',st') = f st e
  stmap f st (N_Plet d ce) = (N_Plet d ce',st') 
                             where (ce',st') = stmap (stmap f) st ce

  stmap f st (Beamed ce)   = (Beamed ce',st')   
                             where (ce',st') = stmap (stmap f) st ce


-- StateMap2 

instance StateMap2 ChordPitch where
  stmap2 f g st (ChordPitch a p) = (ChordPitch a' p',st'') 
                                   where (a',st')  = f st a
                                         (p',st'') = g st' p


-- StateMap3 

instance StateMap3 Note where
  stmap3 f1 f2 f3 st (Note a p d) = (Note a' p' d', st''')
                                    where (a',st')   = f1 st a
                                          (p',st'')  = f2 st' p
                                          (d',st''') = f3 st'' d

instance StateMap3 Glyph where
  stmap3 f1 f2 f3 st (GlyNote n t)  = (GlyNote n' t, st') 
    where (n',st') = stmap3 f1 f2 f3 st n

  stmap3 _  _  f3 st (Rest d)       = (Rest d', st')   where (d',st') = f3 st d
  stmap3 _  _  f3 st (Spacer d)     = (Spacer d', st') where (d',st') = f3 st d
  stmap3 f1 f2 f3 st (Chord os d t) = (Chord os' d' t, st'')
    where (os',st') = stmap (stmap2 f1 f2) st os
          (d',st'') = f3 st' d
  
  stmap3 f1 f2 f3 st (Graces os)    = (Graces os', st')
    where (os',st') = stmap (stmap3 f1 f2 f3) st os


--------------------------------------------------------------------------------

instance NumMeasured (Glyph anno pch Duration) where
  type Measurement (Glyph anno pch Duration) = DurationMeasure
  nmeasure (GlyNote  (Note _ _ d) _) = extent d
  nmeasure (Rest     d)              = extent d
  nmeasure (Spacer   d)              = extent d
  nmeasure (Chord _ d _)             = extent d
  nmeasure (Graces _)                = 0

instance MakeSpacer (Glyph anno pch Duration) where
  makeSpacer d = Spacer d

instance MakeRest (Glyph anno pch Duration) where
  makeRest d = Rest d

--------------------------------------------------------------------------------
instance BeamExtremity (Glyph anno pch dur) where
  rendersToNote (GlyNote _ _) = True
  rendersToNote (Rest _)      = False
  rendersToNote (Spacer _)    = False
  rendersToNote (Chord _ _ _) = True
  rendersToNote (Graces _)    = False
 

--------------------------------------------------------------------------------
-- Pretty instances

snocDur :: Pretty a => Doc -> a -> Doc
snocDur d drn = d <> char '\'' <> pretty drn

snocTie :: Doc -> Bool -> Doc
snocTie d True = d <> char '~'
snocTie d _    = d


instance (Pretty pch, Pretty dur) => Pretty (Glyph anno pch dur) where
  pretty (GlyNote n t)  = pretty n `snocTie` t
  pretty (Rest    d)    = char 'r' `snocDur` d
  pretty (Spacer  d)    = char 's' `snocDur` d
  pretty (Chord os d t) = (angles $ hsep $ toListF pretty os) `snocDur` d 
                                                              `snocTie` t

  pretty (Graces os)    = braces $ hsep $ toListF pretty os


instance (Pretty pch, Pretty dur) => Pretty (Note anno pch dur) where
  pretty (Note _ p d) = pretty p `snocDur` d


instance (Pretty pch) => Pretty (ChordPitch anno pch) where
  pretty (ChordPitch _ p) = pretty p

