{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.SyntaxInterim
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Images - scores that have been partially rendered and are 
-- composed as Docs.
--
--
--------------------------------------------------------------------------------

module Neume.Core.SyntaxInterim
  (


  -- * Phrases and bars
    StaffPhrase(..)
  , StaffBar
  , Phrase(..)
  , Bar 

  -- * Staff expressions
  , CExpr(..)


  , Image
  , PhraseImage(..)
  , BarImage

  , PhraseOverlayImage(..)
  , BarOverlayImage


  , BarNum
  , PhraseName
  , ExtractBarImages(..)

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
-- import Neume.Core.SyntaxGlyph
import Neume.Core.Utils

import Text.PrettyPrint.Leijen          -- package: wl-print

import Data.List ( foldl' )
import Data.Sequence 

--------------------------------------------------------------------------------
-- Phrases and bars 

-- Note - phrases, bars and CExprs are polymorphic on the glyph
-- type. They can use alternatives to the Glyph type. 

data StaffPhrase gly = StaffPhrase 
      { phrase_name     :: String
      , phrase_bars     :: Seq (StaffBar gly) 
      }
  deriving (Show)


type    StaffBar    gly = Seq (CExpr gly)

data Phrase bar = Phrase 
      { phrase_name_z   :: String
      , pahrase_bars_z  :: [bar]
      }
  deriving (Show)

type Bar elt = [elt]


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


cexprFold :: (gly -> b -> b) -> (PletMult -> b -> b) -> (b -> b) 
          -> b -> CExpr gly -> b
cexprFold f _ _ b (Atom a)       = f a b
cexprFold f g h b (N_Plet pm xs) = foldl' (cexprFold f g h) (g pm b) xs
cexprFold f g h b (Beamed xs)    = foldl' (cexprFold f g h) (h b)    xs


cexprMeasure :: DMeasure gly => CExpr gly -> DurationMeasure 
cexprMeasure = snd . cexprFold  phi chi rho (mult_stack_zero,0) where
  phi a  (stk,acc) = (stk, acc + nmeasureCtx stk a)
  chi pm (stk,acc) = (pushPM pm stk,acc) 
  rho    st        = st


--------------------------------------------------------------------------------
-- Phrases and bars 

-- Phrases and bars are composable with pretty-print operations...


type PhraseName     = String
type Image          = Doc

data PhraseImage = PhraseImage 
      { phrase_image_name   :: PhraseName
      , phrase_image_bars   :: [BarImage]
      }
  deriving (Show)

type BarImage           = Image


-- This is formed from merging 2 or more PhraseImages so it has
-- a list of phrase names
--
data PhraseOverlayImage = PhraseOverlayImage
      { phrase_overlay_img_names :: [PhraseName]
      , phrase_overlay_img_bars  :: [BarOverlayImage] 
      }
  deriving (Show)

type BarOverlayImage    = Image

type BarNum             = Int


class ExtractBarImages a where
  extractBarImages :: a -> [Image]

instance ExtractBarImages PhraseImage where
  extractBarImages = phrase_image_bars

instance ExtractBarImages PhraseOverlayImage where
  extractBarImages = phrase_overlay_img_bars



--------------------------------------------------------------------------------
-- 

{-

-- Now defunct?

mapBar :: (CExpr gly -> CExpr gly') -> StaffPhrase gly -> StaffPhrase gly'
mapBar f (StaffPhrase name se) = StaffPhrase name $ fmap (fmap f) se

-}
--------------------------------------------------------------------------------
-- Instances

instance Functor Phrase where
  fmap f (Phrase name bars) = Phrase name $ map f bars

instance Functor StaffPhrase where
  fmap f (StaffPhrase name xs) = StaffPhrase name $ fmap (fmap (fmap f)) xs



instance Functor CExpr where
  fmap f (Atom e)         = Atom $ f e
  fmap f (N_Plet d cexpr) = N_Plet d $ map (fmap f) cexpr
  fmap f (Beamed cexpr)   = Beamed $ map (fmap f) cexpr



-- StateMap
instance StateMap StaffPhrase where
  stmap f st (StaffPhrase name xs) = (StaffPhrase name xs',st') 
    where (xs',st') = stmap (stmap (stmap f)) st xs


instance StateMap CExpr where
  stmap f st (Atom  e)     = (Atom e',st')      where (e',st') = f st e
  stmap f st (N_Plet d ce) = (N_Plet d ce',st') 
                             where (ce',st') = stmap (stmap f) st ce

  stmap f st (Beamed ce)   = (Beamed ce',st')   
                             where (ce',st') = stmap (stmap f) st ce

---

instance DMeasure gly => DMeasure (CExpr gly) where
  dmeasure = cexprMeasure


-- TODO..........................................
-- whoa -- this is circular...
instance BeamExtremity gly => BeamExtremity (CExpr gly) where
  rendersToNote (Atom a) = rendersToNote a
  rendersToNote _        = True
