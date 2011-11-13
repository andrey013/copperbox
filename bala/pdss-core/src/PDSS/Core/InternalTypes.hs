{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.InternalTypes
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal types and types to be made exported opaquely to 
-- client code.
--
--------------------------------------------------------------------------------


module PDSS.Core.InternalTypes
  ( 

    GenSt
  , incrSt
  , zeroSt

  , Primitive 
  , primitive

  , unwrapPrimitive

  , ZOrder(..)  
  , LabelPosition(..)
  , FontFace(..)
  , SRL(..)
  , DisplayProps(..)
  , Point(..)


  , default_props
  , noSRL

  ) where 

import PDSS.Core.Colour
import PDSS.Core.Utils.FormatCombinators
import PDSS.Core.Utils.HList


import Data.Monoid

-- | Generation threads a counter...
--
newtype GenSt = GenSt { getCounter :: Int }

incrSt :: GenSt -> (Int, GenSt)
incrSt s = let i = getCounter s in (i, GenSt $ i + 1 )

zeroSt :: GenSt
zeroSt = GenSt 0

-- | Primitive is a wrapped Doc, although it is expected that the 
-- Doc generates one or more Pd records.
--
newtype Primitive = Primitive { getPrimitive :: H Doc }
              

-- | Make Primitive opaque - the representation may change...
--
primitive :: Doc -> Primitive
primitive = Primitive . wrapH


instance Monoid Primitive where
  mempty        = Primitive emptyH
  a `mappend` b = Primitive $ getPrimitive a `appendH` getPrimitive b

unwrapPrimitive :: Primitive -> Doc
unwrapPrimitive = vcat . toListH . getPrimitive
  

-- | ZOrder of drawing.
--
data ZOrder = ZABOVE | ZBELOW
  deriving (Bounded,Enum,Eq,Ord,Show)


data LabelPosition = LEFT | RIGHT | TOP | BOTTOM
  deriving (Enum,Eq,Ord,Show)

data FontFace = COURIER | HELVETICA | TIMES
  deriving (Enum,Eq,Ord,Show)

-- | Send - Receive - Label
-- 
-- Labels should not contain spaces.
--
data SRL = SRL
   { srl_send       :: Maybe String
   , srl_recv       :: Maybe String
   , srl_label      :: Maybe String
   }


-- | Display properties for @obj@.
--
data DisplayProps = DisplayProps
   { obj_fontface   :: FontFace
   , obj_fontsize   :: Int
   , obj_bgcolour   :: RGBi
   , obj_fgcolour   :: RGBi
   , obj_lblcolour  :: RGBi
   }
  deriving (Eq,Ord,Show)   



data Point = P2 { point_x :: !Int, point_y :: !Int }
  deriving (Eq,Ord,Show)


newtype Bang = Bang Int
  deriving (Eq,Ord,Show)


default_props :: DisplayProps
default_props = DisplayProps
   { obj_fontface   = COURIER
   , obj_fontsize   = 8
   , obj_bgcolour   = white
   , obj_fgcolour   = black
   , obj_lblcolour  = black
   }


noSRL :: SRL 
noSRL = SRL Nothing Nothing Nothing