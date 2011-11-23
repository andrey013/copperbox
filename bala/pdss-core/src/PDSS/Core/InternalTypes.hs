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

  , FontSize(..)

  , InitLoad(..)
  , OpenOnLoad(..)
  , GraphOnParent(..)
  , NewOld(..)
  , SliderScale(..)
  , SliderSteady(..)
  , Offsets(..)


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
  

data FontSize = FONT_08 | FONT_10 | FONT_12 | FONT_16
              | FONT_24 | FONT_36
  deriving (Bounded,Enum,Eq,Ord,Show)


-- | ZOrder of drawing.
--
data ZOrder = ZABOVE | ZBELOW
  deriving (Bounded,Enum,Eq,Ord,Show)


data LabelPosition = LEFT | RIGHT | TOP | BOTTOM
  deriving (Bounded,Enum,Eq,Ord,Show)

data FontFace = COURIER | HELVETICA | TIMES
  deriving (Bounded,Enum,Eq,Ord,Show)




-- | Default is NONE_ON_LOAD (aka 0).
--
data InitLoad = NONE_ON_LOAD | DEFAULT_ON_LOAD
  deriving (Bounded,Enum,Eq,Ord,Show)


-- | A canvas (ncanvas rather than cnv) may be open or closed on 
-- load.
-- 
-- Default is CLOSED_ON_LOAD (aka 0).
-- 
data OpenOnLoad = CLOSED_ON_LOAD | OPEN_ON_LOAD
  deriving (Bounded,Enum,Eq,Ord,Show)



-- | Default is GRAPH_AS_OBJECT (aka 0).
-- 
data GraphOnParent = GRAPH_AS_OBJECT | GRAPH_ON_PARENT
  deriving (Bounded,Enum,Eq,Ord,Show)




data NewOld = NEW_AND_OLD | NEW_ONLY
  deriving (Bounded,Enum,Eq,Ord,Show)



data SliderScale = SLIDER_LINEAR | SLIDER_LOG
  deriving (Bounded,Enum,Eq,Ord,Show)

data SliderSteady = SLIDER_JUMPS | SLIDER_STEADY
  deriving (Bounded,Enum,Eq,Ord,Show)

data Offsets = Offsets { x_offset :: !Int, y_offset :: !Int }
  deriving (Eq,Ord,Show)



-- | Send - Receive - Label
-- 
-- Labels should not contain spaces - Pd rewrites user entered
-- string s with spaces to use underscores instead.
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