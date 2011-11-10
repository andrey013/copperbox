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
    LabelPosition(..)
  , Font(..)
  , SRL(..)
  , DisplayProps(..)

  , Bang(..)

  , default_display
  , noSRL

  ) where 

import PDSS.Core.Colour



data LabelPosition = LEFT | RIGHT | TOP | BOTTOM
  deriving (Enum,Eq,Ord,Show)

data Font = COURIER | HELVETICA | TIMES
  deriving (Enum,Eq,Ord,Show)

-- | Send - Receive - Label
--
data SRL = SRL
   { srl_send       :: Maybe String
   , srl_recv       :: Maybe String
   , srl_label      :: Maybe String
   }


-- | Display properties for @obj@.
--
data DisplayProps = DisplayProps
   { obj_font       :: Font
   , obj_fontsize   :: Int
   , obj_bgcolour   :: RGBi
   , obj_fgcolour   :: RGBi
   , obj_lblcolour  :: RGBi
   }
  deriving (Eq,Ord,Show)   




newtype Bang = Bang Int
  deriving (Eq,Ord,Show)


default_display :: DisplayProps
default_display = DisplayProps
   { obj_font       = COURIER
   , obj_fontsize   = 8
   , obj_bgcolour   = white
   , obj_fgcolour   = black
   , obj_lblcolour  = black
   }


noSRL :: SRL 
noSRL = SRL Nothing Nothing Nothing