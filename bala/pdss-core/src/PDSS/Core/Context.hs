{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  PDSS.Core.Context
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Equivalent to DrawingContext in Wumpus-Basic.
--
--------------------------------------------------------------------------------


module PDSS.Core.Context
  ( 
    PdContext
  , PdContextF

  , standard_context

  , ContextM(..)

  -- * Getters
  , getFontSize
  , getDisplayProps
  , getLabelOffsets

  , getObjectBBox
  , getMessageBBox

  -- * Setters

  , font_size
  , set_label_font
  , label_font_size
  , label_font_face

  , bg_colour
  , fg_colour
  , label_colour
  

  ) where 

import PDSS.Core.BoundingBox
import PDSS.Core.Colour
import PDSS.Core.InternalTypes

import Control.Applicative


-- | Canvas has a font size that is inherited by objects 
-- and comments (text).
--
data PdContext = PdContext
    { ctx_font_size     :: FontSize
    , ctx_display_props :: DisplayProps
    , ctx_label_xoff    :: Int
    , ctx_label_yoff    :: Int
    }


type PdContextF = PdContext -> PdContext

standard_context :: PdContext
standard_context = 
    PdContext { ctx_font_size     = FONT_16
              , ctx_display_props = default_props
              , ctx_label_xoff    = 0
              , ctx_label_yoff    = (-6)
              }


--------------------------------------------------------------------------------

-- | 'ContextM' is equivalent to the to the @MonadReader@ 
-- class, but the environment type is fixed to 'PdContext'.
--
-- To avoid name clashes with @mtl@ this scheme is used:
--
-- > askCtx   = ask
-- > asksCtx  = asks
-- > localize = local
--
class (Applicative m, Monad m) => ContextM (m :: * -> *) where
  askCtx    :: m PdContext
  asksCtx   :: (PdContext -> a) -> m a
  localize  :: (PdContext -> PdContext) -> m a -> m a

  asksCtx f  = f <$> askCtx


--------------------------------------------------------------------------------
-- Getters


getFontSize :: ContextM m => m FontSize
getFontSize = asksCtx ctx_font_size

getDisplayProps :: ContextM m => m DisplayProps
getDisplayProps = asksCtx ctx_display_props

getLabelOffsets :: ContextM m => m (Int,Int)
getLabelOffsets = (,) <$> asksCtx ctx_label_xoff <*> asksCtx ctx_label_yoff


getObjectBBox :: ContextM m => Int -> Point -> m BoundingBox
getObjectBBox wc bl = (\sz -> objectBBox sz wc bl) 
    <$> asksCtx ctx_font_size

getMessageBBox :: ContextM m => Int -> Point -> m BoundingBox
getMessageBBox wc bl = (\sz -> messageBBox sz wc bl) 
    <$> asksCtx ctx_font_size


--------------------------------------------------------------------------------
-- Setters

-- helpers 

updateDisplayProps :: (DisplayProps -> DisplayProps) -> PdContextF
updateDisplayProps fn = 
    (\s i -> s { ctx_display_props = fn i }) <*> ctx_display_props


font_size :: FontSize -> PdContextF
font_size sz = \s -> s { ctx_font_size = sz }

set_label_font :: FontFace -> Int -> PdContextF
set_label_font ff sz = updateDisplayProps $ \s -> s { obj_fontface  = ff
                                              , obj_fontsize  = sz }

label_font_face :: FontFace -> PdContextF
label_font_face ff = updateDisplayProps $ \s -> s { obj_fontface  = ff }

label_font_size :: Int -> PdContextF
label_font_size sz = updateDisplayProps $ \s -> s { obj_fontsize  = sz }



bg_colour :: RGBi -> PdContextF 
bg_colour rgb = updateDisplayProps $ \s -> s { obj_bgcolour = rgb }

fg_colour :: RGBi -> PdContextF 
fg_colour rgb = updateDisplayProps $ \s -> s { obj_fgcolour = rgb }


label_colour :: RGBi -> PdContextF 
label_colour rgb = updateDisplayProps $ \s -> s { obj_lblcolour = rgb }