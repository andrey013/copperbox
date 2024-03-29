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
  , getAtomBBox

  , getBangOnLoad

  -- * Setters

  , font_size
  , set_label_font
  , label_font_size
  , label_font_face

  , bg_colour
  , fg_colour
  , label_colour


  , bang_on_load
  , no_bang_on_load  

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
    , ctx_label_offsets :: Offsets
    , ctx_bang_on_load  :: InitLoad
    }


type PdContextF = PdContext -> PdContext

standard_context :: PdContext
standard_context = 
    PdContext { ctx_font_size     = FONT_16
              , ctx_display_props = default_props
              , ctx_label_offsets = Offsets { x_offset = 0, y_offset = (-6) }
              , ctx_bang_on_load  = NONE_ON_LOAD
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

getLabelOffsets :: ContextM m => m Offsets
getLabelOffsets = asksCtx ctx_label_offsets


getObjectBBox :: ContextM m => Int -> Point -> m BoundingBox
getObjectBBox cc bl = (\sz -> objectBBox sz cc bl) 
    <$> asksCtx ctx_font_size

getMessageBBox :: ContextM m => Int -> Point -> m BoundingBox
getMessageBBox cc bl = (\sz -> messageBBox sz cc bl) 
    <$> asksCtx ctx_font_size

getAtomBBox :: ContextM m => Int -> Point -> m BoundingBox
getAtomBBox cc bl = (\sz -> atomBBox sz cc bl) 
    <$> asksCtx ctx_font_size

getBangOnLoad :: ContextM m => m InitLoad 
getBangOnLoad = asksCtx ctx_bang_on_load


--------------------------------------------------------------------------------
-- Setters

-- helpers 

updateDisplayProps :: (DisplayProps -> DisplayProps) -> PdContextF
updateDisplayProps fn = 
    (\s i -> s { ctx_display_props = fn i }) <*> ctx_display_props



--- Setters 

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

bang_on_load :: PdContextF 
bang_on_load = \s -> s { ctx_bang_on_load = DEFAULT_ON_LOAD }

no_bang_on_load :: PdContextF 
no_bang_on_load = \s -> s { ctx_bang_on_load = NONE_ON_LOAD }