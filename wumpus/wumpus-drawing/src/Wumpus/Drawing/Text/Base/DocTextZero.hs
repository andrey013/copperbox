{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.DocTextZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Flexible text type, composable with @pretty-print@ style 
-- operators.
-- 
-- Direction zero (left-to-right) only.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.DocTextZero
  ( 


    Doc 
  , DocGraphic
  , runDoc

  , (<+>)
  , blank
  , space
  , string
  , escaped
  , embedPosObject
  
  , bold
  , italic
  , boldItalic

  , monospace
  , int 
  , integer
  , float
  , ffloat

  , strikethrough
  , underline
  , highlight

  ) where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Monoid
import Numeric


 
-- | Doc type.
--
newtype Doc u a = Doc { getDoc :: DocEnv -> PosObject u a } 

type instance DUnit (Doc u a) = u

type DocGraphic u = Doc u (UNil u)


data DocEnv = DocEnv 
      { doc_alignment   :: VAlign
      , doc_font_family :: FontFamily
      }

instance Functor (Doc u) where
  fmap f ma = Doc $ \env -> fmap f $ getDoc ma env

instance Applicative (Doc u) where
  pure a    = Doc $ \_   -> pure a
  mf <*> ma = Doc $ \env -> getDoc mf env <*> getDoc ma env


instance Monad (Doc u) where
  return a  = Doc $ \_   -> return a
  ma >>= k  = Doc $ \env -> getDoc ma env >>= \a -> getDoc (k a) env



instance DrawingCtxM (Doc u) where
  askDC           = Doc $ \_   -> askDC 
  asksDC fn       = Doc $ \_   -> asksDC fn
  localize upd ma = Doc $ \env -> localize upd (getDoc ma env)

instance (Monoid a, InterpretUnit u) => Monoid (Doc u a) where
  mempty = Doc $ \_ -> mempty
  ma `mappend` mb = Doc $ \env -> getDoc ma env `hconcat` getDoc mb env



runDoc :: Doc u a -> VAlign -> FontFamily -> PosObject u a
runDoc ma va ff = getDoc ma env1 
  where
    env1 = DocEnv { doc_alignment = va, doc_font_family = ff }



--------------------------------------------------------------------------------
-- Get vcat vconcat... from the Concat class

instance (Monoid a, Fractional u, InterpretUnit u) => Concat (Doc u a) where
  hconcat = mappend
  vconcat = vcatImpl

vcatImpl        :: (Monoid a, Fractional u, InterpretUnit u) 
                => Doc u a -> Doc u a -> Doc u a
vcatImpl ma mb  = Doc $ \env -> 
    let va = doc_alignment env 
    in textlineSpace >>= \sep -> 
       valignSpace va sep (getDoc ma env) (getDoc mb env)

--------------------------------------------------------------------------------
-- Primitives

infixr 6 <+>

-- | Concatenate two Docs separated with a space.
--
-- (infixr 6)
--
(<+>) :: InterpretUnit u => DocGraphic u -> DocGraphic u -> DocGraphic u
a <+> b = a `mappend` space `mappend` b 



blank     :: InterpretUnit u => DocGraphic u
blank     = Doc $ \_ -> posTextPrim (Left "")

space     :: InterpretUnit u => DocGraphic u
space     = Doc $ \_ -> posCharPrim (Left ' ')


string    :: InterpretUnit u => String -> DocGraphic u
string ss = Doc $ \_ -> posTextPrim (Left ss)



escaped     :: InterpretUnit u => EscapedText -> DocGraphic u
escaped esc = Doc $ \_ -> posTextPrim (Right esc)

embedPosObject :: PosObject u a -> Doc u a
embedPosObject ma = Doc $ \_ -> ma



--------------------------------------------------------------------------------
-- Change font weight

bold :: Doc u a -> Doc u a 
bold ma = Doc $ \env -> 
    localize (set_font $ boldWeight $ doc_font_family env)
             (getDoc ma env)


italic :: Doc u a -> Doc u a 
italic ma = Doc $ \env -> 
    localize (set_font $ italicWeight $ doc_font_family env)
             (getDoc ma env)


boldItalic :: Doc u a -> Doc u a 
boldItalic ma = Doc $ \env -> 
    localize (set_font $ boldItalicWeight $ doc_font_family env)
             (getDoc ma env)


--------------------------------------------------------------------------------
-- Monospace

monospace :: InterpretUnit u => EscapedChar -> EscapedText -> DocGraphic u
monospace ref_ch esc = Doc $ \_ -> 
    monospaceEscText (vector_x <$> escCharVector ref_ch) esc




int :: InterpretUnit u => Int -> DocGraphic u
int i = integer $ fromIntegral i


integer :: InterpretUnit u => Integer -> DocGraphic u
integer i = monospace (CharLiteral '0') (escapeString $ show i)



--------------------------------------------------------------------------------


-- | Specialized version of 'ffloat' - the answer is always 
-- rendered at \"full precision\".
--
float :: (RealFloat a, InterpretUnit u) => a -> DocGraphic u
float = ffloat Nothing


-- | This is equivalent to 'showFFloat' in the Numeric module.
-- 
-- Like 'showFFloat', the answer is rendered to supplied 
-- precision. @Nothing@ indicated full precision.
--
ffloat :: (RealFloat a, InterpretUnit u) => (Maybe Int) -> a -> DocGraphic u
ffloat mb d = 
    monospace (CharLiteral '0') $ escapeString  $ ($ "") $ showFFloat mb d






--------------------------------------------------------------------------------
-- Decorate

strikethrough :: (Fractional u, InterpretUnit u) 
              => Doc u a -> Doc u a
strikethrough = decorateDoc SUPERIOR drawStrikethrough 

underline :: (Fractional u, InterpretUnit u) 
          => Doc u a -> Doc u a
underline = decorateDoc SUPERIOR drawUnderline

highlight :: (Fractional u, InterpretUnit u) 
          => RGBi -> Doc u a -> Doc u a
highlight rgb = decorateDoc ANTERIOR (drawBackfill rgb) 
 


decorateDoc :: InterpretUnit u 
            => ZDeco -> (Orientation u -> LocGraphic u) -> Doc u a -> Doc u a
decorateDoc zdec fn ma = Doc $ \env -> 
    decoratePosObject zdec fn $ getDoc ma env


           

-- API might be simple if we conditionally apply strikethrough on 
-- interpText (possibly including spaces), but never on interpSpace.
--
-- Might want to derive stroke_colour from text_colour and linewidth
-- fromf font size as well...
--
drawStrikethrough :: (Fractional u, InterpretUnit u) 
                  => Orientation u -> LocGraphic u
drawStrikethrough (Orientation xmin xmaj _ ymaj) = 
    linestyle $ moveStart (vec (-xmin) vpos) ln
  where
    vpos  = 0.45 * ymaj
    ln    = locStraightLine (hvec $ xmin + xmaj)



drawUnderline :: (Fractional u, InterpretUnit u) 
              => Orientation u -> LocGraphic u
drawUnderline (Orientation xmin xmaj _ _) = 
    underlinePosition >>= \vpos ->
    linestyle $ moveStart (vec (-xmin) vpos) ln
  where
    ln    = locStraightLine (hvec $ xmin + xmaj)


-- | This uses underline_thickness ...
--
linestyle :: LocGraphic u -> LocGraphic u
linestyle mf = 
    underlineThickness >>= \sz -> 
    localize (stroke_use_text_colour . set_line_width sz) mf


-- | Note - quarter margin looks good.
--
drawBackfill :: (Fractional u, InterpretUnit u) 
             => RGBi -> Orientation u -> LocGraphic u
drawBackfill rgb (Orientation xmin xmaj ymin ymaj) = 
    textMargin >>= \(dx,dy) -> 
    let hdx = 0.25 * dx
        hdy = 0.25 * dy 
    in localize (fill_colour rgb) $ moveStart (mkVec hdx hdy) (mkRect hdx hdy)
  where
    mkVec  dx dy = vec (negate $ xmin+dx) (negate $ ymin+dy)
    mkRect dx dy = let w = dx + xmin + xmaj + dx
                       h = dy + ymin + ymaj + dy
                   in dcRectangle FILL w h

