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

    GenDoc
  , Doc 
  , DocGraphic
  , runGenDoc

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

--
-- Design Issue:
--
-- Can user state be added to Doc?
--
-- Easy if PosObject had user state, difficult as it doesn\'t...
--
 
-- | Doc type.
--
newtype GenDoc st u a = GenDoc { getGenDoc :: DocEnv -> GenPosObject st u a } 

type instance DUnit  (GenDoc st u a) = u
type instance UState (GenDoc st u)   = st

type GenDocGraphic st u = GenDoc st u (UNil u)

type Doc u a = GenDoc () u a

type DocGraphic u = Doc u (UNil u)


data DocEnv = DocEnv 
      { doc_alignment   :: VAlign
      , doc_font_family :: FontFamily
      }

instance Functor (GenDoc st u) where
  fmap f ma = GenDoc $ \env -> fmap f $ getGenDoc ma env

instance Applicative (GenDoc st u) where
  pure a    = GenDoc $ \_   -> pure a
  mf <*> ma = GenDoc $ \env -> getGenDoc mf env <*> getGenDoc ma env


instance Monad (GenDoc st u) where
  return a  = GenDoc $ \_   -> return a
  ma >>= k  = GenDoc $ \env -> getGenDoc ma env >>= \a -> getGenDoc (k a) env

instance (Monoid a, InterpretUnit u) => Monoid (GenDoc st u a) where
  mempty          = GenDoc $ \_   -> mempty
  ma `mappend` mb = GenDoc $ \env -> getGenDoc ma env `hconcat` getGenDoc mb env


instance DrawingCtxM (GenDoc st u) where
  askDC           = GenDoc $ \_   -> askDC 
  asksDC fn       = GenDoc $ \_   -> asksDC fn
  localize upd ma = GenDoc $ \env -> localize upd (getGenDoc ma env)


instance UserStateM (GenDoc st u) where
  getState        = GenDoc $ \_ -> getState
  setState s      = GenDoc $ \_ -> setState s
  updateState upd = GenDoc $ \_ -> updateState upd


runGenDoc :: VAlign -> FontFamily -> GenDoc st u a -> GenPosObject st u a
runGenDoc va ff ma = getGenDoc ma env1 
  where
    env1 = DocEnv { doc_alignment = va, doc_font_family = ff }



--------------------------------------------------------------------------------
-- Get vcat vconcat... from the Concat class

instance (Monoid a, Fractional u, InterpretUnit u) => 
    Concat (GenDoc st u a) where
  hconcat = mappend
  vconcat = vcatImpl

vcatImpl        :: (Monoid a, Fractional u, InterpretUnit u) 
                => GenDoc st u a -> GenDoc st u a -> GenDoc st u a
vcatImpl ma mb  = GenDoc $ \env -> 
    let va = doc_alignment env 
    in textlineSpace >>= \sep -> 
       valignSpace va sep (getGenDoc ma env) (getGenDoc mb env)

--------------------------------------------------------------------------------
-- Primitives

infixr 6 <+>

-- | Concatenate two Docs separated with a space.
--
-- (infixr 6)
--
(<+>) :: InterpretUnit u 
      => GenDocGraphic st u -> GenDocGraphic st u -> GenDocGraphic st u
a <+> b = a `mappend` space `mappend` b 



blank     :: InterpretUnit u => GenDocGraphic st u
blank     = GenDoc $ \_ -> posTextPrim (Left "")

space     :: InterpretUnit u => GenDocGraphic st u
space     = GenDoc $ \_ -> posCharPrim (Left ' ')


string    :: InterpretUnit u => String -> GenDocGraphic st u
string ss = GenDoc $ \_ -> posTextPrim (Left ss)



escaped     :: InterpretUnit u => EscapedText -> GenDocGraphic st u
escaped esc = GenDoc $ \_ -> posTextPrim (Right esc)

embedPosObject :: GenPosObject st u a -> GenDoc st u a
embedPosObject ma = GenDoc $ \_ -> ma



--------------------------------------------------------------------------------
-- Change font weight

bold :: GenDoc st u a -> GenDoc st u a 
bold ma = GenDoc $ \env -> 
    localize (set_font $ boldWeight $ doc_font_family env)
             (getGenDoc ma env)


italic :: GenDoc st u a -> GenDoc st u a 
italic ma = GenDoc $ \env -> 
    localize (set_font $ italicWeight $ doc_font_family env)
             (getGenDoc ma env)


boldItalic :: GenDoc st u a -> GenDoc st u a 
boldItalic ma = GenDoc $ \env -> 
    localize (set_font $ boldItalicWeight $ doc_font_family env)
             (getGenDoc ma env)


--------------------------------------------------------------------------------
-- Monospace

monospace :: InterpretUnit u 
          => EscapedChar -> EscapedText -> GenDocGraphic st u
monospace ref_ch esc = GenDoc $ \_ -> 
    monospaceEscText (vector_x <$> escCharVector ref_ch) esc




int :: InterpretUnit u => Int -> GenDocGraphic st u
int i = integer $ fromIntegral i


integer :: InterpretUnit u => Integer -> GenDocGraphic st u
integer i = monospace (CharLiteral '0') (escapeString $ show i)



--------------------------------------------------------------------------------


-- | Specialized version of 'ffloat' - the answer is always 
-- rendered at \"full precision\".
--
float :: (RealFloat a, InterpretUnit u) => a -> GenDocGraphic st u
float = ffloat Nothing


-- | This is equivalent to 'showFFloat' in the Numeric module.
-- 
-- Like 'showFFloat', the answer is rendered to supplied 
-- precision. @Nothing@ indicated full precision.
--
ffloat :: (RealFloat a, InterpretUnit u) 
       => (Maybe Int) -> a -> GenDocGraphic st u
ffloat mb d = 
    monospace (CharLiteral '0') $ escapeString  $ ($ "") $ showFFloat mb d






--------------------------------------------------------------------------------
-- Decorate

strikethrough :: (Fractional u, InterpretUnit u) 
              => GenDoc st u a -> GenDoc st u a
strikethrough = decorateDoc SUPERIOR drawStrikethrough 

underline :: (Fractional u, InterpretUnit u) 
          => GenDoc st u a -> GenDoc st u a
underline = decorateDoc SUPERIOR drawUnderline

highlight :: (Fractional u, InterpretUnit u) 
          => RGBi -> GenDoc st u a -> GenDoc st u a
highlight rgb = decorateDoc ANTERIOR (drawBackfill rgb) 
 


decorateDoc :: InterpretUnit u 
            => ZDeco -> (Orientation u -> LocGraphic u) -> GenDoc st u a 
            -> GenDoc st u a
decorateDoc zdec fn ma = GenDoc $ \env -> 
    decoratePosObject zdec fn $ getGenDoc ma env


           

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
                   in dcRectangle DRAW_FILL w h

