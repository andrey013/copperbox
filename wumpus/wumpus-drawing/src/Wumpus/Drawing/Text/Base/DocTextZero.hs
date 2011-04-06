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
  , TextFrame
  , render 

  , leftAlign
  , centerAlign
  , rightAlign

  , blank
  , space
  , string
  , escaped
  , int
  , integer
  , float
  , ffloat

  , (<>)
  , (<+>) 
  , vcatl
  , vcatc 
  , vcatr

  , lfill
  , rfill
  , centerfill

  , fontColour
  , textSize
  , bold
  , italic
  , boldItalic  
  , strikethrough
  , underline
  , highlight

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Data.Char ( ord )
import Numeric


-- | Space is the width of a space in the current font - it is 
-- filled in during interpretation.
--
data Doc u = Empty
           | Space 
           | Text EscapedText
           | Cat (Doc u) (Doc u)
           | VCat VAlign (Doc u) (Doc u)
           | Fill VAlign u (Doc u)
           | DLocal DrawingContextF (Doc u)
           | TLocal (TextContextF u) (Doc u)
           | Mono (WidthQuery u) [EscapedChar]
           | AElab (AElaborateF u) (Doc u)

type WidthQuery u = Query (AdvanceVec u)

type TextContextF u = TextContext u -> TextContext u

type AElaborateF u = Orientation u -> LocGraphic u



-- | TextFrame is the result Graphic made from rendering multiple
-- lines of DocText.
--
type TextFrame u = BoundedLocRectGraphic u


-- NOTE - should the API use @em@ for fill, padding etc.?

blank       :: Doc u
blank       = Empty

space       :: Doc u
space       = Space

string      :: String -> Doc u
string      = Text . escapeString

escaped     :: EscapedText -> Doc u
escaped     = Text



int :: InterpretUnit u => Int -> Doc u
int i = integer $ fromIntegral i


integer :: InterpretUnit u => Integer -> Doc u
integer i = Mono (charVector $ CharLiteral '0') (map CharLiteral $ show i)

-- | Specialized version of 'ffloat' - the answer is always 
-- rendered at \"full precision\".
--
float :: (RealFloat a, InterpretUnit u) => a -> Doc u
float = ffloat Nothing


-- | This is equivalent to 'showFFloat' in the Numeric module.
-- 
-- Like 'showFFloat', the answer is rendered to supplied 
-- precision. @Nothing@ indicated full precision.
--
ffloat :: (RealFloat a, InterpretUnit u) => (Maybe Int) -> a -> Doc u
ffloat mb d = Mono (charVector $ CharLiteral '0') xs
  where
    xs = (map CharLiteral $ ($ []) $ showFFloat mb d)




infixr 6 <>, <+>


-- | Concatenate two DocTexts separated with no spacing.
--
-- (infixr 6)
--
(<>) :: Doc u -> Doc u -> Doc u
a <> b = Cat a b
 

-- | Concatenate two Docs separated with a space.
--
-- (infixr 6)
--
(<+>) :: Doc u -> Doc u -> Doc u
a <+> b = a <> space <> b 

infixr 5 `vcatl`, `vcatc`, `vcatr`

-- | Vertically concatenate - aligning left.
-- 
-- (infixr 5) 
--
vcatl :: Doc u -> Doc u -> Doc u
vcatl = VCat VLeft

-- | Vertically concatenate - aligning center.
-- 
-- (infixr 5) 
--
vcatc :: Doc u -> Doc u -> Doc u
vcatc = VCat VCenter

-- | Vertically concatenate - aligning right.
-- 
-- (infixr 5) 
--
vcatr :: Doc u -> Doc u -> Doc u
vcatr = VCat VRight

leftAlign  :: [Doc u] -> Doc u
leftAlign  = multiline vcatl

centerAlign  :: [Doc u] -> Doc u
centerAlign  = multiline vcatc

rightAlign  :: [Doc u] -> Doc u
rightAlign  = multiline vcatr


multiline :: (Doc u -> Doc u -> Doc u) -> [Doc u] -> Doc u
multiline _  []     = blank 
multiline op (x:xs) = go x xs
  where
    go a [] = a
    go a (b:bs) = go (a `op` b) bs




rfill :: u -> Doc u -> Doc u
rfill = Fill VLeft

lfill :: u -> Doc u -> Doc u
lfill = Fill VRight


centerfill :: u -> Doc u -> Doc u
centerfill = Fill VCenter



fontColour :: RGBi -> Doc u -> Doc u
fontColour rgb = DLocal (text_colour rgb)



textSize :: Int -> Doc u -> Doc u
textSize sz = DLocal (set_font_size sz)



bold        :: Doc u -> Doc u
bold        = TLocal (\s -> s { text_bold = True, text_italic = False })

italic      :: Doc u -> Doc u
italic      = TLocal (\s -> s { text_bold = False, text_italic = True })

boldItalic  :: Doc u -> Doc u
boldItalic  = TLocal (\s -> s { text_bold = True, text_italic = True })


strikethrough :: Doc u -> Doc u
strikethrough = TLocal (\s -> s { text_strikethrough = True })

underline :: Doc u -> Doc u
underline = TLocal (\s -> s { text_underline = True })

-- | Background fill.
--
highlight :: (Fractional u, InterpretUnit u) 
          => RGBi -> Doc u -> Doc u
highlight rgb = AElab (drawBackfill rgb)



render :: (Real u, Floating u, InterpretUnit u) 
       => FontFamily -> Doc u -> BoundedLocRectGraphic u 
render ff doc = localize (set_font $ regularWeight ff) $
    textlineSpace               >>= \sep -> 
    posTextWithMargins $ runEvalM (initTextCtx sep ff) (interpret doc) 


data TextContext u = TextContext
      { text_strikethrough      :: Bool
      , text_underline          :: Bool
      , text_sep                :: u
      , text_font_family        :: FontFamily
      , text_bold               :: Bool
      , text_italic             :: Bool
      }
  


initTextCtx :: u -> FontFamily -> TextContext u
initTextCtx sep ff = TextContext 
    { text_strikethrough      = False
    , text_underline          = False
    , text_sep                = sep
    , text_font_family        = ff
    , text_bold               = False
    , text_italic             = False 
    }

newtype EvalM u a = EvalM { getEvalM :: TextContext u -> a }


instance Functor (EvalM u) where
  fmap f mf = EvalM $ \ctx -> f $ getEvalM mf ctx


instance Applicative (EvalM u) where
  pure a    = EvalM $ \_   -> a
  mf <*> ma = EvalM $ \ctx -> 
                let f = getEvalM mf ctx
                    a = getEvalM ma ctx
                in f a

instance Monad (EvalM u) where
  return a  = EvalM $ \_   -> a
  ma >>= k  = EvalM $ \ctx -> 
                let a = getEvalM ma ctx
                in (getEvalM . k) a ctx

asks :: (TextContext u -> a) -> EvalM u a
asks f = EvalM $ \ctx -> f ctx

lineSpace :: EvalM u u
lineSpace = asks text_sep

local :: (TextContext u -> TextContext u) -> EvalM u a -> EvalM u a
local upd mf = EvalM $ \ctx -> getEvalM mf (upd ctx)

runEvalM :: TextContext u -> EvalM u a -> a
runEvalM ctx mf = getEvalM mf ctx



interpret :: (Fractional u, Ord u, InterpretUnit u) 
          => Doc u -> EvalM u (PosObject u)
interpret Empty             = interpEmpty
interpret Space             = interpSpace
interpret (Text esc)        = interpText esc
interpret (Cat a b)         = hcatPO    <$> interpret a <*> interpret b
interpret (VCat va a b)     = 
    pvsep va  <$> lineSpace <*> interpret a <*> interpret b

interpret (Fill va w a)     = ppad va w <$> interpret a
interpret (DLocal upd a)    = localizePO upd <$> interpret a
interpret (TLocal upd a)    = local upd (interpret a)
interpret (Mono q1 xs)      = interpMono q1 xs
interpret (AElab fn a)      = aelaboratePO fn <$> interpret a




interpEmpty :: InterpretUnit u => EvalM u (PosObject u)
interpEmpty = return $ makePosObject (pure $ Orientation 0 0 0 0) emptyLocGraphic



-- | Note - the current way of seeding the LocGraphic with the 
-- DrawingContext looks dodgy (substantial copying). 
-- 
-- Maybe EvalM should have a private, much smaller 
-- DrawingContext...
--
interpText :: (Fractional u, InterpretUnit u) 
           => EscapedText -> EvalM u (PosObject u)
interpText esc = interpretLeaf $
    makePosObject (textOrientationZero esc) (escTextLine esc)
   



-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
interpSpace :: InterpretUnit u 
            => EvalM u (PosObject u)
interpSpace = return $ makePosObject qy1  emptyLocGraphic
  where
    qy1 = charOrientationZero $ CharEscInt $ ord ' '

pvsep :: (Fractional u, Ord u, InterpretUnit u)
       => VAlign -> u -> PosObject u -> PosObject u 
       -> PosObject u
pvsep VLeft     = vsepLeftPO
pvsep VCenter   = vsepCenterPO
pvsep VRight    = vsepRightPO



ppad :: (Fractional u, Ord u) 
           => VAlign -> u -> PosObject u -> PosObject u
ppad VLeft   = padLeftPO
ppad VCenter = padHorizontalPO
ppad VRight  = padRightPO


interpMono :: (Fractional u, InterpretUnit u)
           => Query (AdvanceVec u) -> [EscapedChar] 
           -> EvalM u (PosObject u)
interpMono qy1 chs = interpretLeaf $
    makeBindPosObject qy hkernOrientationZero hkernLine
  where
    qy = (\v1 -> monoSpace (advanceH v1) chs ) <$> qy1 




interpretLeaf :: (Fractional u, InterpretUnit u)
              => PosObject u -> EvalM u (PosObject u)
interpretLeaf po = 
    (\f1 f2 sty -> f1 $ f2 $ localizePO sty po) 
       <$> (fmap (condE drawUnderline)       $ asks text_underline)
       <*> (fmap (condE drawStrikethrough)   $ asks text_strikethrough)
       <*> textstyle
   where
     condE f b = if b then elaboratePO f else id

textstyle :: EvalM u DrawingContextF
textstyle = 
    fn <$> asks text_font_family <*> asks text_bold <*> asks text_italic
  where
    fn ff False False = set_font $ regularWeight ff
    fn ff True  False = set_font $ boldWeight ff
    fn ff False True  = set_font $ italicWeight ff
    fn ff _     _     = set_font $ boldItalicWeight ff

--------------------------------------------------------------------------------
-- Helpers


monoSpace :: Num u => u -> [EscapedChar] -> [KernChar u]
monoSpace w1 (c:cs) = (0,c) : map (\ch -> (w1,ch)) cs
monoSpace _  []     = []

           

-- API might be simple if we conditionally apply strikethrough on 
-- interpText (possibly including spaces), but never on interpSpace.
--
-- Might want to derive stroke_colour from text_colour and linewidth
-- fromf font size as well...
--
drawStrikethrough :: (Fractional u, InterpretUnit u) 
              => Orientation u -> LocGraphic u
drawStrikethrough (Orientation xmin xmaj _ ymaj) = 
    linestyle $ moveStart (displaceVec $ vec (-xmin) vpos) hline 
  where
    vpos  = 0.45 * ymaj
    hline = locStraightLine (hvec $ xmin + xmaj)



drawUnderline :: (Fractional u, InterpretUnit u) 
              => Orientation u -> LocGraphic u
drawUnderline (Orientation xmin xmaj ymin _) = 
    linestyle $ moveStart (displaceVec $ vec (-xmin) vpos) hline 
  where
    vpos  = negate $ 0.45 * ymin
    hline = locStraightLine (hvec $ xmin + xmaj)

linestyle :: LocGraphic u -> LocGraphic u
linestyle mf = 
    pointSize >>= \sz -> 
    localize (stroke_use_text_colour . set_line_width (lim sz)) mf
  where
    lim i | i < 10    = 1.0
          | otherwise = (fromIntegral i) / 15.0 


-- | Note - halving the TextMargin looks good.
--
drawBackfill :: (Fractional u, InterpretUnit u) 
              => RGBi -> Orientation u -> LocGraphic u
drawBackfill rgb (Orientation xmin xmaj ymin ymaj) = 
    textMargin >>= \(dx,dy) -> 
    let hdx = 0.5 * dx
        hdy = 0.5 * dy 
    in localize (fill_colour rgb) $ moveStart (mkVec hdx hdy) (mkRect hdx hdy)
  where
    mkVec  dx dy = displaceVec $ vec (negate $ xmin+dx) (negate $ ymin+dy)
    mkRect dx dy = let w = dx + xmin + xmaj + dx
                       h = dy + ymin + ymaj + dy
                   in filledRectangle w h
