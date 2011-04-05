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

  , lfill
  , rfill
  , centerfill

  , fontColour
  , textSize

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
           | Text  EscapedText
           | Cat   (Doc u)                (Doc u)
           | Fill  VAlign                 u             (Doc u)
           | Local DrawingContextF        (Doc u)
           | Mono  (Query (AdvanceVec u)) [EscapedChar]

-- | TextFrame is the result Graphic made from rendering multiple
-- lines of DocText.
--
type TextFrame u = BoundedLocRectGraphic u


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
(<>) :: Doc u -> Doc u -> Doc u
a <> b = Cat a b
 

-- | Concatenate two DocTexts separated with a space.
--
(<+>) :: Doc u -> Doc u -> Doc u
a <+> b = a <> space <> b 




rfill :: u -> Doc u -> Doc u
rfill = Fill VLeft

lfill :: u -> Doc u -> Doc u
lfill = Fill VRight


centerfill :: u -> Doc u -> Doc u
centerfill = Fill VCenter



fontColour :: RGBi -> Doc u -> Doc u
fontColour rgb = Local (text_colour rgb)


-- Note with the formulation @ CF (u,AdvGraphic u) @ changing
-- text size will not work.
--

textSize :: Int -> Doc u -> Doc u
textSize sz = Local (set_font_size sz)



leftAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
leftAlign = renderMultiLine VLeft

centerAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
centerAlign = renderMultiLine VCenter


rightAlign :: (Real u, Floating u, InterpretUnit u) 
          => [Doc u] -> TextFrame u
rightAlign = renderMultiLine VRight



renderMultiLine :: (Real u, Floating u, InterpretUnit u) 
                => VAlign -> [Doc u] -> BoundedLocRectGraphic u
renderMultiLine va docs = body >>= posTextWithMargins
  where
    body = (\dy -> let xs = map interpret docs
                   in valignSepPO emptyPosObject va dy $ reverse xs)
              <$> textlineSpace

interpret :: (Fractional u, Ord u, InterpretUnit u) 
          => Doc u -> PosObject u
interpret Empty             = interpEmpty
interpret Space             = interpSpace
interpret (Text esc)        = interpText esc
interpret (Cat a b)         = hcatPO (interpret a) (interpret b)
interpret (Fill va w a)     = interpFill va w (interpret a)
interpret (Local upd a)     = localizePO upd (interpret a)
interpret (Mono q1 xs)      = interpMono q1 xs



interpEmpty :: InterpretUnit u => PosObject u
interpEmpty = makePosObject (pure $ Orientation 0 0 0 0) emptyLocGraphic



-- | Note - the current way of seeding the LocGraphic with the 
-- DrawingContext looks dodgy (substantial copying). 
-- 
-- Maybe EvalM should have a private, much smaller 
-- DrawingContext...
--
interpText :: InterpretUnit u => EscapedText -> PosObject u
interpText esc = 
    makePosObject (textOrientationZero esc) (escTextLine esc)
   



-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
interpSpace :: InterpretUnit u => PosObject u
interpSpace = 
    makePosObject (charOrientationZero $ CharEscInt $ ord ' ') emptyLocGraphic




interpFill :: (Fractional u, Ord u) 
           => VAlign -> u -> PosObject u -> PosObject u
interpFill VLeft   = padLeftPO
interpFill VCenter = padHorizontalPO
interpFill VRight  = padRightPO


interpMono :: InterpretUnit u 
           => Query (AdvanceVec u) -> [EscapedChar] -> PosObject u
interpMono qy1 chs = makeBindPosObject qy hkernOrientationZero hkernLine
  where
    qy = (\v1 -> monoSpace (advanceH v1) chs ) <$> qy1 




--------------------------------------------------------------------------------
-- Helpers


monoSpace :: Num u => u -> [EscapedChar] -> [KernChar u]
monoSpace w1 (c:cs) = (0,c) : map (\ch -> (w1,ch)) cs
monoSpace _  []     = []

           


