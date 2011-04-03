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
--  , int
--  , integer
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



-- | Space is the width of a space in the current font - it is 
-- filled in during interpretation.
--
data Doc u = Empty
           | Space 
           | Text  EscapedText
           | Cat   (Doc u)          (Doc u)
           | Fill  VAlign           u         (Doc u)
           | Local DrawingContextF  (Doc u)

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



type BoundedDoc u = BoundedPosObject u



newtype EvalM a = EvalM { getEvalM :: DrawingContext -> a }

instance Functor EvalM where
  fmap f mf = EvalM $ \ctx -> f $ getEvalM mf ctx


instance Applicative EvalM where
  pure a    = EvalM $ \_   -> a
  mf <*> ma = EvalM $ \ctx -> 
                let f = getEvalM mf ctx
                    a = getEvalM ma ctx
                in f a

instance Monad EvalM where
  return a  = EvalM $ \_   -> a
  ma >>= k  = EvalM $ \ctx -> 
                let a = getEvalM ma ctx
                in (getEvalM . k) a ctx


instance DrawingCtxM EvalM where
  askDC           = EvalM $ \ctx -> ctx
  asksDC f        = EvalM $ \ctx -> f ctx
  localize upd mf = EvalM $ \ctx -> getEvalM mf (upd ctx)


runEvalM :: DrawingContext -> EvalM a -> a
runEvalM ctx mf = getEvalM mf ctx



type InterpAns u = (Orientation u, BoundedDoc u)


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
renderMultiLine va docs = 
    askDC >>= \ctx -> 
    let xs = map (snd . runEvalM ctx . interpret) docs
    in body xs >>= posTextWithMargins
  where
    body xs = (\dy -> valignSepPO emptyBoundedPosObject va dy $ reverse xs)
                 <$> textlineSpace



interpret :: (Fractional u, Ord u, InterpretUnit u) 
          => Doc u -> EvalM (InterpAns u)
interpret Empty             = interpEmpty
interpret Space             = interpSpace
interpret (Text esc)        = interpText esc
interpret (Cat a b)         = catAns <$> interpret a <*> interpret b
interpret (Fill va w a)     = fillAns va w <$> interpret a
interpret (Local upd mf)    = interpLocal upd mf 


makeInterpAns :: Num u => Orientation u -> LocGraphic u -> InterpAns u
makeInterpAns ortt gf = (ortt, makeBoundedPosObject (pure $ ortt) gf)

interpEmpty :: InterpretUnit u => EvalM (InterpAns u)
interpEmpty = return $ makeInterpAns (Orientation 0 0 0 0) emptyLocGraphic



-- | Note - the current way of seeding the LocGraphic with the 
-- DrawingContext looks dodgy (substantial copying). 
-- 
-- Maybe EvalM should have a private, much smaller 
-- DrawingContext...
--
interpText :: InterpretUnit u
           => EscapedText -> EvalM (InterpAns u)
interpText esc = 
    (\ortt ctx -> (ortt, makeBoundedPosObject (pure ortt) $ 
                            localize (const ctx) $ escTextLine esc))
      <$> textOrientationZero esc <*> askDC 


-- | Note - a space character is not draw in the output, instead 
-- 'space' advances the width vector by the width of a space in 
-- the current font.
--
interpSpace :: InterpretUnit u 
            => EvalM (InterpAns u)
interpSpace = 
    (\ortt -> makeInterpAns ortt emptyLocGraphic)
      <$> charOrientationZero (CharEscInt $ ord ' ')


-- | Don\'t need the monad for @Cat@.
--
catAns :: (Num u, Ord u) => InterpAns u -> InterpAns u -> InterpAns u
catAns (o0,gf0) (o1,gf1) = (o0 `spineRight` o1, gf0 `hcatPO` gf1)


fillAns :: (Fractional u, Ord u) 
        => VAlign -> u -> InterpAns u -> InterpAns u
fillAns va w (o@(Orientation xmin xmaj _ _), po) = 
    if (w <= ow) then (o,po)
                 else (omove va o, gmove va po)
  where
    ow            = xmin + xmaj
    dx            = w - ow
    
    omove VLeft   = extendORight dx 
    omove VCenter = extendORight (0.5*dx) . extendOLeft (0.5*dx)
    omove VRight  = extendOLeft dx

    gmove VLeft   = bimapPosObject (fmap $ omove VLeft) id
    gmove VCenter = bimapPosObject (fmap $ omove VCenter) 
                                   (moveStart (displaceVec $ hvec $ 0.5*dx))
    gmove VRight  = bimapPosObject (fmap $ omove VRight) 
                                   (moveStart (displaceVec $ hvec dx))




interpLocal :: (Fractional u, Ord u, InterpretUnit u) 
            => DrawingContextF -> Doc u -> EvalM (InterpAns u)
interpLocal upd doc = localize upd (interpret doc)


{-

int :: InterpretUnit u => Int -> DocText u
int i = uniformSpace (map CharLiteral $ show i) mkvecQ 
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0') 
        


integer :: InterpretUnit u => Integer -> DocText u
integer i = uniformSpace (map CharLiteral $ show i) mkvecQ
  where
    mkvecQ = advanceH <$> charVector (CharLiteral '0')
    
-}



{-
--------------------------------------------------------------------------------
-- Helpers


uniformSpace :: InterpretUnit u 
             => [EscapedChar] -> Query u -> BoundedPosObject u
uniformSpace xs qx = hkernPrim $ qx >>= go xs
  where 
    go (c:cs) dx = return $ (0,c) : map (\ch -> (dx,ch)) cs
    go []     _  = return []


hkernPrim :: InterpretUnit u => Query [KernChar u] -> BoundedPosObject u
hkernPrim qks = 
    makeBoundedPosObject (qks >>= hkernOrientationZero) (lift0R1 qks >>= hkernLine)
           
-}

