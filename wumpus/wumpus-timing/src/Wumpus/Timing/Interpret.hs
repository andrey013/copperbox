{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Timing.Interpret
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- 
--
--------------------------------------------------------------------------------

module Wumpus.Timing.Interpret
  (
    compile
  , interpret

  ) where


import Wumpus.Timing.Alphabet
import Wumpus.Timing.Drawing
import Wumpus.Timing.DrawingPrims
import Wumpus.Timing.Width

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.SVGColours




compile :: [Letter] -> (Posn, [(Instruction,Transition)])
compile []      = (Mid,[])
compile xs      = (firstPosn xs, program xs)


-- Never start with a transition - thus glyphs and spaces
-- need to look further into the list...
--
firstPosn :: [Letter] -> Posn
firstPosn []     = Mid
firstPosn (a:as) = step a as
  where
    step (H _)   _      = High
    step (C _ _) _      = High      -- ?? Clock
    step (L _)   _      = Low
    step G       (x:xs) = step x xs
    step (S _)   (x:xs) = step x xs
    step _       _      = Mid

program :: [Letter] -> [(Instruction,Transition)]
program = lookahead1 prgm2 prgm1

prgm2 :: Letter -> Letter -> (Instruction,Transition)
prgm2 a b = (ins,transition a b) 
  where
    ins   = instruction a (isSpace b)
          
prgm1 :: Letter -> (Instruction,Transition)
prgm1 a = (ins,T_Zero) 
  where
    ins   = instruction a False


transition :: Letter -> Letter -> Transition
transition G        _   = T_Zero 
transition (S _)    _   = T_Zero
transition (H _)    b   = fromHi b  
transition (L _)    b   = fromLo b
transition (C n _)  b   = if even n then fromHi b else fromLo b 
transition _        b   = fromMid b


fromHi :: Letter -> Transition
fromHi (H _)          = T_Zero
fromHi G              = T_Zero
fromHi (C _ _)        = T_Zero
fromHi (L _)          = HL
fromHi _              = HM

fromLo :: Letter -> Transition
fromLo (L _)          = T_Zero
fromLo G              = T_Zero
fromLo (H _)          = LH
fromLo (C _ _)        = LHC
fromLo _              = HM

fromMid :: Letter -> Transition
fromMid (H _)          = MH
fromMid (L _)          = ML
fromMid (C _ _)        = MHC
fromMid _              = T_Zero




instruction :: Letter -> Bool -> Instruction
instruction (H w)     _                      = Line w black
instruction (L w)     _                      = Line w black
instruction (Z w)     _                      = Line w blue
instruction (X w)     _                      = Line w red
instruction (D w s)   open | open == True    = OData w whitesmoke  (Just s)
instruction (D w s)   _                      = CData w whitesmoke  (Just s)
instruction (U w)     open | open == True    = OData w darkGray    Nothing
instruction (U w)     _                      = CData w darkGray    Nothing
instruction (C n w1)  _                      = Clock n w1
instruction (M w)     _                      = Metastasis w
instruction G         _                      = Glitch
instruction (S w)     _                      = Space w
    
isSpace :: Letter -> Bool
isSpace (S _) = True
isSpace _     = False

-- traverse the list with a lookahead of 1.
--
lookahead1 :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
lookahead1 phi chi = step
  where step []       = []
        step [x]      = [chi x]
        step (x:y:xs) = phi x y : step (y:xs)






--------------------------------------------------------------------------------
-- OLD


-- Coordinate handling could be simplified with something like
-- TikZ's path ...

interpret :: [Letter] -> DGraphic
interpret []     = emptyG
interpret (l:ls) = let (g1,next,x2) = interp l Init 0 in g1 . rest next x2 ls
  
pt :: Width -> DPoint2
pt w = P2 (fromIntegral $ 10*halfcount w) 0

std_props :: Props
std_props = defaultProps

zprops :: Props
zprops = std_props { stroke_colour = blue }

xprops :: Props
xprops = std_props { stroke_colour = red }

interp :: Letter -> Prefix -> Width -> (DGraphic,Prefix,Width)
interp (H n) pre w = (lineHigh pre (halfcount n) 10 std_props (pt w) , FromTop, w+n)
interp (L n) pre w = (lineLow  pre (halfcount n) 10 std_props (pt w),  FromBtm, w+n)
interp (Z n) pre w = (lineMid  pre (halfcount n) 10 zprops (pt w),     FromCtr, w+n)
interp (X n) pre w = (lineMid  pre (halfcount n) 10 xprops (pt w),     FromCtr, w+n)

interp G     pre w = (glitch 10 std_props (pt w), pre, w) -- props wrong...

rest :: Prefix -> Width -> [Letter] -> DGraphic
rest _   _ [] = emptyG
rest pre w (x:xs) = let (g1,next,w2) = interp x pre w in g1 . rest next w2 xs
