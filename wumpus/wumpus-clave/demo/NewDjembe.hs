{-# OPTIONS -Wall #-}

module NewDjembe1 where

import Wumpus.Djembe.Base
import Wumpus.Djembe.Graphic


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Maybe
import System.Directory
import Data.Ratio


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_attr text_drawing 
    writeEPS "./out/new_djembe01.eps" pic1
    writeSVG "./out/new_djembe01.svg" pic1 


std_attr :: DrawingContext
std_attr = joinBevel $ thick $ fontFace helvetica $ standardContext 48

text_drawing :: DDrawing
text_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ textline "PTgd..B" `at` zeroPt
   draw $ localize (fillColour black) $ period      `at` P2 200 0
   draw $ localize (fillColour black) $ swingStem   `at` P2 200 0
   draw $ localize (fillColour black) $ flamStem    `at` P2 220 0
   draw $ localize (fillColour black) $ dot         `at` P2 230 0
   draw $ localize (fillColour black) $ flamDot     `at` P2 230 0
   draw $ localize (fillColour black) $ evenStems 5 `at` P2 230 0
   draw $ localize (fillColour black) $ smallLetter 'D' `at` P2 250 0

   draw $ barBeamLines [group3, group3] `at` P2 0 300
   draw $ barBeats     [group3, group3] `at` P2 0 300

--   draw $ extractLocGraphic (ag_01 `aplus` advCh 'G') `at` P2 0 200
--   draw $ extractLocGraphic (drawBeat $ I sangban) `at` P2 350 200

ag_01 :: DAdvGraphic
ag_01 = advconcat (map advCh "ABCDEF")

advCh :: Num u => Char -> AdvGraphic u
advCh ch = makeAdvGraphic (hdisplace 30) (textline [ch])

-- How to get /many/ interpretations of a beat pattern?
--
-- ... given that the alphabet is not fixed e.g. need that add 
-- more drums like kenkeni.
-- 

-- Note - this is simple but not really valid - as kenkeni is 
-- fixed to the I constructor we cannot have it inside a plet.
--
abioueka :: Bar ()
abioueka = [ [nil, nil, nil], [nil, nil, nil]
           , [nil, nil, nil], [kenkeni, kenkeni, nil]
           ]

nil :: Beat () 
nil = I ()

kenkeni :: Beat () 
kenkeni = I ()

-- Note - this really needs a /no-graphic/ element.




class CSangban repr where
   sangban :: repr

instance CSangban G where
  sangban = G $ dot

-- This formulation will lead to huge class contexts!

demo1 :: CSangban repr => [repr]
demo1 = [sangban, sangban ]   

group1 :: CSangban repr => Group repr 
group1 = [ I sangban, I sangban, I sangban ]



class CSlap repr where
   slap :: repr
   slapflam  :: repr

instance CSlap G where
  slap      = G $ letter 'P'
  slapflam  = G $ letter 'P' -- todo


group2a :: (CSangban repr, CSlap repr) => Group repr 
group2a = [ I sangban, I sangban, I slap ]

class (CStroke repr, CSangban repr, CSlap repr) => Composite repr

instance Composite G

group2b :: Composite repr => Group repr 
group2b = [ I $ accent sangban, I sangban, I slap, S sangban ]

group3 :: Composite repr => Group repr 
group3 = [ I sangban, I sangban, S sangban, I sangban ] -- , Pl 3 2 [sangban, sangban, sangban] ]

-- This is annoying... 
-- group3 must be given an explicit type.
demo2 :: [(Int,Ratio Int)]
demo2 = groupSpans group3
