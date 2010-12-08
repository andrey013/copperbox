{-# OPTIONS -Wall #-}

module NewDjembe1 where

import Wumpus.Djembe.Base
import Wumpus.Djembe.Graphic


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import System.Directory


main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runDrawingU std_attr text_drawing 
    writeEPS "./out/new_djembe01.eps" pic1
    writeSVG "./out/new_djembe01.svg" pic1 


std_attr :: DrawingContext
std_attr = joinBevel $ fontFace helvetica $ standardContext 24

text_drawing :: DDrawing
text_drawing = drawTracing $ localize bothStrokeColour $ do 
   draw $ textline "PTgd..B" `at` zeroPt
   draw $ localize (fillColour black) $ fullstop    `at` P2 200 0
   draw $ localize (fillColour black) $ swingStem   `at` P2 200 0
   draw $ localize (fillColour black) $ flamStem    `at` P2 220 0
   draw $ localize (fillColour black) $ dot         `at` P2 230 0
   draw $ localize (fillColour black) $ flamDot     `at` P2 230 0
   draw $ localize (fillColour black) $ smallLetter 'D' `at` P2 250 0
   
   draw $ barLocGraphic abioueka_djembe_call `at` P2 0 300
   draw $ barLocGraphic [group3, group3]     `at` P2 0 150
   


class CBoxNotation repr where
  period           :: repr
  bass             :: repr
  tone             :: repr
  slap             :: repr
  tone_flam        :: repr
  
instance CBoxNotation G where
  period           = G $ fullstop
  bass             = G $ letter 'B'
  tone             = G $ dot
  slap             = G $ letter 'X'
  tone_flam        = G $ letter '?'

abioueka_djembe_call :: CBoxNotation repr => Bar repr
abioueka_djembe_call = 
    [ [ I tone_flam,    I tone,         I tone   ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I tone,         I period ]
    , [ I tone,         I period,       I period ] 
    ]
    



--------------------------------------------------------------------------------
-- dummy

-- experiments combining /Tagless/ classes...

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
   cslap :: repr

instance CSlap G where
   cslap = G $ fullstop

group2a :: (CSangban repr, CSlap repr) => Group repr 
group2a = [ I sangban, I sangban, I cslap ]

class (CStroke repr, CSangban repr, CSlap repr) => Composite repr

instance Composite G

group2b :: Composite repr => Group repr 
group2b = [ I $ accent sangban, I sangban, I cslap, S sangban ]

group3 :: Composite repr => Group repr 
group3 = [ I sangban, I sangban, S sangban, I sangban ] 