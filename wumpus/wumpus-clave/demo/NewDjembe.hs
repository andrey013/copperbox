{-# OPTIONS -Wall #-}

module NewDjembe1 where

import Wumpus.Djembe.Base
import Wumpus.Djembe.Graphic

import Wumpus.Core                      -- package: wumpus-core
import Wumpus.Basic.Colour.SVGColours   -- package: wumpus-basic
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts

import Data.AffineSpace                 -- package: vector-space
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
text_drawing = drawTracing $ do 
   draw $ textline "PTgd..B" `at` zeroPt
   draw $ localize (fillColour black) $ period      `at` P2 200 0
   draw $ localize (fillColour black) $ flamstem    `at` P2 230 0
   draw $ localize (fillColour black) $ dot         `at` P2 230 0
   draw $ localize (fillColour black) $ flamDot     `at` P2 230 0
   draw $ localize (fillColour black) $ evenStems 5 `at` P2 230 0
   draw $ localize (fillColour black) $ smallLetter 'D' `at` P2 250 0

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


-- | To generate output we need a Graphical interpretation.
--
newtype G u = G { unG :: LocGraphic u }


class CSangban repr where
   sangban :: repr

instance (Fractional u, FromPtSize u) => CSangban (G u) where
  sangban = G $ dot

-- This formulation will lead to huge class contexts!

demo1 :: CSangban repr => [repr]
demo1 = [sangban, sangban ]   

group1 :: CSangban repr => Group repr 
group1 = [ I sangban, I sangban, I sangban ]



class CSlap repr where
   slap :: repr
   slapflam  :: repr

instance (Fractional u, FromPtSize u) => CSlap (G u) where
  slap      = G $ letter 'P'
  slapflam  = G $ letter 'P' -- todo

instance CStroke (G u) where
  optional nh = G $ unG nh  
  lead_in  nh = G $ unG nh
  accent   nh = G $ unG nh

group2a :: (CSangban repr, CSlap repr) => Group repr 
group2a = [ I sangban, I sangban, I slap ]

class (CStroke repr, CSangban repr, CSlap repr) => Composite repr

instance (Fractional u, FromPtSize u) => Composite (G u)

group2b :: Composite repr => Group repr 
group2b = [ I $ accent sangban, I sangban, I slap, S sangban ]

group3 :: Composite repr => Group repr 
group3 = [ I slapflam, I sangban, I slap, S sangban ]

-- This is annoying... 
-- group3 must be given an explicit type.
demo2 :: [(Int,Ratio Int)]
demo2 = hspans g3
  where
    g3 :: Group (G Double)
    g3 = group3