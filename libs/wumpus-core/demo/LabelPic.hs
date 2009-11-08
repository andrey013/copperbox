{-# LANGUAGE ScopedTypeVariables        #-}

module LabelPic where

import Wumpus.Core

import Data.FunctionExtras ( (#) )

import Data.AffineSpace

import Data.List                ( mapAccumR )

--------------------------------------------------------------------------------


  
-- The functions here were in Wumpus.Core but they aren't very 
-- good. They need to supplanted with something better.

-- Firefox seems to have a issues with Courier.

labelDefault :: LabelProps
labelDefault = (psBlack, FontAttr "Courier" "Courier New" 10)

frameDefault :: Num u => Frame2 u 
frameDefault = ortho zeroPt

psBlack :: PSColour
psBlack = PSRgb 0 0 0


-- The width guesses by picLabel1 and picLabel are very poor...

picLabel1 :: (Num u, Ord u) => Int -> String -> Picture u
picLabel1 fontsz str = Single (frameDefault, bb) lbl where
  bb  = BBox zeroPt (P2 w (fromIntegral fontsz))
  w   = fromIntegral $ fontsz * length str
  lbl = Label1 labelDefault (Label zeroPt str) 


picLabel :: forall u. (Num u, Ord u) => Int -> Int -> String -> Picture u
picLabel fontsz linespace str = Multi (frameDefault, bb) lbls where
  xs   = lines str
  lc   = length xs
  w    = fromIntegral $ fontsz * (maximum . map length) xs
  h    = fromIntegral $ fontsz * lc + linespace*(lc-1)
  bb   = BBox zeroPt (P2 w h)
  lbls = snd $ mapAccumR fn zeroPt xs
  fn pt ss = let pt' = pt .+^ (V2 (0::u) (fromIntegral $ fontsz + linespace))
             in (pt', Label1 labelDefault (Label pt ss))



drawBounds :: (Num u, Ord u) => Picture u -> Picture u
drawBounds Empty = Empty
drawBounds p     = p `composite` (frame $ cstroke () ph) where
    ph   = vertexPath $ corners $ boundary p

--------------------------------------------------------------------------------

lbl1 :: Picture Double
lbl1 = picLabel 10 3 "Hello\nWorld" {- # setRGBColour aquamarine4 
                                       # setFont "Helvetica" 12 -}



demo1 = writeEPS "label1.eps" (Just ("Times-Roman",10)) lbl1

demo2 = writeEPS "label2.eps" (Just ("Times-Roman",10)) p
  where
    p = lbl1 ->- lbl1 ->- (rotateAbout (pi/4) (center lbl1) lbl1) ->- lbl1

demo3 = do 
    writeEPS "label3.eps" (Just ("Courier",10)) p
    writeSVG "label3.svg" p
  where
    p = (drawBounds lbl1) ->- 
        (drawBounds lbl1) ->- 
        (drawBounds $ rotateAbout (pi/4) (center lbl1) lbl1) ->- 
        (drawBounds lbl1)




demo4 = writeEPS "label4.eps" (Just ("Times-Roman",10)) p
  where
    p =           (drawBounds lbl1) 
        `composite` (drawBounds $ scale 2 2 lbl1)
        `composite` (drawBounds $ scale 3 3 lbl1)


