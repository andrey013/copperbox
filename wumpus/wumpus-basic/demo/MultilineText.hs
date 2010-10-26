{-# OPTIONS -Wall #-}

module MultilineText where

import Wumpus.Basic.Anchors
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Shapes

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space


-- Note - [0,2,4,6 ... ]
demo00 :: [Int]
demo00 = take 10 $ iterate (+2) 0 


main :: IO ()
main = do 
    writeEPS_latin1 "./out/multiline_text.eps" pic1
    writeSVG_latin1 "./out/multiline_text.svg" pic1


times_roman_ctx :: DrawingContext
times_roman_ctx = fontface times_roman $ standardContext 24

-- Note - for multiline text 1.2 * font size seems good vertical spacing
--
pic1 :: DPicture
pic1 = liftToPictureU $ execDrawing times_roman_ctx $ do
    draw $ textline ss `at` (zeroPt .+^ vvec (1.2 * 24))
    draw $ textline ss `at` zeroPt
    draw $ textlineMulti xs `at` (P2 500 0)
    let m = regularMargin 6 (-24)
    a <- drawi $ drawText $ rotate30About (P2 50 120) $ 
                 mplaintext2 m [(10,"The quick"), (0,"black dog")] `at` P2 50 120
    drawi_ $ coordinateDot $ coordinate `at` P2 50 120
    drawi_ $ coordinateX $ coordinate `at` (north a)
    drawi_ $ coordinateX $ coordinate `at` (south a)
    drawi_ $ coordinateX $ coordinate `at` (east a)
    drawi_ $ coordinateX $ coordinate `at` (west a)
    return ()
  where
    ss = "The quick brown fox jumped over the lazy dog."
    xs = ["The quick", "brown fox", "jumped over", "the lazy", "dog."]



mplaintext1 :: Num u => BoxMargin u -> String -> LocPlaintext u
mplaintext1 a ss = setMargin a . plaintext ss

mplaintext2 :: Num u => BoxMargin u -> [(u,String)] -> LocPlaintext u
mplaintext2 a xs = setMargin a . multitext xs 