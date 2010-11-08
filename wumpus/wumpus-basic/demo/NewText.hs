{-# OPTIONS -Wall #-}


module NewText where

import NimbusRomanMetricsGPL

import Wumpus.Basic.Colour.SVGColours
import Wumpus.Basic.Graphic
import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Text.Advance
import Wumpus.Basic.Text.Datatypes
import Wumpus.Basic.Text.LocBoundingBox

import Wumpus.Core                      -- package: wumpus-core

import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import Prelude hiding ( pi )

import System.Directory

dummy :: CharBoundingBox AfmUnit
dummy = charBoundingBox (-115) (-240) 1151 1009

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    demo01

demo01 :: IO ()
demo01 = do 
    writeEPS_latin1 "./out/new_text01.eps" pic1
    writeSVG_latin1 "./out/new_text01.svg" pic1

std_ctx :: DrawingContext
std_ctx = fontface times_roman $ standardContext 18

testString :: FromPtSize u
           => String -> PtSize -> AfmCharMetricsTable -> AdvanceSingle u
testString ss sz cm = makeSingle bbox av (textline ss)
  where
    av     = getAdvanceVec $ stringVector sz cm ss 
    width  = vector_x av
    bbox   = oLocBoundingBox width (afmValue (glyph_max_height cm) sz)

stringVector :: FromPtSize u 
             => PtSize -> AfmCharMetricsTable -> String -> AdvanceVec u
stringVector sz cm ss = 
   foldr (\c v -> appendAdvanceVec v $ charVector sz cm c) (advanceVec 0 0) ss


charVector :: FromPtSize u 
           => PtSize -> AfmCharMetricsTable -> Char -> AdvanceVec u
charVector sz cm c = scaleVector sz $ lookupCodePoint (ord c) cm


scaleVector :: FromPtSize u 
            => PtSize -> Vec2 AfmUnit -> AdvanceVec u
scaleVector sz (V2 x y) = advanceVec (afmValue x sz) (afmValue y sz)

lookupCodePoint :: CodePoint -> AfmCharMetricsTable -> Vec2 AfmUnit
lookupCodePoint n t = 
    fromMaybe (default_adv_vec t) $ Map.lookup n (char_adv_vecs t)



testLine :: Fractional u => Int -> AdvanceSingle u 
testLine n = makeSingle bbox av (straightLine av)
  where
    width  = fromIntegral n * 12
    bbox   = oLocBoundingBox width 10
    av     = hvec width 


type CatF u = AdvanceMulti u -> AdvanceMulti u -> AdvanceMulti u

dummyText :: (Fractional u, Ord u, FromPtSize u) 
          => CatF u -> AdvanceMulti u
dummyText op = (((mk1 "One Two Three" `op` mk1 "Four") 
                  `op` mk1 "Five") `op` mk1 "Six") `op` mk1 "Seven"
  where
    mk1 ss = oneLineH $ testString ss ptsize nimbus_metrics
    ptsize = fromIntegral $ font_size $ font_props std_ctx


pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ do
          drawi_ $ right_text  `at` P2 0 200
          drawi_ $ left_text   `at` P2 0 100
          drawi_ $ center_text `at` P2 0   0



right_text :: (Fractional u, Ord u, FromPtSize u) 
           => LocImage u (BoundingBox u)
right_text = localize (strokeColour red) $ 
               runAdvanceMulti (dummyText $ alignRightH 16)

left_text :: (Fractional u, Ord u, FromPtSize u) 
          => LocImage u (BoundingBox u)
left_text = localize (strokeColour green) $ 
               runAdvanceMulti (dummyText $ alignLeftH 16)

center_text :: (Fractional u, Ord u, FromPtSize u) 
            => LocImage u (BoundingBox u)
center_text = localize (strokeColour blue) $ 
               runAdvanceMulti (dummyText $ alignCenterH 16)


{-

pic1 :: DPicture
pic1 = liftToPictureU $ execTraceDrawing std_ctx $ do
          drawi_ $ right_text  `at` P2 0 200
          drawi_ $ left_text   `at` P2 0 100
          drawi_ $ center_text `at` P2 0   0


dummyText :: (Fractional u, Ord u) => CatF u -> AdvanceMulti u
dummyText op = (((mk1 10 `op` mk1 4) `op` mk1 5) `op` mk1 6) `op` mk1 4
  where
    mk1   = oneLineH . testLine


right_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
right_text = localize (strokeColour red) $ 
               runAdvanceMulti (dummyText $ alignRightH 16)

left_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
left_text = localize (strokeColour green) $ 
               runAdvanceMulti (dummyText $ alignLeftH 16)

center_text :: (Fractional u, Ord u) => LocImage u (BoundingBox u)
center_text = localize (strokeColour blue) $ 
               runAdvanceMulti (dummyText $ alignCenterH 16)


-}