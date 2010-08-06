{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Basic.SafeFonts
import Wumpus.Basic.SVGColours ( steelBlue )
import Wumpus.Basic.X11Colours ( indianRed1 )
import Wumpus.Deprecated.PictureLanguage

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.List ( unfoldr )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    writeEPS_latin1 "./out/font_courier.eps"   courier_pic
    writeSVG_latin1 "./out/font_courier.svg"   courier_pic
    writeEPS_latin1 "./out/font_times.eps"     times_pic
    writeSVG_latin1 "./out/font_times.svg"     times_pic
    writeEPS_latin1 "./out/font_helvetica.eps" helvetica_pic
    writeSVG_latin1 "./out/font_helvetica.svg" helvetica_pic
    writeEPS_latin1 "./out/font_symbol.eps"    symbol_pic
    writeSVG_latin1 "./out/font_symbol.svg"    symbol_pic


makeFontLabel :: DRGB -> FontAttr -> DPoint2 -> DPrimitive
makeFontLabel c fa = textlabel (c,fa) msg
  where
    msg = unwords [ font_name fa, (show $ font_size fa) ++ "pt"]

blueLabel :: (Int -> FontAttr) -> Int -> DPoint2 -> DPrimitive
blueLabel f i = makeFontLabel steelBlue (f i)

redLabel :: (Int -> FontAttr) -> Int -> DPoint2 -> DPrimitive
redLabel f i = makeFontLabel indianRed1 (f i)


point_sizes :: [Int]
point_sizes = [10, 12, 18, 24, 36, 48]

--------------------------------------------------------------------------------
-- Times

times_pic :: Picture Double
times_pic = timesroman_pic -//- timesitalic_pic -//- timesbold_pic
                           -//- timesbolditalic_pic



timesroman_pic :: Picture Double
timesroman_pic = 
    frameMulti $ zipWith (blueLabel timesRoman) point_sizes (mkPoints 1.5)

timesitalic_pic :: Picture Double
timesitalic_pic = 
    frameMulti $ zipWith (redLabel timesItalic) point_sizes (mkPoints 1.5)

timesbold_pic :: Picture Double
timesbold_pic = 
    frameMulti $ zipWith (blueLabel timesBold) point_sizes (mkPoints 1.5)

timesbolditalic_pic :: Picture Double
timesbolditalic_pic = 
    frameMulti $ zipWith (redLabel timesBoldItalic) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------
helvetica_pic :: Picture Double
helvetica_pic = helvetica_pic1 -//- helveticaoblique_pic -//- helveticabold_pic
                           -//- helveticaboldoblique_pic

helvetica_pic1 :: Picture Double
helvetica_pic1 = 
    frameMulti $ zipWith (blueLabel helvetica) point_sizes (mkPoints 1.5)

helveticaoblique_pic :: Picture Double
helveticaoblique_pic = 
    frameMulti $ zipWith (redLabel helveticaOblique) point_sizes (mkPoints 1.5)
    
helveticabold_pic :: Picture Double
helveticabold_pic = 
    frameMulti $ zipWith (blueLabel helveticaBold) point_sizes (mkPoints 1.5)
    
helveticaboldoblique_pic :: Picture Double
helveticaboldoblique_pic = 
    frameMulti $ zipWith (redLabel helveticaBoldOblique) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------

courier_pic :: Picture Double
courier_pic = courier_pic1 -//- courieroblique_pic -//- courierbold_pic
                           -//- courierboldoblique_pic
    
courier_pic1 :: Picture Double
courier_pic1 = 
    frameMulti $ zipWith (blueLabel courier) point_sizes (mkPoints 1.5)
    
courieroblique_pic :: Picture Double
courieroblique_pic = 
    frameMulti $ zipWith (redLabel courierOblique) point_sizes (mkPoints 1.5)
    
courierbold_pic :: Picture Double
courierbold_pic = 
    frameMulti $ zipWith (blueLabel courierBold) point_sizes (mkPoints 1.5)
    
courierboldoblique_pic :: Picture Double
courierboldoblique_pic = 
    frameMulti $ zipWith (redLabel courierBoldOblique) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------

    
symbol_pic :: Picture Double
symbol_pic = 
    frameMulti $ zipWith (blueLabel symbol) point_sizes (mkPoints 1.5)


--------------------------------------------------------------------------------


mkPoints :: Num u => u -> [Point2 u]
mkPoints n = unfoldr phi zeroPt where
  phi pt = Just (pt, pt .+^ (V2 0 15 ^* n))
