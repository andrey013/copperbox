{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Core
import Wumpus.Extra.PictureLanguage
import Wumpus.Extra.SafeFonts
import Wumpus.Extra.SVGColours ( steelBlue )
import Wumpus.Extra.X11Colours ( indianRed1 )

import Data.AffineSpace
import Data.VectorSpace

import Data.List ( unfoldr )

main :: IO ()
main = do 
    writeEPS_latin1 "./out/font_courier.eps"   courier_pic
    writeSVG_latin1 "./out/font_courier.svg"   courier_pic
    writeEPS_latin1 "./out/font_times.eps"     times_pic
    writeSVG_latin1 "./out/font_times.svg"     times_pic
    writeEPS_latin1 "./out/font_helvetica.eps" helvetica_pic
    writeSVG_latin1 "./out/font_helvetica.svg" helvetica_pic
    writeEPS_latin1 "./out/font_symbol.eps"    symbol_pic
    writeSVG_latin1 "./out/font_symbol.svg"    symbol_pic


makeFontLabel :: DRGB -> FontAttr -> (DPoint2 -> DPrimitive)
makeFontLabel c fa = textlabel (c,fa) msg
  where
    msg = unwords [ font_name fa, (show $ font_size fa) ++ "pt"]

blueLabel :: FontAttr -> (DPoint2 -> DPrimitive)
blueLabel = makeFontLabel steelBlue

redLabel :: FontAttr -> (DPoint2 -> DPrimitive)
redLabel = makeFontLabel indianRed1


--------------------------------------------------------------------------------
-- Times

times_pic :: Picture Double
times_pic = timesroman_pic -//- timesitalic_pic -//- timesbold_pic
                           -//- timesbolditalic_pic



timesroman_pic :: Picture Double
timesroman_pic = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ timesRoman10, timesRoman12, timesRoman18
         , timesRoman24, timesRoman36, timesRoman48 ]

timesitalic_pic :: Picture Double
timesitalic_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ timesItalic10, timesItalic12, timesItalic18
         , timesItalic24, timesItalic36, timesItalic48 ]


timesbold_pic :: Picture Double
timesbold_pic = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ timesBold10, timesBold12, timesBold18
         , timesBold24, timesBold36, timesBold48 ]

timesbolditalic_pic :: Picture Double
timesbolditalic_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ timesBoldItalic10, timesBoldItalic12, timesBoldItalic18
         , timesBoldItalic24, timesBoldItalic36, timesBoldItalic48 ]

--------------------------------------------------------------------------------
helvetica_pic :: Picture Double
helvetica_pic = helvetica_pic1 -//- helveticaoblique_pic -//- helveticabold_pic
                           -//- helveticaboldoblique_pic

helvetica_pic1 :: Picture Double
helvetica_pic1 = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ helvetica10, helvetica12, helvetica18
         , helvetica24, helvetica36, helvetica48 ]


    
helveticaoblique_pic :: Picture Double
helveticaoblique_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ helveticaOblique10, helveticaOblique12, helveticaOblique18
         , helveticaOblique24, helveticaOblique36, helveticaOblique48 ]

    
helveticabold_pic :: Picture Double
helveticabold_pic = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ helveticaBold10, helveticaBold12, helveticaBold18
         , helveticaBold24, helveticaBold36, helveticaBold48 ]

    
helveticaboldoblique_pic :: Picture Double
helveticaboldoblique_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ helveticaBoldOblique10, helveticaBoldOblique12
         , helveticaBoldOblique18, helveticaBoldOblique24
         , helveticaBoldOblique36, helveticaBoldOblique48 ]

--------------------------------------------------------------------------------

courier_pic :: Picture Double
courier_pic = courier_pic1 -//- courieroblique_pic -//- courierbold_pic
                           -//- courierboldoblique_pic
    
courier_pic1 :: Picture Double
courier_pic1 = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ courier10, courier12, courier18
         , courier24, courier36, courier48 ]

    
courieroblique_pic :: Picture Double
courieroblique_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ courierOblique10, courierOblique12, courierOblique18
         , courierOblique24, courierOblique36, courierOblique48 ]

    
courierbold_pic :: Picture Double
courierbold_pic = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ courierBold10, courierBold12, courierBold18
         , courierBold24, courierBold36, courierBold48 ]

    
courierboldoblique_pic :: Picture Double
courierboldoblique_pic = 
    frameMulti $ zipWith redLabel xs (mkPoints 1.5)
  where
    xs = [ courierBoldOblique10, courierBoldOblique12, courierBoldOblique18
         , courierBoldOblique24, courierBoldOblique36, courierBoldOblique48 ]

--------------------------------------------------------------------------------

    
symbol_pic :: Picture Double
symbol_pic = 
    frameMulti $ zipWith blueLabel xs (mkPoints 1.5)
  where
    xs = [ symbol10, symbol12, symbol18, symbol24, symbol36, symbol48 ]


--------------------------------------------------------------------------------


mkPoints :: Num u => u -> [Point2 u]
mkPoints n = unfoldr phi zeroPt where
  phi pt = Just (pt, pt .+^ (V2 0 15 ^* n))
