{-# OPTIONS -Wall #-}

module FontTest where

import Wumpus.Core
import Wumpus.Extra


import Data.AffineSpace
import Data.VectorSpace

import Data.List ( unfoldr )

main :: IO ()
main = do 
    writeEPS "./out/font_courier.eps"   Nothing courier_pic
    writeSVG "./out/font_courier.svg"   courier_pic
    writeEPS "./out/font_times.eps"     Nothing times_pic
    writeSVG "./out/font_times.svg"     times_pic
    writeEPS "./out/font_helvetica.eps" Nothing helvetica_pic
    writeSVG "./out/font_helvetica.svg" helvetica_pic
    writeEPS "./out/font_symbol.eps"    Nothing symbol_pic
    writeSVG "./out/font_symbol.svg"    symbol_pic

--------------------------------------------------------------------------------
-- Times

times_pic :: Picture Double
times_pic = timesroman_pic -//- timesitalic_pic -//- timesbold_pic
                           -//- timesbolditalic_pic

timesroman_pic :: Picture Double
timesroman_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel timesRoman10 pt "Times-Roman 10pt" 
    f12 pt = textlabel timesRoman12 pt "Times-Roman 12pt"
    f18 pt = textlabel timesRoman18 pt "Times-Roman 18pt"
    f24 pt = textlabel timesRoman24 pt "Times-Roman 24pt"

timesitalic_pic :: Picture Double
timesitalic_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel timesItalic10 pt "Times-Italic 10pt" 
    f12 pt = textlabel timesItalic12 pt "Times-Italic 12pt"
    f18 pt = textlabel timesItalic18 pt "Times-Italic 18pt"
    f24 pt = textlabel timesItalic24 pt "Times-Italic 24pt"


timesbold_pic :: Picture Double
timesbold_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel timesBold10 pt "Times-Bold 10pt" 
    f12 pt = textlabel timesBold12 pt "Times-Bold 12pt"
    f18 pt = textlabel timesBold18 pt "Times-Bold 18pt"
    f24 pt = textlabel timesBold24 pt "Times-Bold 24pt"

timesbolditalic_pic :: Picture Double
timesbolditalic_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel timesBoldItalic10 pt "Times-BoldItalic 10pt" 
    f12 pt = textlabel timesBoldItalic12 pt "Times-BoldItalic 12pt"
    f18 pt = textlabel timesBoldItalic18 pt "Times-BoldItalic 18pt"
    f24 pt = textlabel timesBoldItalic24 pt "Times-BoldItalic 24pt"

--------------------------------------------------------------------------------
helvetica_pic :: Picture Double
helvetica_pic = helvetica_pic1 -//- helveticaoblique_pic -//- helveticabold_pic
                           -//- helveticaboldoblique_pic

helvetica_pic1 :: Picture Double
helvetica_pic1 = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel helvetica10 pt "Helvetica 10pt" 
    f12 pt = textlabel helvetica12 pt "Helvetica 12pt"
    f18 pt = textlabel helvetica18 pt "Helvetica 18pt"
    f24 pt = textlabel helvetica24 pt "Helvetica 24pt"


    
helveticaoblique_pic :: Picture Double
helveticaoblique_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel helveticaOblique10 pt "Helvetica-Oblique 10pt" 
    f12 pt = textlabel helveticaOblique12 pt "Helvetica-Oblique 12pt"
    f18 pt = textlabel helveticaOblique18 pt "Helvetica-Oblique 18pt"
    f24 pt = textlabel helveticaOblique24 pt "Helvetica-Oblique 24pt"

    
helveticabold_pic :: Picture Double
helveticabold_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel helveticaBold10 pt "Helvetica-Bold 10pt" 
    f12 pt = textlabel helveticaBold12 pt "Helvetica-Bold 12pt"
    f18 pt = textlabel helveticaBold18 pt "Helvetica-Bold 18pt"
    f24 pt = textlabel helveticaBold24 pt "Helvetica-Bold 24pt"

    
helveticaboldoblique_pic :: Picture Double
helveticaboldoblique_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel helveticaBoldOblique10 pt "Helvetica-Bold-Oblique 10pt" 
    f12 pt = textlabel helveticaBoldOblique12 pt "Helvetica-Bold-Oblique 12pt"
    f18 pt = textlabel helveticaBoldOblique18 pt "Helvetica-Bold-Oblique 18pt"
    f24 pt = textlabel helveticaBoldOblique24 pt "Helvetica-Bold-Oblique 24pt"

--------------------------------------------------------------------------------

courier_pic :: Picture Double
courier_pic = courier_pic1 -//- courieroblique_pic -//- courierbold_pic
                           -//- courierboldoblique_pic
    
courier_pic1 :: Picture Double
courier_pic1 = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel courier10 pt "Courier 10pt" 
    f12 pt = textlabel courier12 pt "Courier 12pt"
    f18 pt = textlabel courier18 pt "Courier 18pt"
    f24 pt = textlabel courier24 pt "Courier 24pt"

    
courieroblique_pic :: Picture Double
courieroblique_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel courierOblique10 pt "Courier-Oblique 10pt" 
    f12 pt = textlabel courierOblique12 pt "Courier-Oblique 12pt"
    f18 pt = textlabel courierOblique18 pt "Courier-Oblique 18pt"
    f24 pt = textlabel courierOblique24 pt "Courier-Oblique 24pt"

    
courierbold_pic :: Picture Double
courierbold_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel courierBold10 pt "Courier-Bold 10pt" 
    f12 pt = textlabel courierBold12 pt "Courier-Bold 12pt"
    f18 pt = textlabel courierBold18 pt "Courier-Bold 18pt"
    f24 pt = textlabel courierBold24 pt "Courier-Bold 24pt"

    
courierboldoblique_pic :: Picture Double
courierboldoblique_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel courierBoldOblique10 pt "Courier-Bold-Oblique 10pt" 
    f12 pt = textlabel courierBoldOblique12 pt "Courier-Bold-Oblique 12pt"
    f18 pt = textlabel courierBoldOblique18 pt "Courier-Bold-Oblique 18pt"
    f24 pt = textlabel courierBoldOblique24 pt "Courier-Bold-Oblique 24pt"

--------------------------------------------------------------------------------

    
symbol_pic :: Picture Double
symbol_pic = frameMulti $ zipWith ($) [f10, f12, f18, f24] (mkPoints 1.5)
  where
    f10 pt = textlabel symbol10 pt "Symbol 10pt" 
    f12 pt = textlabel symbol12 pt "Symbol 12pt"
    f18 pt = textlabel symbol18 pt "Symbol 18pt"
    f24 pt = textlabel symbol24 pt "Symbol 24pt"


--------------------------------------------------------------------------------


mkPoints :: Num u => u -> [Point2 u]
mkPoints n = unfoldr phi zeroPt where
  phi pt = Just (pt, pt .+^ (V2 0 15 ^* n))
