{-# OPTIONS -Wall #-}

module FontPic where

import Wumpus.Basic.SafeFonts
import Wumpus.Basic.Colour.SVGColours ( steel_blue )
import Wumpus.Basic.Colour.X11Colours ( indian_red1 )
import Wumpus.Basic.PictureLanguage

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


makeFontLabel :: RGBi -> FontAttr -> DPoint2 -> DPrimElement
makeFontLabel rgb fa = textlabel rgb fa msg
  where
    msg = unwords [ font_name $ font_face fa, (show $ font_size fa) ++ "pt"]

blueLabel :: FontFace -> Int -> DPoint2 -> DPrimElement
blueLabel ff i = makeFontLabel steel_blue (FontAttr i ff)

redLabel :: FontFace -> Int -> DPoint2 -> DPrimElement
redLabel ff i = makeFontLabel indian_red1 (FontAttr i ff)


point_sizes :: [Int]
point_sizes = [10, 12, 18, 24, 36, 48]

--------------------------------------------------------------------------------
-- Times

times_pic :: Picture Double
times_pic = timesroman_pic `nextToV` timesitalic_pic     `nextToV` timesbold_pic
                           `nextToV` timesbolditalic_pic



timesroman_pic :: Picture Double
timesroman_pic = 
    frame $ zipWith (blueLabel timesRoman) point_sizes (mkPoints 1.5)

timesitalic_pic :: Picture Double
timesitalic_pic = 
    frame $ zipWith (redLabel timesItalic) point_sizes (mkPoints 1.5)

timesbold_pic :: Picture Double
timesbold_pic = 
    frame $ zipWith (blueLabel timesBold) point_sizes (mkPoints 1.5)

timesbolditalic_pic :: Picture Double
timesbolditalic_pic = 
    frame $ zipWith (redLabel timesBoldItalic) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------
helvetica_pic :: Picture Double
helvetica_pic = vcat helvetica_pic1 [ helveticaoblique_pic  
                                    , helveticabold_pic
                                    , helveticaboldoblique_pic ]
                     

helvetica_pic1 :: Picture Double
helvetica_pic1 = 
    frame $ zipWith (blueLabel helvetica) point_sizes (mkPoints 1.5)

helveticaoblique_pic :: Picture Double
helveticaoblique_pic = 
    frame $ zipWith (redLabel helveticaOblique) point_sizes (mkPoints 1.5)
    
helveticabold_pic :: Picture Double
helveticabold_pic = 
    frame $ zipWith (blueLabel helveticaBold) point_sizes (mkPoints 1.5)
    
helveticaboldoblique_pic :: Picture Double
helveticaboldoblique_pic = 
    frame $ zipWith (redLabel helveticaBoldOblique) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------

courier_pic :: Picture Double
courier_pic = vcat courier_pic1 [ courieroblique_pic
                                , courierbold_pic
                                , courierboldoblique_pic ]
    
courier_pic1 :: Picture Double
courier_pic1 = 
    frame $ zipWith (blueLabel courier) point_sizes (mkPoints 1.5)
    
courieroblique_pic :: Picture Double
courieroblique_pic = 
    frame $ zipWith (redLabel courierOblique) point_sizes (mkPoints 1.5)
    
courierbold_pic :: Picture Double
courierbold_pic = 
    frame $ zipWith (blueLabel courierBold) point_sizes (mkPoints 1.5)
    
courierboldoblique_pic :: Picture Double
courierboldoblique_pic = 
    frame $ zipWith (redLabel courierBoldOblique) point_sizes (mkPoints 1.5)

--------------------------------------------------------------------------------

    
symbol_pic :: Picture Double
symbol_pic = 
    frame $ zipWith (blueLabel symbol) point_sizes (mkPoints 1.5)


--------------------------------------------------------------------------------


mkPoints :: Num u => u -> [Point2 u]
mkPoints n = unfoldr phi zeroPt where
  phi pt = Just (pt, pt .+^ (V2 0 15 ^* n))
