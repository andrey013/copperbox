{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- ghci> :set -i../src


module DummyGlyf where

import Graphics.OTFont
import Graphics.OTFont.FontLoader
import Graphics.OTFont.Table.GlyphTables
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.ParseMonad ( ParseError(..), setRange ) 

import Graphics.OTFont.Glyph.Datatypes
import Graphics.OTFont.Glyph.GlyphDecoder
import Graphics.OTFont.Glyph.GlyphNaming


import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error
import Text.PrettyPrint.Leijen  hiding ( (<$>) )



process :: FontLoader r (MaxpTable, HeadTable, LocaTable, NameTable)
process = do
    Just (maxp  :: MaxpTable)   <- decodeTable "maxp"
    Just (headt :: HeadTable)   <- decodeTable "head"
    Just (loca  :: LocaTable)   <- decodeTable "loca"
    Just (name  :: NameTable)   <- decodeTable "name"
    return (maxp,headt,loca,name)
{-    
    let glyph_count     = num_glyphs maxp
    let storage = index_to_loc_format headt
    loca  <- decodeTable "loca" (readLocaTable storage (fromIntegral glyph_count)) loader
    -- glyf  <- decodeTable "glyf" (readGlyf 0 112) face
    glyf  <- decodeTable "glyf" (readGlyf 102332 596) loader
    return (loader,maxp,loca,glyf)
-}    
    
    
decodeG :: Glyf -> [[OutlinePoint]]
decodeG (SimpleGlyf _ (SimpleGlyphData ends _ _ fs xys)) = outlines ends fs xys    
decodeG _                                                = []


                        
tt_sample = "./data/GenBasR.ttf"
ot_sample = "./data/lavoisier.otf"
arabic_sample = "./data/ScheherazadeRegOT.ttf"

demo01 :: IO ()    
demo01 = do 
    ans  <- runFontLoader tt_sample process
    case ans of 
      Left err_msg                 -> putStrLn err_msg
      Right (maxp,headt,loca,name) -> do 
--          print $ pretty maxp
--          print $ pretty headt
--          print $ pretty loca
          print $ pretty name
 
 --         mapM_ print (decodeG glyf)

