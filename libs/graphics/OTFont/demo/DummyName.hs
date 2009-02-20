{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- ghci> :set -i../src


module DummyName where

import Graphics.OTFont
import Graphics.OTFont.FontLoader
import Graphics.OTFont.ParserCombinators
import Graphics.OTFont.View.Name

import Graphics.OTFont.View.Relation4
import Graphics.OTFont.Glyph.GlyphNaming

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Error
import Data.Array.Unboxed
import Text.PrettyPrint.Leijen  hiding ( (<$>) )



process :: FontLoader r (NameTable,PostTable)
process = do
    Just (name  :: NameTable)   <- decodeTable "name"
    Just (post  :: PostTable)   <- decodeTable "post"
    return (name,post)
{-    
    let glyph_count     = num_glyphs maxp
    let storage = index_to_loc_format headt
    loca  <- decodeTable "loca" (readLocaTable storage (fromIntegral glyph_count)) loader
    -- glyf  <- decodeTable "glyf" (readGlyf 0 112) face
    glyf  <- decodeTable "glyf" (readGlyf 102332 596) loader
    return (loader,maxp,loca,glyf)
-}    
    

                        
tt_sample = "./data/GenBasR.ttf"
ot_sample = "./data/lavoisier.otf"
arabic_sample = "./data/ScheherazadeRegOT.ttf"

demo01 :: IO ()    
demo01 = do 
    ans  <- runFontLoader tt_sample process
    case ans of 
      Left err_msg -> putStrLn err_msg
      Right (name,post)   -> do 
          let nm = buildNameRel name
          mapM_ putStrLnSafe $ allNames nm
          print $ pretty post
          print $ bounds $ glyph_name_index $ post_subtable post
          print $ ppStringSequence $ buildGlyphNameMap post

