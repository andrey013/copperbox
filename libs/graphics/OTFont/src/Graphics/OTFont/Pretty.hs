{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Pretty
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print
--
--------------------------------------------------------------------------------


module Graphics.OTFont.Pretty where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Utils

import Data.Array.Unboxed
import Data.Bits ( testBit ) 
import Data.Char
import Data.List
import qualified Data.IntMap as IntMap
import Data.Word 
import Numeric ( showHex )
import Text.PrettyPrint.Leijen



ppMeaning :: Meaning a => a -> Doc
ppMeaning = text . meaning

meaningParensEnum :: (Meaning a, Enum a) => a -> Doc
meaningParensEnum a = text (meaning a) <+> parens (integral $ fromEnum a)



      


prettyThenLine :: Pretty a => a -> Doc
prettyThenLine a = pretty a <> line

ppTable :: String -> [Doc] -> Doc
ppTable s ds = title s <$> vsep ds
  
title :: String -> Doc   
title s = string s <$> uline (length s) where
  uline i = text $ replicate i '-'

field :: String -> Int -> Doc -> Doc
field s i d = fill i (text s) <> equals <+> d
 
integral :: Integral i => i -> Doc
integral = integer . fromIntegral

instance Pretty Fixed where
  pretty = double . unFixed

instance Pretty FWord where
  pretty = integral . unFWord
  
instance Pretty UFWord where
  pretty = integral . unUFWord
  
  
ppBitfield :: Meaning a => [a] -> Doc
ppBitfield = list . map ppMeaning

ppArray :: (IArray arr a, Ix idx, Enum idx) => (a -> Doc) -> arr idx a -> Doc
ppArray f arr = foldl' (\a i -> a <> f (arr!i)) empty xs where
    xs        = let (l,u) = bounds arr in [l..u]

ppArraySep :: (IArray arr a, Ix idx, Enum idx) => 
              (a -> Doc) -> (Doc -> Doc -> Doc) ->  arr idx a -> Doc
ppArraySep f op arr = foldl' (\a i -> a `op` f (arr!i)) empty xs where
    xs        = let (l,u) = bounds arr in [l..u]
    

ppStringSequence :: StringSequence -> Doc
ppStringSequence = foldl' fn empty. IntMap.toAscList where
    fn a (k,v) = a <$> fill 4 (int k) <> colon <+> text v  
    
pchar :: Char -> Doc
pchar ch | isPrint ch   = char ch
         | otherwise    = char '.' 

instance Pretty DateTime where
  pretty _ = text "ugh!" 

pptag :: String -> Doc
pptag s | all isPrint s = text s
        | otherwise     = hcat $ map (f . ord) s 
  where 
    f i | i < 10    = char '0' <> pphex i
        | otherwise = pphex i 

pphex :: Integral i => i -> Doc
pphex = text . (showHex `flip` [])

hex2 :: Integral i => i -> String
hex2 i | i < 16     = '0' : showHex i [] 
       | otherwise  = showHex i []

pphex2 :: Integral i => i -> Doc    
pphex2 = text . hex2  


ppBits :: Word8 -> Doc
ppBits w = hcat $ 
    map (\i -> if w `testBit` i then char '1' else char '0') [0..7]
    
            