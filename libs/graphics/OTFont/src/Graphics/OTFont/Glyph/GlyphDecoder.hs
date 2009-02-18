{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.Glyph.GlyphDecoder
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Decode a /simple glyph description/.
--
--------------------------------------------------------------------------------

module Graphics.OTFont.Glyph.GlyphDecoder where

import Graphics.OTFont.Datatypes
import Graphics.OTFont.Glyph.Datatypes
import Graphics.OTFont.ParserCombinators ( mkI16be )

import Data.Bits
import Data.Int
import Data.Word

type FlagSeq = [Word8]




data CurvePos = COn | COff deriving (Eq,Show)

data Inst a b = Inst CurvePos a b deriving (Eq,Show)

decode :: [Word8] -> [Word8] -> [OutlinePoint]
decode fs xys = let insts     = decodeStep1 fs 
                    (is,ws)   = decodeXCoords insts xys 
                in decodeYCoords is ws  


type Step1Inst = Inst Int Int

decodeStep1 :: [Word8] -> [Step1Inst]
decodeStep1 []                     = []
decodeStep1 (x:xs)  | testBit x 3 = let a       = decode1 x
                                        (i,xs') = expandLA xs                          
                                    in a : replicate i a ++ decodeStep1 xs'
                    | otherwise   = decode1 x : decodeStep1 xs   
  where
    decode1 :: Word8 -> Step1Inst
    decode1 w = let m = decodeX w; n = decodeY w in 
                if w `testBit` 0 then Inst COn m n else Inst COff m n

    -- expand lookahead
    expandLA :: [Word8] -> (Int,[Word8])
    expandLA []     = (0,[])
    expandLA (z:zs) = (fromIntegral z, zs)




decodeX :: Word8 -> Int
decodeX w | xShort w && xSame w     = 1
          | xShort w                = (-1)
          |             xSame w     = 0
          | otherwise               = 2
  where
    xShort, xSame :: Word8 -> Bool 
    xShort = testBit `flip` 1
    xSame  = testBit `flip` 4



         
decodeY :: Word8 -> Int
decodeY w | yShort w && ySame w     = 1
          | yShort w                = (-1)
          |             ySame w     = 0
          | otherwise               = 2                    
  where
    yShort, ySame :: Word8 -> Bool 
    yShort = testBit `flip` 2
    ySame  = testBit `flip` 5 

type Step2Inst = Inst Int16 Int

decodeXCoords :: [Step1Inst] -> [Word8] -> ([Step2Inst],[Word8])
decodeXCoords = step (0,[]) 
  where
    step :: (Int16,[Step2Inst]) -> [Step1Inst] -> [Word8] -> ([Step2Inst],[Word8])
    
    step (_,cca) []                 ws        = (reverse cca,ws)
    
    -- 2 = int16 -- take two from the word-stream
    step (u,cca) (Inst c 2 y:xs)    (w:w':ws) = let x = u + mkI16be w w'
                                                    i = Inst c x y
                                                in step (x,i:cca) xs ws
                                            
    -- 1 = word8 and positive - take one from word-stream
    step (u,cca) (Inst c 1 y:xs)    (w:ws)    = let x = u + fromIntegral w
                                                    i = Inst c x y
                                                in step (x,i:cca) xs ws
                                            
    -- 0 = use cached x value - take none
    step (u,cca) (Inst c 0 y:xs)    ws        = let i = Inst c u y
                                                in step (u,i:cca) xs ws  
    
    -- 1 = word8 and negative - take one from word-stream and negate it
    step (u,cca) (Inst c (-1) y:xs) (w:ws)    = let x = u - fromIntegral w
                                                    i = Inst c x y
                                                in step (x,i:cca) xs ws 

    step _       xs                  ws         = error $ msg "decodeXCoords" xs ws
    
msg :: Show a => String -> [a] -> [b] -> String
msg pre []    _    = pre ++ " list1 exhausted"
msg pre _     []   = pre ++ " list2 exhausted"
msg pre (x:_)  _   = pre ++ " " ++ show x                                      





decodeYCoords :: [Step2Inst] -> [Word8] -> [OutlinePoint]
decodeYCoords = step 0
  where
    step :: Int16 -> [Step2Inst] -> [Word8] -> [OutlinePoint]
    step _ []                 _ws       = []
    
    -- 2 = int16 -- take two from the word-stream
    step u (Inst c x 2:ys)    (w:w':ws) = let y   = u + mkI16be w w'
                                              pt  = mkPoint c x y
                                          in pt : step y ys ws
                                           
    -- 1 = word8 and positive - take one from word-stream
    step u (Inst c x 1:ys)    (w:ws)    = let y   = u + fromIntegral w
                                              pt  = mkPoint c x y
                                          in pt : step y ys ws
                                            
    -- 0 = use cached y value - take none
    step u (Inst c x 0:ys)    ws        = let pt  = mkPoint c x u
                                          in pt : step u ys ws  
    
    -- 1 = word8 and negative - take one from word-stream and negate it
    step u (Inst c x (-1):ys) (w:ws)    = let y = u - fromIntegral w
                                              pt  = mkPoint c x y
                                          in pt : step y ys ws 
  
    step _ ys                 ws        = error $ msg "decodeYCoords" ys ws                                             

    mkPoint :: CurvePos -> Int16 -> Int16 -> OutlinePoint
    mkPoint COn  x y                    = OnCurvePt x y
    mkPoint COff x y                    = OffCurvePt x y

outlines :: [UShort] -> [Byte] -> [Byte] -> [[OutlinePoint]]
outlines ends flags xys = endSegment ends $ decode flags xys    

-- special version of @segment@ as /ends/ counts from zero (so we have to add
-- 1 to splitAt in the right place).
endSegment :: [UShort] -> [a] -> [[a]]
endSegment []      xs = [xs]
endSegment [i]     xs = let (l,r) = splitAt (1 + fromIntegral i) xs in 
                        if null r then [l] else l:[r] 
endSegment (i:ix)  xs = let (l,r) = splitAt (1 + fromIntegral i) xs 
                        in l : endSegment ix r 

    