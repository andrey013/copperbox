{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OTFont.SFont.GlyphDecoder
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

module Graphics.SFont.GlyphDecoder ( 
  simpleGlyphContours, 
  compositeElements
) where


import Graphics.SFont.KangarooAliases
import Graphics.SFont.Syntax

import Data.ParserCombinators.KangarooWriter

import Control.Applicative
import Data.Bits
import Data.Int
import Data.Word


--------------------------------------------------------------------------------
-- Simple glyphs


data CurvePos = COn | COff deriving (Eq,Show)

data Inst a b = Inst CurvePos a b deriving (Eq,Show)

simpleGlyphContours :: [UShort] -> [Byte] -> [Byte] -> [Contour]
simpleGlyphContours end_pts flags xy_data = 
    map Contour $ endSegment end_pts $ decodeAllPoints flags xy_data   


decodeAllPoints :: [Word8] -> [Word8] -> [OutlinePoint]
decodeAllPoints flags xy_data = 
    let insts     = decodeStep1 flags 
        (is,ws)   = decodeXCoords insts xy_data 
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
    step (u,cca) (Inst c 2 y:xs)    (w:w':ws) = let x = u + i16be w w'
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
    step u (Inst c x 2:ys)    (w:w':ws) = let y   = u + i16be w w'
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

 

-- special version of @segment@ as /ends/ counts from zero (so we have to add
-- 1 to splitAt in the right place).
endSegment :: [UShort] -> [a] -> [[a]]
endSegment []      xs = [xs]
endSegment [i]     xs = let (l,r) = splitAt (1 + fromIntegral i) xs in 
                        if null r then [l] else l:[r] 
endSegment (i:ix)  xs = let (l,r) = splitAt (1 + fromIntegral i) xs 
                        in l : endSegment ix r 

--------------------------------------------------------------------------------
-- Composite glyphs

-- This one is easier in the parse monad...

compositeElements :: Parser [CompositeElement]
compositeElements = do 
    (a,more)  <- compositeElt
    if not more then return [a] else (return a) <:> compositeElements
            
            
compositeElt :: Parser (CompositeElement,Bool)  
compositeElt = do 
    flag  <- ushort
    gidx  <- (fromIntegral <$> ushort)
    args  <- extractArgs (argsAreWords flag) (argmaker $ argsAreXYVals flag) 
    op    <- cond3 (haveScale flag)     (Scale   <$> f2dot14) 
                   (haveXYScale flag)   (XyScale <$> f2dot14 <*> f2dot14) 
                   (have2x2 flag)       twoByTwo
                   NoTrans
    return (CompositeElement gidx args op, moreComponents flag)                   
  where
    extractArgs True f = (\a b -> f (fromIntegral a, fromIntegral b)) <$>
                            short <*> short
                          
    extractArgs _    f = (\a b -> f (fromIntegral a, fromIntegral b)) <$>
                            byte <*> byte                          
    
    argmaker True (x,y) = OffsetArgs x y  
    argmaker _    (x,y) = PointNumbers x y 
    
    twoByTwo :: Parser CompositeTrans
    twoByTwo = TwoByTwo <$> f2dot14 <*> f2dot14 <*> f2dot14 <*> f2dot14
    
cond3 :: Monad m => Bool -> m a -> Bool -> m a -> Bool -> m a -> a -> m a
cond3 pred1 f1 pred2 f2 pred3 f3 deft
    | pred1     = f1
    | pred2     = f2
    | pred3     = f3
    | otherwise = return deft
  
    

argsAreWords      :: UShort -> Bool
argsAreWords      = testBit `flip` 0

argsAreXYVals     :: UShort -> Bool
argsAreXYVals     = testBit `flip` 1

haveScale         :: UShort -> Bool
haveScale         = testBit `flip` 3

moreComponents    :: UShort -> Bool
moreComponents    = testBit `flip` 5

haveXYScale       :: UShort -> Bool
haveXYScale       = testBit `flip` 6

have2x2           :: UShort -> Bool
have2x2           = testBit `flip` 7


