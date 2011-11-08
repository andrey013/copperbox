{-# OPTIONS -Wall #-}

module Demo01 where

import PDSS.Core.Colour
import PDSS.Core.Monad

import Data.Bits
import Data.Word

demo01 :: IO ()
demo01 = writeFile "demo01.pd" $ run (467,185,466,155) 12 $ do 
    text 94 44 "hello pd"
    b1 <- bang 20 20 
    return ()




demo02 :: IO ()
demo02 = writeFile "demo02.pd" $ run (467,185,420,360) 12 $ do 
    canvas 10 10  30 60 black
    canvas 10 90  30 60 red
    canvas 10 170 30 60 green
    canvas 10 250 30 60 blue
    return ()



fromI :: Int -> RGBi
fromI i = RGBi r g b
  where
    w32 :: Word32
    w32 = fromIntegral $  (-1) - i
    r   = fromIntegral $ hiRes (w32 `shiftR` 12)
    g   = fromIntegral $ hiRes (w32 `shiftR` 6)
    b   = fromIntegral $ hiRes w32 

-- | Go from 8 bit to 6 bit - range [0..255] changes to [0..63].
--
hiRes :: Word32 -> Word8
hiRes i = floor $ (255.0 :: Double) * ((fromIntegral $ 0x3f .&. i) / 63.0)



-- | Go from 8 bit to 6 bit - range [0..255] changes to [0..63].
--
lowRes :: Word8 -> Word32
lowRes i = floor $ (63.0 :: Double) * ((fromIntegral i) / 255.0)


test00 = fromI (-266304)

-- black
test01 = fromI (-1)

-- red-ish
test02 = fromI (-258113)

-- green-ish
test03 = fromI (-4034)

-- blue-ish
test04 = fromI (-4160)



