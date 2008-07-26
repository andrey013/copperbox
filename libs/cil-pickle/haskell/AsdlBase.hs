{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}


-- AsdlBase.asdl 

-- Copyright (c) 2007 Stephen Peter Tetley 

-- __MIT License__ 
-- Permission is hereby granted, free of charge, to any person obtaining a    
-- copy of this software and associated documentation files (the "Software"), 
-- to deal in the Software without restriction, including without limitation   
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,    
-- and/or sell copies of the Software, and to permit persons to whom the      
-- Software is furnished to do so, subject to the following conditions:       

-- The above copyright notice and this permission notice shall be included in 
-- all copies or substantial portions of the Software.                        


-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        
-- DEALINGS IN THE SOFTWARE.  


module AsdlBase 
  ( Pickle (..)
  , CM (..)
  , evalCM
  , execCM
  , tagFail
  , pinteger
  , uinteger
  , pstring
  , ustring
  , pmaybe
  , umaybe
  , plist
  , ulist
  , pbool
  , ubool
  , pint
  , uint
  , write_tag  
  , read_tag   
  , writePickle 
  , readPickle
  , pchar
  , uchar
  , pfloat
  , ufloat
  , pint64
  , uint64 
  ) where


import Control.Monad
import Data.Bits
import Data.Int
import qualified Data.ByteString as B
import Data.Char
import Data.Word
import System.IO

-- helpers

kBit :: Word8 -> Bool
kBit a = testBit a 7

sBit :: Word8 -> Bool
sBit a = testBit a 6

kBitOn :: Word8 -> Word8
kBitOn a = setBit a 7

sBitOn :: Word8 -> Word8
sBitOn a = setBit a 6

-- take the rightmost 7 bits; bits 0-6 
right7 :: Word8 -> Word8
right7 a = clearBit a 7

-- take the rightmost 6 bits; bits 0-5 
right6 :: Word8 -> Word8
right6 a = a .&. 0x3f

forward :: Word8 -> Integer -> Int -> Integer
forward a acc shift = acc + ((fromIntegral a) `shiftL` shift)



-- real stuff

                  
--------------------------------------------------------------------------------
-- Long winded forms
--------------------------------------------------------------------------------
type CMst = (B.ByteString,Int)

integerWrite :: Integer -> CMst -> CMst
integerWrite i (bs,pos)
  | abs i <= 63 = let n = fromIntegral (abs i) in 
                    case i < 0 of
                      True -> (bs `B.snoc` (sBitOn n), pos+1)
                      False -> (bs `B.snoc` n,pos+1)
  | otherwise   = let n = kBitOn $ fromIntegral $ i .&. 0x7f in
                  integerWrite (i `shiftR` 7) (bs `B.snoc` n, pos+1)
                  
integerRead :: CMst -> (Integer, CMst)
integerRead (bs,pos) = rec bs 0 0 (pos+1)
  where 
    rec bs n s z 
      | B.null bs = error $ "integerI - input stream empty"
      | otherwise = let c = B.head bs
                        cs = B.tail bs in
                    case kBit c of
                      True -> rec cs (forward (right7 c) n s) (s+7) (z+1)
                      False -> case sBit c of
                                True -> (negate $ forward (right6 c) n s, (cs, z))
                                False -> (forward (right6 c) n s, (cs, z))

textWrite :: String -> CMst -> CMst 
textWrite s st = let st' = integerWrite (fromIntegral $ length s) st
                 in foldl fn st' s
  where
    fn (bs,pos) ch = (bs `B.snoc` (fromIntegral $ ord ch), pos+1)                            

textRead :: CMst -> (String,CMst)
textRead st = let (len,(bs,pos)) = integerRead st
                  (a,bs') = B.splitAt (fromIntegral len) bs
              in (cnv a, (bs',pos + fromIntegral len))
  where cnv :: B.ByteString -> String
        cnv = B.foldr (\w acc -> (chr $ fromIntegral w) : acc ) []  
       
  

--------------------------------------------------------------------------------
-- The 'consume' monad
--------------------------------------------------------------------------------

newtype CM a =  CM { runCM :: CMst -> (a, CMst) }
  


instance Monad CM where
  return a = CM (\s -> (a,s))
  (CM {runCM = sm0}) >>= f = CM $ \s0 -> 
    let (a1, s1)  = sm0 s0
        (CM sm1) = f a1
        (a2,s2)  = sm1 s1
    in (a2,s2)


evalCM :: CM a -> CMst -> a
evalCM cmd = fst . (runCM cmd)


execCM :: CM a -> CMst -> CMst
execCM cmd = snd . (runCM cmd)

tagFail :: Int -> String -> CM a
tagFail i ss = do
  (_,pos) <- getstate
  error $ "tag " ++ show i ++ " failed for " ++ ss ++ " at position " ++ show pos

  
class Pickle a where
  pickle   :: a -> CM ()
  unpickle :: CM a


-- need an 'ask' on the state

getstate = CM $ \s -> (s,s)
updstate  s = CM $ \s -> ((), s)  


pinteger :: Integer -> CM ()
pinteger i = CM $ \st -> let st' = integerWrite i st
                         in ((), st')
                      
uinteger :: CM Integer
uinteger = CM $ \st -> let (a,st') = integerRead st
                       in (a,st')


pstring :: String -> CM ()
pstring ss = CM $ \st -> let st' = textWrite ss st
                         in ((),st')

ustring :: CM String             
ustring = CM $ \st -> let (a,st') = textRead st
                       in (a,st') 



pmaybe :: (a -> CM ()) -> (Maybe a) -> CM ()
pmaybe f Nothing  = pinteger 0 
pmaybe f (Just a) = do 
  pinteger 1 
  f a
  return ()
 

umaybe :: CM a -> CM (Maybe a)
umaybe (CM {runCM = f}) = CM $ \st -> 
  let (i,st')  = integerRead st
      (a,st'') = advance i st'
  in (a,st'')
  where advance 0 st = (Nothing, st)
        advance 1 st = let (a,st') = f st
                       in (Just a,st')
        advance i st = error $ "umaybe - tag was " ++ show i

pint :: Int -> CM ()
pint i = pinteger (fromIntegral i)
                 
uint :: CM Int
uint = do 
  i <- uinteger
  return (fromIntegral i)

         

plist :: (a -> CM ()) -> [a] -> CM ()      
plist f xs = do
  pint (length xs)
  mapM_ f xs

ulist :: CM a -> CM [a]
ulist f = do
  i <- uint
  replicateM i f  
          
pbool :: Bool -> CM ()
pbool True = pinteger (fromIntegral 1)
pbool False = pinteger (fromIntegral 0)

ubool :: CM Bool
ubool = do
  i <- uinteger 
  case i of
    0 -> return False
    1 -> return True

pchar :: Char -> CM ()
pchar c = do 
  pinteger 1
  pint (ord c)

uchar :: CM Char
uchar = do
  i <- uinteger 
  case i of
    1 -> do { s <- uint; return (chr s) }
    x -> error $ "uchar - tag was " ++ show x
    



pint64 :: Int64 -> CM ()
pint64 i = do 
  pinteger 1
  pstring (show i)

uint64 :: CM Int64
uint64 = do
  i <- uinteger 
  case i of
    1 -> do { s <- ustring; return (read s) } 


pfloat :: Float -> CM ()
pfloat n = do 
  pinteger 1
  pstring (show n)

ufloat :: CM Float
ufloat = do
  i <- uinteger 
  case i of
    1 -> do { s <- ustring; return (read s) } 
    x -> error $ "ufloat - tag was " ++ show x
      
  
read_tag :: CM Int
read_tag = do 
  i <- uinteger
  return (fromIntegral i)

write_tag :: Int -> CM ()
write_tag i = pinteger (fromIntegral i)

instance Pickle Integer where
  pickle   = pinteger
  unpickle = uinteger

instance Pickle String where
  pickle    = pstring
  unpickle  = ustring  
  
  
instance Pickle Int where
  pickle i = pinteger (fromIntegral i)
  unpickle = do i <- uinteger
                return (fromIntegral i)
                       
instance (Pickle a) => Pickle (Maybe a) where
  pickle            = pmaybe pickle
  unpickle          = umaybe unpickle


instance (Pickle a) => Pickle [a] where
  pickle            = plist pickle
  unpickle          = ulist unpickle
                            


writePickle :: FilePath -> (t -> CM a) -> t -> IO ()
writePickle fpath fn a = B.writeFile fpath bs
  where bs = fst $ execCM (fn a) (B.empty,0)
  

readPickle :: FilePath -> CM a -> IO a
readPickle filepath fn = do
  bs <- B.readFile filepath
  return (evalCM fn (bs,0))




--------------------------------------------------------------------------------
-- for testing 
--------------------------------------------------------------------------------


-- Note: running pickle >> unpickle will give double 
-- the pickle length for position...


demo01 = runCM (pstring "hello" >> ustring) (B.empty,0)


demo02 :: (Maybe Int, CMst)
demo02 = runCM (pickle m1 >> unpickle) (B.empty,0)
  where m1 :: Maybe Int
        m1 = Just 1
        cmd :: CM (Maybe Int)
        cmd = do a <- unpickle
                 return a 
                 
demo03 :: ((), CMst)                
demo03  = runCM (pickle (Just "hello")) (B.empty,0)


demo04 :: (Maybe String, CMst) 
demo04 = runCM (umaybe ustring) (mkBString ('\SOH':'\ENQ':"hello"),0)

demo05 :: (Maybe String, CMst) 
demo05 = runCM (unpickle) (mkBString ('\SOH':'\ENQ':"hello"),0)
 
demo06 :: ([String], CMst) 
demo06 = runCM (plist pstring ls1 >> ulist ustring) (B.empty,0)
  where ls1 = ["one", "two", "three"]

demo07 :: ([String], CMst) 
demo07 = runCM (pickle ls1 >> unpickle) (B.empty,0)
  where ls1 = ["one", "two", "three"]

mkBString :: [Char] -> B.ByteString
mkBString = B.pack . map (fromIntegral . ord)


        











                
  