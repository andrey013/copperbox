{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module AsdlBase 
  ( Pickle (..)
  , CM (..)
  , evalCM
  , execCM
  , pinteger
  , uinteger
  , pint
  , uint
  , pstring
  , ustring
  , pmaybe
  , umaybe
  , write_tag  
  , read_tag   
  , writePickle 
  , readPickle 
  ) where


import Control.Monad
import Data.Bits
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

integerWrite :: Integer -> B.ByteString -> B.ByteString
integerWrite i bs
  | abs i <= 63 = let n = fromIntegral (abs i) in 
                    case i < 0 of
                      True -> bs `B.snoc` (sBitOn n)
                      False -> bs `B.snoc` n
  | otherwise   = let n = kBitOn $ fromIntegral $ i .&. 0x7f in
                  integerWrite (i `shiftR` 7) (bs `B.snoc` n)
                  
integerRead :: B.ByteString -> (Integer, B.ByteString)
integerRead bs = rec bs 0 0
  where 
    rec bs n s  
      | B.null bs = error $ "integerI - input stream empty"
      | otherwise = let c = B.head bs
                        cs = B.tail bs in
                    case kBit c of
                      True -> rec cs (forward (right7 c) n s) (s+7)
                      False -> case sBit c of
                                True -> (negate $ forward (right6 c) n s, cs)
                                False -> (forward (right6 c) n s, cs)

textWrite :: String -> B.ByteString -> B.ByteString 
textWrite s bs = foldl fn bs s
  where
    fn acc ch = acc `B.snoc` (fromIntegral $ ord ch)                            


--------------------------------------------------------------------------------
-- The 'consume' monad
--------------------------------------------------------------------------------

type BSt = B.ByteString

newtype CM a =  CM { runCM :: BSt -> (a, BSt) }
  


instance Monad CM where
  return a = CM (\s -> (a,s))
  (CM {runCM = sm0}) >>= f = CM $ \s0 -> 
    let (a1, s1)  = sm0 s0
        (CM sm1) = f a1
        (a2,s2)  = sm1 s1
    in (a2,s2)


evalCM :: CM a -> BSt -> a
evalCM cmd = fst . (runCM cmd)


execCM :: CM a -> BSt -> BSt
execCM cmd = snd . (runCM cmd)

  
class Pickle a where
  pickle   :: a -> CM ()
  unpickle :: CM a


-- need an 'ask' on the state

getstream = CM $ \s -> (s,s)
updstream  s = CM $ \s -> ((), s)  

pinteger :: Integer -> CM ()
pinteger i = CM $ \bs -> let bs' = integerWrite i bs
                         in ((), bs')
-- or
pi2 i = do 
  bs <- getstream
  let bs' = integerWrite i bs
  updstream  bs'

uinteger :: CM Integer
uinteger = CM $ \bs -> let (a,bs') = integerRead bs
                       in (a,bs')


pstring :: String -> CM ()
pstring ss = CM $ \bs -> let bs'  = integerWrite (fromIntegral $ length ss) bs
                             bs'' = textWrite ss bs'
                         in ((),bs'')

ustring :: CM String             
ustring = do
  i <- uinteger
  s <- text i
  return s
  where 
    text :: Integer -> CM String
    text n = CM $ \bs -> let (a,bs') = B.splitAt (fromIntegral n) bs
                         in (cnv a,bs')
                         
    cnv :: B.ByteString -> String
    cnv = B.foldr (\w acc -> (chr $ fromIntegral w) : acc ) []  



pmaybe :: (a -> CM ()) -> (Maybe a) -> CM ()
pmaybe f Nothing  = pinteger 0 
pmaybe f (Just a) = do 
  pinteger 1 
  f a
  return ()
 

umaybe :: CM a -> CM (Maybe a)
umaybe (CM {runCM = f}) = CM $ \bs -> 
  let (i,bs')  = integerRead bs
      (a,bs'') = advance i bs'
  in (a,bs'')
  where advance 0 bs = (Nothing, bs)
        advance 1 bs = let (a,bs') = f bs
                       in (Just a,bs')
        advance i bs = error $ "umaybe - tag was " ++ show i

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
          
punit :: () -> CM ()
punit () = pinteger 0

uunit = do
  i <- uinteger 
  return ()


read_tag :: CM Int
read_tag = do 
  i <- uinteger
  return (fromIntegral i)

write_tag :: Int -> CM ()
write_tag i = pinteger (fromIntegral i)

pbool :: Bool -> CM ()
pbool True = pinteger (fromIntegral 1)
pbool False = pinteger (fromIntegral 0)

ubool :: CM Bool
ubool = do
  i <- uinteger 
  case i of
    0 -> return False
    1 -> return True

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
  where bs = execCM (fn a) B.empty
  

readPickle :: FilePath -> CM a -> IO a
readPickle fpath fn = do
  bs <- B.readFile fpath
  return (evalCM fn bs)




--------------------------------------------------------------------------------
-- for testing 
--------------------------------------------------------------------------------


             
demo01 = runCM (pstring "hello" >> ustring) B.empty

demo02 :: (Maybe Int, BSt)
demo02 = runCM (pickle m1 >> unpickle) B.empty
  where m1 :: Maybe Int
        m1 = Just 1
        cmd :: CM (Maybe Int)
        cmd = do a <- unpickle
                 return a 
                 
demo03 :: ((), BSt)                
demo03  = runCM (pickle (Just "hello")) B.empty          


demo04 :: (Maybe String, BSt) 
demo04 = runCM (umaybe ustring) (mkBString ('\SOH':'\ENQ':"hello"))

demo05 :: (Maybe String, BSt) 
demo05 = runCM (unpickle) (mkBString ('\SOH':'\ENQ':"hello"))
 
demo06 :: ([String], BSt) 
demo06 = runCM (plist pstring ls1 >> ulist ustring) B.empty
  where ls1 = ["one", "two", "three"]

demo07 :: ([String], BSt) 
demo07 = runCM (pickle ls1 >> unpickle) B.empty
  where ls1 = ["one", "two", "three"]

mkBString :: [Char] -> B.ByteString
mkBString = B.pack . map (fromIntegral . ord)


        











                
  