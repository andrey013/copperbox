{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Random access parse monad 
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.ParseMonad 
  (
    GenKangaroo
  , ParseErr
  
  , getSt
  , putSt
  , modifySt
  
  , getUserSt
  , putUserSt
  , modifyUserSt

  , askEnv
  , throwErr
  , withSuccess
  , runGenKangaroo

  , reportError
  , substError
  , word8
  , checkWord8
  , opt 
  , position
  , regionEnd
  , atEnd
  , lengthRemaining

  -- * Parse within a /region/.
  , RegionCoda(..)
  , intraparse
  , advance
  , advanceRelative
  , restrict
  , restrictToPos
   
  ) where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Word
import Numeric
import System.IO

type ParseErr = String

type ImageData = IOUArray Int Word8   -- Is Int big enough for index?

data ArrIx = ArrIx { arr_ix_ptr :: !Int, arr_ix_end :: !Int }
  deriving (Eq,Show) 

type St    = ArrIx
type Env   = ImageData
  

-- Kangaroo is not a transformer as IO is always at the 
-- \'bottom\' of the effect stack. Like the original Parsec it is
-- parametric on user state (refered to as ust).
--
newtype GenKangaroo ust a = GenKangaroo { 
          getGenKangaroo :: Env -> St -> ust -> IO (Either ParseErr a, St, ust) }
          

fmapKang :: (a -> b) -> GenKangaroo ust a -> GenKangaroo ust b
fmapKang f (GenKangaroo x) = GenKangaroo $ \env st ust -> 
    x env st ust `bindIO` \(a,st',ust') -> return (fmap f a, st', ust')


instance Functor (GenKangaroo ust) where
    fmap = fmapKang



returnIO :: a -> IO a
returnIO = return

infixl 1 `bindIO`

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)


returnKang :: a -> GenKangaroo st a
returnKang a = GenKangaroo $ \_ st ust -> returnIO (Right a, st, ust)

infixl 1 `bindKang`

bindKang :: GenKangaroo ust a -> (a -> GenKangaroo ust b) -> GenKangaroo ust b
(GenKangaroo x) `bindKang` f = GenKangaroo $ \env st ust -> 
    x env st ust `bindIO` \(ans, st', ust') ->
        case ans of Left err -> returnIO (Left err,st',ust')
                    Right a  -> getGenKangaroo (f a) env st' ust'
 



instance Monad (GenKangaroo ust) where
  return = returnKang
  (>>=)  = bindKang


instance Applicative (GenKangaroo ust) where
  pure = return
  (<*>) = ap

-- I don't think Kangaroo has natural implementations of 
-- Alternative or MonadPlus.
-- My 'proposition' is that the sort of parsing that Kangaroo 
-- intends to provide you always now want you want hence there 
-- is no inbuilt backtracking or support for list-of-successes.


getSt :: GenKangaroo ust St
getSt = GenKangaroo $ \_ st ust -> return (Right st, st, ust)

putSt :: St -> GenKangaroo ust ()
putSt st = GenKangaroo $ \_ _ ust -> return (Right (), st, ust)

modifySt :: (St -> St) -> GenKangaroo ust ()
modifySt f = GenKangaroo $ \_ st ust -> return (Right (), f st, ust)


getUserSt :: GenKangaroo ust ust
getUserSt = GenKangaroo $ \_ st ust -> return (Right ust, st, ust)

putUserSt :: ust -> GenKangaroo ust ()
putUserSt ust = GenKangaroo $ \_ st _ -> return (Right (), st, ust)

modifyUserSt :: (ust -> ust) -> GenKangaroo ust ()
modifyUserSt f = GenKangaroo $ \_ st ust -> return (Right (), st, f ust)



askEnv :: GenKangaroo ust Env
askEnv = GenKangaroo $ \env st ust -> return (Right env, st, ust)


throwErr :: ParseErr -> GenKangaroo ust a
throwErr msg = GenKangaroo $ \_ st ust -> return (Left msg, st, ust)



liftIOAction :: IO a -> GenKangaroo ust a
liftIOAction ma = GenKangaroo $ \_ st ust -> 
    ma >>= \a -> return (Right a, st, ust) 



runGenKangaroo :: GenKangaroo ust a -> ust -> FilePath -> IO (Either ParseErr a,ust)
runGenKangaroo p user_state filename = 
    withBinaryFile filename ReadMode $ \ handle -> 
      do { sz          <- hFileSize handle
         ; arr         <- newArray_ (0,fromIntegral $ sz-1)
         ; rsz         <- hGetArray handle arr  (fromIntegral sz)
         ; (ans,_,ust) <- runP p rsz arr
         ; return (ans,ust)   
         }
  where 
    runP (GenKangaroo x) upper arr = x arr (ArrIx 0 (upper-1)) user_state



--------------------------------------------------------------------------------
-- Helpers

modifyIx :: (Int -> Int) -> GenKangaroo ust ()
modifyIx f = modifySt $ \(ArrIx ix end) -> ArrIx (f ix) end

--------------------------------------------------------------------------------
-- 


reportError :: ParseErr -> GenKangaroo ust a
reportError s = do 
    posn <- getSt
    throwErr $ s ++ posStr posn
  where
    posStr (ArrIx pos end) = concat [ " absolute position "
                                    , show pos
                                    , " (0x" 
                                    , showHex pos []
                                    , "), region length "
                                    , show end
                                    ]


substError :: GenKangaroo ust a -> ParseErr -> GenKangaroo ust a
substError p msg = GenKangaroo $ \env st ust -> 
    (getGenKangaroo p) env st ust >>= \ ans -> 
      case ans of
        (Left _, st', ust')  -> return (Left msg, st', ust')
        okay                 -> return okay


withSuccess :: Bool -> ParseErr -> GenKangaroo ust a -> GenKangaroo ust a
withSuccess False msg _  = throwErr msg
withSuccess True  _   mf = mf


   
word8 :: GenKangaroo ust Word8
word8 = do
    (ArrIx ix end)   <- getSt
    when (ix>end)    (reportError "word8")   -- test emphatically is (>) !
    arr              <- askEnv
    a                <- liftIOAction $ readArray arr ix
    putSt $ ArrIx (ix+1) end
    return a


checkWord8 :: (Word8 -> Bool) -> GenKangaroo ust (Maybe Word8)
checkWord8 check = word8 >>= \ans ->
    if check ans then return $ Just ans
                 else modifyIx (subtract 1) >> return Nothing



-- no 'try' in Kangaroo... 
-- opt is the nearest to it, opt backtracks the cursor onm failure.
opt :: GenKangaroo ust a -> GenKangaroo ust (Maybe a)
opt p = GenKangaroo $ \env st ust -> (getGenKangaroo p) env st ust >>= \ ans -> 
    case ans of
      (Left _, _, ust')    -> return (Right Nothing, st, ust')
      (Right a, st', ust') -> return (Right $ Just a, st', ust')

position :: GenKangaroo ust Int
position = liftM arr_ix_ptr getSt

regionEnd :: GenKangaroo ust Int
regionEnd = liftM arr_ix_end getSt



atEnd :: GenKangaroo ust Bool
atEnd = getSt >>= \(ArrIx ix end) -> return $ ix >= end

lengthRemaining :: GenKangaroo ust Int
lengthRemaining = getSt >>= \(ArrIx ix end) -> 
   let rest = end - ix in if rest < 0 then return 0 else return rest

--------------------------------------------------------------------------------
-- The important ones parsing within a /region/ ...

-- | 'RegionCoda' - three useful final positions:
--
-- 1. dalpunto  - 'from the point'      
-- - Run the parser within a region and return to where you came
--   from.
--
-- 2. alfermata - 'to the stop'    
-- - Run the parser within a region, the cursor remains wherever 
--   the parse finished.
--
-- 3. alfine    - 'to the end'     
-- - Run the parser within a region and jump to the right-end of 
--   the region after the parse.
--
data RegionCoda = Dalpunto | Alfermata | Alfine
  deriving (Enum,Eq,Show)


-- | 'intraparse' - coda x abs_start x length x parser
--
intraparse :: RegionCoda -> Int -> Int 
           -> GenKangaroo ust a 
           -> GenKangaroo ust a
intraparse coda intra_start len p = 
    withLoc "intraparse" intra_start len $ \old_start old_end -> 
      p >>= \ans -> 
      case coda of
        Dalpunto  -> putSt (ArrIx old_start old_end) >> return ans
        Alfermata -> position                  >>= \pos -> 
                     putSt (ArrIx pos old_end) >>  return ans
        Alfine    -> putSt (ArrIx (intra_start+len) old_end) >> return ans
     
             
  
-- Essentially we can go 1+ the old end, this represents the 
-- cursor at the end of file...
--
withLoc :: String -> Int -> Int 
        -> (Int -> Int -> GenKangaroo ust a) 
        -> GenKangaroo ust a
withLoc fun_name new_start len mf = let new_end = new_start + len in
    getSt >>= \(ArrIx pos end) ->
        withSuccess (pos <= new_start) (backwardsError new_start pos fun_name) $
            withSuccess (new_end <= end+1) (forwardsError new_end end fun_name) $ 
                putSt (ArrIx new_start new_end) >> mf pos end


backwardsError :: Int -> Int -> String -> String  
backwardsError new_pos old_pos fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name
    , " - cannot backtrack, " 
    , show new_pos 
    , " is before current position " 
    , show old_pos
    ]

forwardsError :: Int -> Int -> String -> String
forwardsError new_end old_end fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name 
    , " - new end point " 
    , show new_end 
    , " extends beyond the end of the current region "
    , show old_end
    ]

advance :: RegionCoda -> Int-> GenKangaroo ust a -> GenKangaroo ust a
advance coda intra_start p = 
    getSt >>= \(ArrIx _ end) -> intraparse coda intra_start end p

advanceRelative :: RegionCoda -> Int-> GenKangaroo ust a -> GenKangaroo ust a
advanceRelative coda dist p = 
    getSt >>= \(ArrIx pos end) -> intraparse coda (pos+dist) end p



restrict :: RegionCoda -> Int-> GenKangaroo ust a -> GenKangaroo ust a
restrict coda len p = 
    getSt >>= \(ArrIx pos _) -> intraparse coda pos len p


restrictToPos :: RegionCoda -> Int-> GenKangaroo ust a -> GenKangaroo ust a
restrictToPos coda abs_pos p = 
    getSt >>= \(ArrIx pos _) -> intraparse coda pos (abs_pos-pos) p

                
