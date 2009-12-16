{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Random access parse monad 
--
--------------------------------------------------------------------------------

module Data.ParserCombinators.Kangaroo.ParseMonad where

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Word
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


throwErr :: String -> GenKangaroo ust a
throwErr msg = GenKangaroo $ \_ st ust -> return (Left msg, st, ust)

liftIOAction :: IO a -> GenKangaroo ust a
liftIOAction ma = GenKangaroo $ \_ st ust -> 
    ma >>= \a -> return (Right a, st, ust) 




runGenKangaroo :: GenKangaroo ust a -> ust -> FilePath -> IO (Either ParseErr a,ust)
runGenKangaroo p user_state filename = 
    withBinaryFile filename ReadMode $ \ handle -> 
      do { sz          <- hFileSize handle
         ; arr         <- newArray_ (0,fromIntegral $ sz-1)
         -- Next line necessary, even though the result is not needed...
         ; rsz         <- hGetArray handle arr  (fromIntegral sz)
         ; (ans,_,ust) <- runP p rsz arr
         ; return (ans,ust)   
         }
  where 
    runP (GenKangaroo x) upper arr = x arr (ArrIx 0 (upper-1)) user_state



--------------------------------------------------------------------------------
-- 


   
word8 :: GenKangaroo ust Word8
word8 = do
    (ArrIx ix end)   <- getSt
    arr              <- askEnv
    a                <- liftIOAction $ readArray arr ix
    putSt $ ArrIx (ix+1) end
    return a


atEnd :: GenKangaroo ust Bool
atEnd = getSt >>= \(ArrIx ix end) -> return $ ix >= end


reportError :: ParseErr -> GenKangaroo ust a
reportError s = do 
    posn <- getSt
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   



