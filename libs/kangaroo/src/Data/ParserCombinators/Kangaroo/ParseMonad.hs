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

type St    = Int            -- 'file' position
type Env   = ImageData
  

-- Kangaroo is not a transformer as IO is always at the 
-- \'bottom\' of the effect stack. Like the original Parsec it is
-- parametric on user state (refered to as ust).
--
newtype Kangaroo ust a = Kangaroo { 
          getKangaroo :: Env -> St -> ust -> IO (Either ParseErr a, St, ust) }
          

fmapKang :: (a -> b) -> Kangaroo ust a -> Kangaroo ust b
fmapKang f (Kangaroo x) = Kangaroo $ \env st ust -> 
    x env st ust `bindIO` \(a,st',ust') -> return (fmap f a, st', ust')


instance Functor (Kangaroo ust) where
    fmap = fmapKang



returnIO :: a -> IO a
returnIO = return

infixl 1 `bindIO`

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)


returnKang :: a -> Kangaroo st a
returnKang a = Kangaroo $ \_ st ust -> returnIO (Right a, st, ust)

infixl 1 `bindKang`

bindKang :: Kangaroo ust a -> (a -> Kangaroo ust b) -> Kangaroo ust b
(Kangaroo x) `bindKang` f = Kangaroo $ \env st ust -> 
    x env st ust `bindIO` \(ans, st', ust') ->
        case ans of Left err -> returnIO (Left err,st',ust')
                    Right a  -> getKangaroo (f a) env st' ust'
 



instance Monad (Kangaroo ust) where
  return = returnKang
  (>>=)  = bindKang


instance Applicative (Kangaroo ust) where
  pure = return
  (<*>) = ap




getSt :: Kangaroo ust St
getSt = Kangaroo $ \_ st ust -> return (Right st, st, ust)

putSt :: St -> Kangaroo ust ()
putSt st = Kangaroo $ \_ _ ust -> return (Right (), st, ust)

modifySt :: (St -> St) -> Kangaroo ust ()
modifySt f = Kangaroo $ \_ st ust -> return (Right (), f st, ust)


getUserSt :: Kangaroo ust ust
getUserSt = Kangaroo $ \_ st ust -> return (Right ust, st, ust)

putUserSt :: ust -> Kangaroo ust ()
putUserSt ust = Kangaroo $ \_ st _ -> return (Right (), st, ust)

modifyUserSt :: (ust -> ust) -> Kangaroo ust ()
modifyUserSt f = Kangaroo $ \_ st ust -> return (Right (), st, f ust)



askEnv :: Kangaroo ust Env
askEnv = Kangaroo $ \env st ust -> return (Right env, st, ust)


throwErr :: String -> Kangaroo ust a
throwErr msg = Kangaroo $ \_ st ust -> return (Left msg, st, ust)

liftIOAction :: IO a -> Kangaroo ust a
liftIOAction ma = Kangaroo $ \_ st ust -> 
    ma >>= \a -> return (Right a, st, ust) 




runKangaroo :: Kangaroo ust a -> ust -> FilePath -> IO (Either ParseErr a,ust)
runKangaroo p user_state filename = 
    withBinaryFile filename ReadMode $ \ handle -> 
      do { sz'         <- hFileSize handle
         ; let sz = fromIntegral sz'
         ; arr         <- newArray_ (0,sz-1)
         ; (ans,_,ust) <- runP p arr
         ; return (ans,ust)   
         }
  where 
--    runP :: Kangaroo ust a -> ImageData -> IO (Either ParseErr a,St,ust) 
    runP (Kangaroo x) arr = x arr 0 user_state



--------------------------------------------------------------------------------
-- 


   
word8 :: Kangaroo ust Word8
word8 = do
    ix   <- getSt
    arr  <- askEnv
    a    <- liftIOAction $ readArray arr ix
    putSt $ ix+1
    return a


eof :: Kangaroo ust Bool
eof = do
     ix  <- getSt
     arr <- askEnv
     (_,up)  <- liftIOAction $ getBounds arr
     return $ (ix>=up) 


reportFail :: String -> Kangaroo ust a
reportFail s = do 
    posn <- getSt
    throwErr $ s ++ posStr posn
  where
    posStr p = " position " ++ show p   



