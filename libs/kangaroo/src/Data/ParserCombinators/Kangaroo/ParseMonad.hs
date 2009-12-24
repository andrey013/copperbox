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
  , dalpunto
  , dalpuntoRelative
  , advanceDalpunto
  , advanceDalpuntoAbsolute

  , alfine
  , alfineRelative
  , restrictAlfine

  , alfermata
  , alfermataRelative
  , advanceAlfermata
  , advanceAlfermataAbsolute
  , restrictAlfermata 
   
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

-- Three useful final positions 
--
-- 1. dalpunto  - 'from the point'      
-- - Run the parser within a region and return to where you came
--   from.
--
-- 2. alfine    - 'to the end'     
-- - Run the parser within a region and jump to the right-end of 
--   the region after the parse.
--
-- 3. alfermata - 'to the stop'    
-- - Run the parser within a region, the cursor remains wherever 
--   the parse finished.
--




assertSubsetRegion :: String -> Int -> Int -> GenKangaroo ust ()
assertSubsetRegion fun_name start end = 
    getSt >>= \(ArrIx pos endpos) -> step pos endpos 
  where
    step pos endpos | start < pos   = reportError $ 
                                        backtrackErr start pos fun_name
                    | end > endpos  = reportError $ 
                                        tooFarErr end endpos fun_name
                    | otherwise     = return ()


backtrackErr :: Int -> Int -> String -> String  
backtrackErr new_pos old_pos fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name
    , " - cannot backtrack, " 
    , show new_pos 
    , " is before current position " 
    , show old_pos
    ]

tooFarErr :: Int -> Int -> String -> String
tooFarErr new_end old_end fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name 
    , " - new end point " 
    , show new_end 
    , " extends beyond the end of the current region "
    , show old_end
    ]


-- Parser inside the supplied region, afterwards restore the
-- end of the /outer/ region and return to the initial position.
--
dalpuntoP :: String -> Int -> Int 
                 -> GenKangaroo ust a 
                 -> GenKangaroo ust a
dalpuntoP fun_name start end p = do
    st <- getSt 
    assertSubsetRegion fun_name start end
    putSt $ ArrIx start end
    ans <- p
    putSt st
    return ans
    


-- | return to the start position - start x length
dalpunto :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
dalpunto = dalpuntoP "dalpunto"


-- | return to the start position - displacement x length
dalpuntoRelative :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
dalpuntoRelative disp len p = getSt >>= \(ArrIx pos _) ->
    dalpuntoP "dalpuntoRelative" (pos+disp) (pos+disp+len-1) p

-- | Advance the current position by the supplied distance.
advanceDalpunto :: Int -> GenKangaroo ust p -> GenKangaroo ust p
advanceDalpunto i p = getSt >>= \(ArrIx pos end) -> 
    dalpuntoP "advanceDalpunto" (pos+i) end p

-- | Advance the current position to the supplied (absolute) 
-- position.
advanceDalpuntoAbsolute :: Int -> GenKangaroo ust p -> GenKangaroo ust p
advanceDalpuntoAbsolute i p = getSt >>= \(ArrIx _ end) -> 
    dalpuntoP "advanceDalpuntoAbsolute" i end p


-- Parse inside the supplied region, afterwards go to the end of
-- the supplied region and restore the end of the /outer/ region.
--
alfineP :: String -> Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfineP fun_name start end p = do
    ArrIx _ prev_end <- getSt 
    assertSubsetRegion fun_name start end
    putSt $ ArrIx start end
    ans <- p
    putSt $ ArrIx (end+1) prev_end
    return ans


-- | alfine - parse to the end
alfine :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfine = alfineP "alfine"

-- | finish at the right of the region - displacement x length
alfineRelative :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfineRelative disp len p = getSt >>= \(ArrIx pos _) ->
    alfineP "alfineRelative" (pos+disp) (pos+disp+len-1) p


restrictAlfine :: Int -> GenKangaroo ust p -> GenKangaroo ust p
restrictAlfine dist_to_end p = getSt >>= \(ArrIx pos _) -> 
    alfineP "restrictAlfine" pos (pos + dist_to_end) p
    


-- Parser inside the supplied region, afterwards restore the end 
-- of the /outer/region but keep the cursor in at the current 
-- position.
--
alfermataP :: String -> Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfermataP fun_name start end p = do
    ArrIx _ prev_end    <- getSt 
    assertSubsetRegion fun_name start end
    putSt $ ArrIx start end
    ans <- p
    ArrIx curr_pos _    <- getSt
    putSt $ ArrIx curr_pos prev_end 
    return ans
 
-- | alfermata - parse to the /sign/ i.e. wherever the supplied 
-- parser stops within the supplied region. At the end of the
-- parse restore the outer region.
--
alfermata :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfermata = alfermataP "alfermata"


alfermataRelative :: Int -> Int -> GenKangaroo ust a -> GenKangaroo ust a
alfermataRelative disp len p = getSt >>= \(ArrIx pos _) ->
    alfermataP "alfermataRel" (pos+disp) (pos+disp+len-1) p


-- | Advance the current position by the supplied distance.
advanceAlfermata :: Int -> GenKangaroo ust p -> GenKangaroo ust p
advanceAlfermata i p = getSt >>= \(ArrIx pos end) -> 
    alfermataP "advanceAlfermata" (pos+i) end p

-- | Advance the current position to the supplied (absolute) 
-- position.
advanceAlfermataAbsolute :: Int -> GenKangaroo ust p -> GenKangaroo ust p
advanceAlfermataAbsolute i p = getSt >>= \(ArrIx _ end) -> 
    alfermataP "advanceAlfermataAbsolute" i end p

restrictAlfermata :: Int -> GenKangaroo ust p -> GenKangaroo ust p
restrictAlfermata dist_to_end p = getSt >>= \(ArrIx pos _) ->
    alfermataP "restrictAlfermata" pos (pos + dist_to_end) p