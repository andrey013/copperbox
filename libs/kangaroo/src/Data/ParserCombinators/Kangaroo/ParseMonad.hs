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
  , RegionName
    
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

import Data.ParserCombinators.Kangaroo.Utils 

import Text.PrettyPrint.JoinPrint

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

-- For debugging, track regions...
type RegionName  = String
type RegionInfo  = (RegionName,RegionCoda,Int,Int)
type RegionStack = [RegionInfo]


type St    = (ArrIx,RegionStack)
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


getArrIx :: GenKangaroo ust ArrIx
getArrIx = GenKangaroo $ \_ st ust -> return (Right $ fst st, st, ust)

putArrIx :: ArrIx -> GenKangaroo ust ()
putArrIx ix@(ArrIx s e)  = GenKangaroo $ \_ (_,stk) ust -> 
    if s <= e then return (Right (), (ix,stk), ust)
              else return (Left $ "Bad index " ++ show (s,e), (ix,stk), ust)

   

-- modifyArrIx :: (Int -> Int) -> GenKangaroo ust ()
-- modifyArrIx f = GenKangaroo $ \_ (ArrIx ix end,stk) ust -> 
--    return (Right (), (ArrIx (f ix) end, stk), ust)



bracketRegionInfo :: RegionInfo -> GenKangaroo ust a -> GenKangaroo ust a
bracketRegionInfo i = bracketM_ (push i) pop 
  where 
    push s      = GenKangaroo $ \_ (ix,stk) ust -> 
                    return (Right (), (ix,s:stk), ust)
    pop         = GenKangaroo $ \_ (ix,stk) ust -> 
                    return (Right (), (ix,popp stk), ust)
    popp (_:xs) = xs
    popp []     = [] -- should be unreachable


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
      do { sz                 <- hFileSize handle
         ; arr                <- newArray_ (0,fromIntegral $ sz-1)
         ; rsz                <- hGetArray handle arr  (fromIntegral sz)
         ; (ans,(_,stk),ust)  <- runP p rsz arr
         ; return (mergeStk ans stk,ust)   
         }
  where 
    runP (GenKangaroo x) upper arr = x arr st0 user_state  where 
        st0 = (ArrIx 0 (upper-1), [(" -- file -- ",Alfine,0,upper-1)]) 
    
    mergeStk (Left err)  stk = Left $ err ++ ('\n':'\n':printRegionStack stk)
    mergeStk (Right ans) _   = Right ans

printRegionStack :: RegionStack -> String
printRegionStack stk = render $ vcat $ map fn stk
  where
    (w1,w4)                 = onSnd (length . show) $ foldr phi (0,0) stk
    phi (name,_,_,i) (a,b)  = (max a (length name), max b i)
    onSnd f (a,b)            = (a, f b)

    fn                      :: RegionInfo -> Doc
    fn (name,_,st,end)   =     alignPad AlignLeft w1 ' ' (text name)
                           <+> alignPad AlignLeft w4 ' ' (int st)
                           <+> alignPad AlignLeft w4 ' ' (int end)
    


--------------------------------------------------------------------------------
-- Helpers

--------------------------------------------------------------------------------
-- 


reportError :: ParseErr -> GenKangaroo ust a
reportError s = do 
    posn <- getArrIx
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
    (ArrIx ix end)   <- getArrIx
    when (ix>end)    (reportError "word8")   -- test emphatically is (>) !
    arr              <- askEnv
    a                <- liftIOAction $ readArray arr ix
    putArrIx $ ArrIx (ix+1) end
    return a


checkWord8 :: (Word8 -> Bool) -> GenKangaroo ust (Maybe Word8)
checkWord8 check = getArrIx >>= \ix -> word8 >>= \ans ->
    if check ans then return $ Just ans
                 else putArrIx ix >> return Nothing



-- no 'try' in Kangaroo... 
-- opt is the nearest to it, opt backtracks the cursor onm failure.
opt :: GenKangaroo ust a -> GenKangaroo ust (Maybe a)
opt p = GenKangaroo $ \env st ust -> (getGenKangaroo p) env st ust >>= \ ans -> 
    case ans of
      (Left _, _, ust')    -> return (Right Nothing, st, ust')
      (Right a, st', ust') -> return (Right $ Just a, st', ust')

position :: GenKangaroo ust Int
position = liftM arr_ix_ptr getArrIx

regionEnd :: GenKangaroo ust Int
regionEnd = liftM arr_ix_end getArrIx



atEnd :: GenKangaroo ust Bool
atEnd = getArrIx >>= \(ArrIx ix end) -> return $ ix >= end

lengthRemaining :: GenKangaroo ust Int
lengthRemaining = getArrIx >>= \(ArrIx ix end) -> 
   let rest = end - ix in if rest < 0 then return 0 else return rest

--------------------------------------------------------------------------------
-- The important ones parsing within a /region/ ...



-- | 'intraparse' - coda x abs_pos x length x parser
--
-- Note abs_pos must be within the current region.
--
intraparse :: RegionName -> RegionCoda -> Int -> Int 
           -> GenKangaroo ust a 
           -> GenKangaroo ust a
intraparse name coda intra_start len p = 
    bracketRegionInfo (name,coda,intra_start, intra_start+len) $ 
      withLoc "intraparse" intra_start len $ \old_start old_end -> 
        p >>= \ans ->
          case coda of
            Dalpunto  -> do { putArrIx (ArrIx old_start old_end)
                            ; return ans 
                            }
            Alfermata -> do { pos <- position
                            ; putArrIx (ArrIx pos old_end)
                            ; return ans
                            }
            Alfine    -> do { putArrIx (ArrIx (intra_start+len) old_end)
                            ; return ans
                            }
     
             

--
withLoc :: String -> Int -> Int 
        -> (Int -> Int -> GenKangaroo ust a) 
        -> GenKangaroo ust a
withLoc fun_name new_start len mf = let new_end = new_start + len in
    getArrIx >>= \(ArrIx pos end) ->
        withSuccess (pos <= new_start) (backwardsError new_start pos fun_name) $
            withSuccess (new_end <= end) (forwardsError new_end end fun_name) $ 
                putArrIx (ArrIx new_start new_end) >> mf pos end


backwardsError :: Int -> Int -> String -> String  
backwardsError new_pos old_pos fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name
    , "\n*** Cannot backtrack, " 
    , show new_pos 
    , " is before current position " 
    , show old_pos
    ]

forwardsError :: Int -> Int -> String -> String
forwardsError new_end old_end fun_name = concat
    [ "Kangaroo.ParseMonad."
    , fun_name 
    , "\n*** New end point " 
    , show new_end 
    , " extends beyond the end of the current region "
    , show old_end
    ]

advance :: RegionName -> RegionCoda -> Int 
        -> GenKangaroo ust a 
        -> GenKangaroo ust a
advance name coda intra_start p = getArrIx >>= \(ArrIx _ end) -> 
    intraparse name coda intra_start (end - intra_start) p

advanceRelative :: RegionName -> RegionCoda -> Int
                -> GenKangaroo ust a 
                -> GenKangaroo ust a
advanceRelative name coda dist p = getArrIx >>= \(ArrIx pos _) -> 
    intraparse name coda (pos+dist) dist p



restrict :: RegionName -> RegionCoda -> Int 
         -> GenKangaroo ust a 
         -> GenKangaroo ust a
restrict name coda len p = getArrIx >>= \(ArrIx pos _) -> 
    intraparse name coda pos len p


restrictToPos :: RegionName -> RegionCoda -> Int 
              -> GenKangaroo ust a 
              -> GenKangaroo ust a
restrictToPos name coda abs_pos p = getArrIx >>= \(ArrIx pos _) -> 
    intraparse name coda pos (abs_pos-pos) p


--------------------------------------------------------------------------------
-- 


                
