{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Kangaroo.ParseMonad
-- Copyright   :  (c) Stephen Tetley 2009-2010
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

  -- * Parser types
    GenKangaroo         -- don't export outside the package
  , ParseErr

  -- * Region types
  , RegionCoda(..)
  , RegionName    

  -- * Monadic run function
  , runGenKangaroo      -- don't export outside the package

  -- * Lift IO actions
  , liftIOAction

  -- * Non proper morphisms    
  , throwErr            -- don't export outside the package
  , getUserSt           -- don't export outside the package
  , putUserSt           -- don't export outside the package
  , modifyUserSt        -- don't export outside the package

  -- * Error reporting and exception handling
  , reportError
  , substError

  -- * Primitive parsers
  , word8
  , satisfy
  , checkWord8
  , opt 

  -- * Query the cursor position
  , position
  , region
  , atEnd
  , lengthRemaining
  , regionSize

  -- * Parse within a region
  , intraparse
  , advance
  , advanceRelative
  , restrict
  , restrictToPos
   
  -- * Debug
  , printHexAll
  , printRegionStack 

  ) where

import Data.ParserCombinators.Kangaroo.Debug
import Data.ParserCombinators.Kangaroo.Region
import Data.ParserCombinators.Kangaroo.Utils

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Word
import Numeric
import System.IO

type ParseErr = String

type ImageData = IOUArray Int Word8   -- Is Int big enough for index?


type St    = ParseStack
type Env   = ImageData    


-- | Kangaroo is not a transformer as IO is always at the 
-- \'bottom\' of the effect stack. Like the original Parsec it is
-- parametric on user state (refered to as ust).
--
newtype GenKangaroo ust a = GenKangaroo { 
          getGenKangaroo :: Env -> St -> ust -> IO (Either ParseErr a, St, ust) }
          

--------------------------------------------------------------------------------
-- Instances and helpers


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
--
-- My proposition is that the sort of parsing that Kangaroo 
-- intends to provide you always now want you want hence there 
-- is no inbuilt backtracking or support for list-of-successes.

--------------------------------------------------------------------------------
-- Run...

-- | Primitive monadic run function - other modules should export
-- type specific specializations of this function...
--
runGenKangaroo :: GenKangaroo ust a 
               -> ust 
               -> FilePath 
               -> IO (Either ParseErr a,ust)
runGenKangaroo p user_state filename = 
    withBinaryFile filename ReadMode $ \ handle -> 
      do { sz                 <- hFileSize handle
         ; arr                <- newArray_ (0,fromIntegral $ sz-1)
         ; rsz                <- hGetArray handle arr  (fromIntegral sz)
         ; (ans,stk,ust)      <- runP p rsz arr
         ; return (answer ans stk,ust)   
         }
  where 
    runP (GenKangaroo x) upper arr = x arr st0 user_state  
      where 
        st0 = newStack 0 (upper-1) Alfine " -- file -- "

    answer (Left err)  stk        = Left $ err 
                                          ++ ('\n':'\n':printParseStack stk)
    answer (Right ans) _          = Right ans


--------------------------------------------------------------------------------
-- Non proper morphisms 

throwErr  :: ParseErr -> GenKangaroo ust a
throwErr msg = GenKangaroo $ \_ st ust -> return (Left msg, st, ust)

askEnv    :: GenKangaroo ust Env
askEnv    = GenKangaroo $ \env st ust -> return (Right env, st, ust)

getSt     :: GenKangaroo ust St
getSt     = GenKangaroo $ \_ st ust -> return (Right st, st, ust)

putSt     :: St -> GenKangaroo ust ()
putSt st  = GenKangaroo $ \_ _ ust -> return (Right (), st, ust)


getPos    :: GenKangaroo ust Pos
getPos    = liftM location getSt

getEnd    :: GenKangaroo ust RegionEnd
getEnd    = liftM regionEnd  getSt

getStart  :: GenKangaroo ust RegionStart
getStart  = liftM regionStart getSt

getUserSt :: GenKangaroo ust ust
getUserSt = GenKangaroo $ \_ st ust -> return (Right ust, st, ust)

putUserSt :: ust -> GenKangaroo ust ()
putUserSt ust = GenKangaroo $ \_ st _ -> return (Right (), st, ust)

modifyUserSt :: (ust -> ust) -> GenKangaroo ust ()
modifyUserSt f = GenKangaroo $ \_ st ust -> return (Right (), st, f ust)


-- Modifying the position lets the parser go beyond the 
-- end-of-file.

advancePos1 :: GenKangaroo ust ()
advancePos1 = modifyPos (+1) 

modifyPos :: (Pos -> Pos) -> GenKangaroo ust ()
modifyPos f = GenKangaroo $ \_ st ust -> return (Right (), move f st, ust)



bracketRegion :: RegionInfo -> GenKangaroo ust a -> GenKangaroo ust a
bracketRegion i = bracketM_ pushM popM 
  where 
    pushM       = getSt >>= \st -> case push i st of
                     Left err -> throwErr $ getRegionError err
                     Right stk -> putSt stk

    popM         = getSt >>= \st -> putSt (pop st)


--------------------------------------------------------------------------------
-- Lift IO actions - we are in the IO 

-- | Lift an IO action into the Kangaroo monad.
--
liftIOAction :: IO a -> GenKangaroo ust a
liftIOAction ma = GenKangaroo $ \ _env st ust -> 
    ma >>= \a -> return (Right a, st, ust) 



--------------------------------------------------------------------------------
-- Helpers

--------------------------------------------------------------------------------
-- Error reporting 

-- | Report a parse error.
--
-- Source position is appended to the supplied error message
--
reportError :: ParseErr -> GenKangaroo ust a
reportError s = do 
    posn <- getPos
    throwErr $ s ++ posStr posn
  where
    posStr pos  = concat [ " absolute position "
                         , show pos
                         , " (0x" 
                         , showHex pos []
                         , ")"
                         ]


-- | 'substError' : @ parser * error_msg -> parser@
--
-- 'substError' is equivalent to Parsec\'s @\<?\>@ combinator.
--
-- Run the supplied parser, if the parse succeeds return the 
-- result, otherwise override the original error message with
-- the supplied @error_msg@.
--
substError :: GenKangaroo ust a -> ParseErr -> GenKangaroo ust a
substError p msg = GenKangaroo $ \env st ust -> 
    (getGenKangaroo p) env st ust >>= \ ans -> 
      case ans of
        (Left _, st', ust')  -> return (Left msg, st', ust')
        okay                 -> return okay


--------------------------------------------------------------------------------
-- Primitive parsers

-- | Parse a single byte.
--
-- If the cursor is beyond the end of the current region a 
-- parse-error is thrown with 'reportError'.
--
word8 :: GenKangaroo ust Word8
word8 = do
    ix               <- getPos
    end              <- getEnd
    when (ix>end)    (reportError "word8")   -- test emphatically is (>) !
    arr              <- askEnv
    a                <- liftIOAction $ readArray arr ix
    advancePos1
    return a

-- | 'satisfy' : @ predicate -> parser @
--
-- Parse a single byte and apply the predicate to it. On @True@ 
-- return the parsed byte, on @False@ throw a parse-error with 
-- 'reportError'.
--
satisfy :: (Word8 -> Bool) -> GenKangaroo ust Word8
satisfy p = word8 >>= \x -> if p x then return x else reportError $ "satisfy"


-- | 'checkWord8' : @ predicate -> opt parser @
--
-- Byte parser with backtracking when the match fails.
-- 
-- Parse a single byte and apply the predicate to the result. On
-- success return @(Just answer)@, on failure move the cursor 
-- position back one and return @Nothing@.
--
checkWord8 :: (Word8 -> Bool) -> GenKangaroo ust (Maybe Word8)
checkWord8 check = word8 >>= \ans ->
    if check ans then return $ Just ans
                 else modifyPos (`subtract` 1) >> return Nothing




-- | Backtracking parser similar to Parsec\'s @try@.
--
-- Try the supplied parser, if the parse succeeds with no
-- parse-errors  return @(Just answer)@. If a parse-error is 
-- generated, discard the parse-error, return the cursor to the
-- initial position and return @Nothing@.
--
opt :: GenKangaroo ust a -> GenKangaroo ust (Maybe a)
opt p = GenKangaroo $ \env st ust -> (getGenKangaroo p) env st ust >>= \ ans -> 
    case ans of
      (Left _, _, ust')    -> return (Right Nothing, st, ust')
      (Right a, st', ust') -> return (Right $ Just a, st', ust')




--------------------------------------------------------------------------------
-- Querying position

-- | 'position' : @-> cursor-position@
--
-- Return the current cursor position
--
position :: GenKangaroo ust Int
position = getPos


-- | 'region' : @-> (region-start, cursor-position, region-end)@
--
-- Return the current parse region and the current position of 
-- the cursor within it.
-- 
region   :: GenKangaroo ust (Int,Int,Int)
region   = liftM3 (,,) getStart getPos getEnd

-- region limits are inclusive so cursor is at the end 
-- if the position is greater that the end location.

-- | 'atEnd' - is the cursor at the end of the current region?
--
atEnd :: GenKangaroo ust Bool
atEnd = liftM2 (>) getPos getEnd

-- | 'lengthRemaining' : @-> distance-to-region-end@
--
-- Distance from the current cursor position to the end of the
-- current region
--
lengthRemaining :: GenKangaroo ust Int
lengthRemaining = liftM2 fn getEnd getPos 
  where  
    fn a b | a <= b    = 0
           | otherwise = a - b


-- | 'regionSize' : @-> region-length@
--
-- Size of the current region.
--
regionSize :: GenKangaroo ust Int
regionSize = liftM2 (-) getEnd getStart


--------------------------------------------------------------------------------
-- The important ones parsing within a /region/ ...



-- | 'intraparse' : @name * coda * abs_region_start * region_length * parser -> parser@
--
-- Create a new region within the current one and run the 
-- supplied parser. The cursor position is moved to the start 
-- of the new region. The value of @coda@ determines where the 
-- cursor is positioned after a successful parse. 
--
-- 'intraparse' throws a parse error if the supplied 
-- absolute-region-start is not located within the current region,
-- or if the right-boundary of the new region 
-- (@abs_region_start + region_length@) extends beyond the 
-- right-boundary of the current region.
--

intraparse :: RegionName -> RegionCoda -> RegionStart -> Int 
           -> GenKangaroo ust a 
           -> GenKangaroo ust a
intraparse name coda intra_start len p = 
    bracketRegion (newRegion intra_start len coda name) p
             

-- | 'advance' : @name * coda * abs_region_start * parser -> parser@
-- 
-- A variation of 'intraparse' - the new region starts at the 
-- supplied @abs_region_start@ and continues to the end of the 
-- current region.
--
-- 'advance' throws a parse error if the new start position is 
-- not within the current region.
--
 
advance :: RegionName -> RegionCoda -> Int 
        -> GenKangaroo ust a 
        -> GenKangaroo ust a
advance name coda intra_start p = getEnd >>= \end -> 
    intraparse name coda intra_start (end - intra_start) p


-- | 'advanceRelative' : @name * coda * distance * parser -> parser@
--
-- A variation of 'advance' - the start of the new region is
-- calculated from the @current-cursor-position@ + the supplied
-- @distance@.
--
-- 'advanceRelative' throws a parse error if the new start 
-- position is not within the current region.
--

advanceRelative :: RegionName -> RegionCoda -> Int
                -> GenKangaroo ust a 
                -> GenKangaroo ust a
advanceRelative name coda dist p = getPos >>= \pos -> 
    intraparse name coda (pos+dist) dist p


-- | 'restrict' : @ name * coda * distance * parser -> parser@
-- 
-- A variation of 'intraparse' - create a new region as a 
-- restriction of the current one and run the supplied parser. 
-- The new region starts at the current coursor position, the 
-- right-boundary is restricted to the @current-cursor-position@ 
-- + the supplied @distance@.
--
-- 'restrict' throws a parse error if the right-boundary of the 
-- new region extends beyond the current region.
--
restrict :: RegionName -> RegionCoda -> Int 
         -> GenKangaroo ust a 
         -> GenKangaroo ust a
restrict name coda len p = getPos >>= \pos -> 
    intraparse name coda pos len p


-- | 'restrictToPos' : @region-name * coda * abs-end-pos * parser -> parser@
--
-- A variantion of 'restrict' - the new region takes the current 
-- cursor position for the left-boundary and the supplied 
-- absolute-end-position (@abs-end-pos@) as the right-boundary. 
--
-- 'restrictToPos' throws a parse error if the @abs-end-pos@ 
-- extends beyond the right-boundary of the current region. 
--
restrictToPos :: RegionName -> RegionCoda -> Int 
              -> GenKangaroo ust a 
              -> GenKangaroo ust a
restrictToPos name coda abs_pos p = getPos >>= \pos -> 
    intraparse name coda pos (abs_pos-pos) p



--------------------------------------------------------------------------------
-- Debug

printHexAll         :: GenKangaroo ust ()
printHexAll         = askEnv >>= liftIOAction . slowHexAll

printRegionStack    :: GenKangaroo ust ()
printRegionStack    = getSt >>= liftIOAction . putStrLn . printParseStack

                
