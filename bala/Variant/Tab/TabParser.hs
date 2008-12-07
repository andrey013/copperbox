

module TabParser where

import TabBase
import Bala.Base

import Data.Sequence

import Control.Applicative hiding (many, optional, (<|>), empty )
import Control.Monad (ap, replicateM_, foldM)

import Data.Char (isPrint, isSpace)
import qualified Data.Foldable as F
import Data.List (sort)
import Data.Maybe
import Data.Sequence
import Text.ParserCombinators.Parsec


-- | An Applicative instance for Parsec. 
instance Applicative (GenParser tok st) where
  pure = return
  (<*>) = ap
  
  
type BPos = Int
type StrNum = Int
type TabParser a = GenParser Char ParseState a

data ParseState = ParseState {
    tuning        :: Tuning,
    string_num    :: Int,
    bar_pos       :: BPos
    }

state_zero tuning = ParseState 
    { tuning      = tuning, 
      string_num  = 1, 
      bar_pos     = 1 }

data TabNote = TabNote BPos StrNum Pitch
  deriving (Eq,Show)
    
instance Ord TabNote where
  compare (TabNote i _ _) (TabNote j _ _) = i `compare` j

data Bar = Bar { 
    bar_length :: Int,
    notes      :: [TabNote] 
    }
  deriving (Show)

type TabLine = [Bar]

  
  
------
-- State updaters

-- bars start a 1 so we have to subtract 1 to get the length
zeroBarPos :: TabParser Int
zeroBarPos = do
    st <- getState
    let len = bar_pos st 
    updateState (\s -> s { bar_pos = 1 })
    return (len - 1)


nextString :: TabParser ()
nextString = do 
    st <- getState 
    updateState (\s -> s { string_num = (next $ string_num st) })
  where
    next i | i < 6      = i + 1
           | otherwise  = 1

incrPos :: TabParser ()              
incrPos = do 
    st <- getState 
    updateState (\s -> s { bar_pos = (bar_pos st) + 1 })   
    
------  


parseTabfile :: FilePath -> [Int] -> ParseState -> IO (Seq Bar)
parseTabfile filename line_numbers state = do
    text <- readFile filename
    case runParser (fullTabParse line_numbers) state filename text of
      Left err -> error $ show err
      Right sq -> return sq

fullTabParse :: [Int] -> TabParser (Seq Bar)
fullTabParse xs = foldM parseStep empty xs
  where
    parseStep :: Seq Bar -> Int -> TabParser (Seq Bar)
    parseStep sq i = do
        advanceToLine i
        blk <- sixStrings'
        return (sq >< blk)

sourceLoc :: TabParser ()
sourceLoc = do
    pos  <- getPosition
    let lnum = sourceLine pos
    error $ "line " ++ show lnum
    return ()

advanceToLine :: Int -> TabParser ()
advanceToLine i = do
    pos  <- getPosition
    let lnum = sourceLine pos
    if (i < lnum) 
      then fail $ message i lnum
      else replicateM_ (i - lnum) skipline
  where
    message i num = "Cannot advance to " ++ show i ++
                    " already at line " ++ show num
    skipline = water (oneOf "\n")                    

-- transpose?
rebar :: [TabLine] -> Seq Bar
rebar []        = empty
rebar (xs:xxs)  = let s1 = foldr (<|) empty xs in foldl mergebars s1 xxs
  where
    mergebars sq xs = fst $ foldl mergestep (sq,0) xs
    
    mergestep (sq,i) bar = let sbar = index sq i 
                           in (update i (mergebar sbar bar) sq, i+1)
    
    mergebar (Bar i xs) (Bar _ ys) = Bar i (sort $ xs++ys)   


sixStrings' :: TabParser (Seq Bar)   
sixStrings' = rebar <$> sixStrings

sixStrings :: TabParser [TabLine]   
sixStrings = count 6 singleString


singleString :: TabParser TabLine
singleString = step <* nextString
  where
    step = linePrefix *> many oneBar

linePrefix :: TabParser [Char]
linePrefix = many $ noneOf "-|1234567890"
                 
oneBar :: TabParser Bar
oneBar = mkBar <$> manyTill step1 barLineParse <*> zeroBarPos
  where
    step1 = choice [dashParse, noteParse] <* incrPos
    mkBar xs i = Bar i (catMaybes xs)
    
          
                 
-- Use positiveInt so we don't consume a leading dash   
noteParse :: TabParser (Maybe TabNote)
noteParse = fn <$> positiveInt <*> getState
  where
    fn i st = let snum = string_num st
                  bp   = bar_pos st
                  tu   = tuning st
              in Just $ TabNote bp snum (tabPitch tu snum i)
              
-- | ignore a dash or a '^'              
dashParse :: TabParser (Maybe TabNote)
dashParse = Nothing <$ choice [char '-', tabmark]

tabmark :: TabParser Char
tabmark = oneOf "hpbr/\\vxt^s"

          
barLineParse :: TabParser Char
barLineParse = char '|' 


-- It generates a parse error if we get to eof without parsing p
-- which is what we want.
water :: GenParser Char st a -> GenParser Char st a
water p = do
    a <- optionMaybe p    
    case a of
      Just a -> return $ a
      Nothing -> anyChar >> water p 
      
-- | A digit seuence, returning Int.
positiveInt :: GenParser Char st Int
positiveInt = read <$> many1 digit      

