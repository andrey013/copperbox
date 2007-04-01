
module Compiler.WriteOutput
  (outputToFile) 
  where
import Compiler.Language

import Control.Monad
import Data.Maybe
import System.IO
import System.Time



outputToFile text commenter opt_prolog opt_epilog path = do
  h <- openFile path WriteMode
  comment <- genTimeStamp commenter
  hPutStrLn h comment
  hPutStrLn h (fromMaybe "\n" opt_prolog) 
  hPutStr h text
  replicateM_ 2 (hPutBlankLine h)
  hPutStrLn h (fromMaybe "\n" opt_epilog) 
  hClose h
  
genTimeStamp :: Commenter -> IO String
genTimeStamp commenter = do
  t <- getClockTime
  t' <- toCalendarTime t  
  return $ commenter (calendarTimeToString t') 
    
hPutBlankLine h = hPutStrLn h ""  
