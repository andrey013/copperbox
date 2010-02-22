{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  AR
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  volatile
-- Portability :  to be determined.
--
-- AR archive files. 
--
--------------------------------------------------------------------------------

module AR where

import Data.ParserCombinators.KangarooWriter
import qualified Text.PrettyPrint.JoinPrint as PP


import Control.Exception
import Control.Monad
import Data.Char
import Prelude hiding ( catch )
import System.Environment
import System.Exit


type Parser a = Kangaroo String a    

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [path] -> process path
    _ ->  putStrLn "Usage: AR <filename>"

process :: FilePath -> IO ()
process filename = do
    ans <- catch (readAR filename) exitHandle
    printArchive ans
  where
    exitHandle :: IOException -> IO a 
    exitHandle e = putStrLn (show e) >> exitFailure


readAR :: FilePath -> IO ArArchive
readAR filename = do 
   (a,w) <- runKangaroo arArchive filename
   putStrLn w
   either error return a


data ArArchive = ArArchive 
      { ar_magic                    :: String
      , ar_objects                  :: [ArchiveObject]
      }
  deriving Show


data ArchiveObject = ArchiveObject
     { ar_header                    :: ArHeader
     , ar_body                      :: (Int,Int)
     }

data ArHeader = ArHeader 
      { arh_name                    :: String
      , arh_date                    :: String
      , arh_user_id                 :: Int
      , arh_group_id                :: Int
      , arh_mode                    :: String
      , arh_size                    :: Int
      , arh_trailer                 :: String
      }  
  deriving Show



instance Show ArchiveObject where
  show (ArchiveObject h _) = "ArchiveObject " ++ show h







arArchive :: Parser ArArchive
arArchive = do
    magic    <- arMagicString
    objects  <- runOn archiveObject
    return $ ArArchive 
                { ar_magic        = magic
                , ar_objects      = objects
                }


arMagicString :: Parser String
arMagicString = text 8 


archiveObject :: Parser ArchiveObject
archiveObject = do 
    header  <- arHeader
    let sz  = arh_size header
    pos     <- position
    moveForward sz
    end_pos     <- position
    when (end_pos `mod` 2 /= 0) (anyChar >> return ())   -- should be a newline 
    
    -- ... silly debug (show off hexdump)
    printHexRange $ limit (pos,pos+sz)
    liftIOAction  $ putStrLn ""
    -- ... end silly

    return $ ArchiveObject 
                { ar_header         = header
                , ar_body           = (pos,sz)
                }
  where
    limit (x,y) = (x, min y (x+200))


paddedNumber :: (Read a, Integral a) => Int -> Parser a
paddedNumber i = liftM fn $ count i anyChar
  where
    fn = read . takeWhile isDigit


arHeader :: Parser ArHeader
arHeader = do 
    name    <- text 16
    date    <- text 12
    uid     <- paddedNumber 6
    gid     <- paddedNumber 6
    mode    <- text 8
    size    <- paddedNumber 10
    trailer <- text 2
    return $ ArHeader 
                { arh_name            = name
                , arh_date            = date
                , arh_user_id         = uid
                , arh_group_id        = gid
                , arh_mode            = mode
                , arh_size            = size
                , arh_trailer         = trailer
                }  


--------------------------------------------------------------------------------
-- pretty print

printArchive :: ArArchive -> IO ()
printArchive = putStr . archiveText



archiveText :: ArArchive -> String
archiveText = show . ppArchive

ppArchive :: ArArchive -> PP.VDoc
ppArchive a = mgc `PP.vcons` body
  where
    mgc  = PP.text $ ar_magic a
    body = PP.vconcatSep $ map ppArchiveObject $ ar_objects a



ppArchiveObject :: ArchiveObject -> PP.VDoc
ppArchiveObject = ppArHeader . ar_header

ppArHeader :: ArHeader -> PP.VDoc
ppArHeader a = PP.vcat $ column_headings : column_sep : sequence fields a
  where
    ppf    = ppField 4 14
    fields = 
       [ ppf 16 "name"                  (PP.text . arh_name)
       , ppf 12 "date"                  (PP.text . arh_date)
       , ppf 6  "user id"               (PP.int  . arh_user_id)
       , ppf 6  "group id"              (PP.int  . arh_group_id)
       , ppf 8  "mode"                  (PP.text . arh_mode)
       , ppf 10 "size"                  (PP.int  . arh_size)
       , ppf 2  "trailer"               (tup2    . arh_trailer)
       ]
    tup2 (x:y:xs)   = PP.hsep [ hexpp x, hexpp y, PP.text xs]
    tup2 xs         = PP.text xs

    hexpp           = PP.hex2 . fromIntegral . ord

    column_headings = PP.hsep [ PP.text "size"
                              , PP.padl 14 ' ' (PP.text "field") 
                              , PP.padr 16 ' ' (PP.text "value")
                              ]

    column_sep      = PP.replicateChar 60 '-' 

--------------------------------------------------------------------------------
-- Helpers

ppField :: Int -> Int -> Int -> String -> (a -> PP.Doc) -> a -> PP.Doc
ppField n1 n2 sz field_name f a = PP.hsep [sz', field_name', f a]
  where
    sz'         = PP.padl n1 ' ' (PP.text $ show sz)
    field_name' = PP.padl n2 ' ' (PP.text field_name)


