{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Ar.Parser
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Read \".a\" archive...
--
--------------------------------------------------------------------------------

module Hurdle.Ar.Parser where

import Hurdle.Ar.Datatypes
import Hurdle.Base.Utils

import Control.Monad
import Data.Char

readAr :: FilePath -> IO ArArchive
readAr filename = do
    (ans,w) <- runKangaroo arArchive filename
    case ans of 
      Left err -> (putStrLn $ toList w) >> error err
      Right mf -> return mf





--------------------------------------------------------------------------------
-- 
    
    
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
    body    <- count sz char
    pos     <- position
    when (pos `mod` 2 /= 0) (char >> return ())   -- should be a newline 
    return $ ArchiveObject 
                { ar_header         = header
                , ar_body           = body
                }

paddedNumber :: (Read a, Integral a) => Int -> Parser a
paddedNumber i = liftM fn $ count i char
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
