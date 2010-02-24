{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Hurdle.Ar.Parser
-- Copyright   :  (c) Stephen Tetley 2009, 2010
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
      Right a -> return a




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
    liftIOAction $ putStrLn "ao"
    header  <- arHeader
    let sz  = arh_size header
    liftIOAction $ putStrLn ("size " ++ show sz)
    pos     <- position
    body    <- count sz word8
    end_pos <- position
    when (end_pos `mod` 2 /= 0) (anyChar >> return ())   -- should be a newline 
    return $ ArchiveObject 
                { ar_header         = header
                , ar_body           = (pos,sz)
                }

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

