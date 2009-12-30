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
    header   <- arHeader
    symbols  <- arSymbolTable
    return $ ArArchive 
                { ar_magic        = magic
                , ar_header       = header
                , ar_symbol_table = symbols
                }


arMagicString :: Parser String
arMagicString = text 8 



arHeader :: Parser ArHeader
arHeader = do 
    name    <- text 16
    date    <- text 12
    uid     <- text 6
    gid     <- text 6
    mode    <- text 8
    size    <- text 10
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


arSymbolTable :: Parser ArSymbolTable
arSymbolTable = do 
    num_syms    <- word32be
    table_len   <- word32be
    return $ ArSymbolTable
                { symb_number_elements  = num_syms
                -- , symb_data_length      = table_len
                -- , symb
                }
