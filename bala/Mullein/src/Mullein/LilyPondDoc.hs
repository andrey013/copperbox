{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondDoc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Document combinators for LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondDoc where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LilyPondOutput ( LilyPondOutput(..), command )
import Mullein.Utils

import Text.PrettyPrint.Leijen 

data CtxComment



data CtxTopLevel 
data CtxHeader

instance Concat CtxTopLevel CtxTopLevel where
  P a +++ P b  = P $ a <$> b

header :: [P CtxHeader] -> P CtxTopLevel
header xs = P $ command "header" <$> nestBraces body where
    body = vsep (map unP xs)

title       :: String -> P CtxHeader
title       = P . equation "title" . doubleQuote

composer    :: String -> P CtxHeader
composer    = P . equation "composer" . doubleQuote

subtitle    :: String -> P CtxHeader
subtitle    = P . equation "subtitle" . doubleQuote

instrument  :: String -> P CtxHeader
instrument  = P . equation "instrument" . doubleQuote

copyright   :: String -> P CtxHeader
copyright   = P . equation "copyright" . doubleQuote

piece       :: String -> P CtxHeader
piece       = P . equation "piece" . doubleQuote


data CtxBook

book :: [P CtxBook] -> P CtxTopLevel
book ps = P $ command "book" <+> nestBraces (vsep $ map unP ps)

data CtxScore 


score :: P CtxScore -> P CtxBook
score p = P $ command "score" <+> nestBraces (unP p)


definition :: String -> P CtxScore -> P CtxTopLevel
definition name p = P $ text name <+> equals <+> unP p

usedef :: String -> P CtxScore
usedef = P . command

keycmd :: Key -> P CtxScore
keycmd _k = P $ command "key" <+> comment "Key - TODO"

metercmd :: Meter -> P CtxScore
metercmd _k = P $ command "meter" <+> comment "Meter - TODO"

-- | The @set@ command allows LilyPond properties to be modified
-- with arbitrary Scheme code.
-- See /Non-traditional key signatures/ in the LilyPond manual 
-- for instance where this is necessary.
setcmd :: Doc -> P CtxScore
setcmd d = P $ command "set" <+> d

lilypondOutput :: LilyPondOutput -> P CtxScore
lilypondOutput = P . getLilyPondOutput  



--------------------------------------------------------------------------------
-- Helpers

comment :: String -> Doc
comment s  = enclose (text "%{ ") (text " %}") (text s) 


equation :: String -> Doc -> Doc
equation s d = text s <+> equals <+> d


nestBraces :: Doc -> Doc
nestBraces d = lbrace <$> indent 2 d <$> rbrace 