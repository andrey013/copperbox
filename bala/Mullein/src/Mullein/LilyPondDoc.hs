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
import Mullein.LilyPondOutput ( command )

import Text.PrettyPrint.Leijen 

data CtxComment

comment :: String -> P CtxComment
comment s  = P $ enclose (text "%{ ") (text " %}") (text s) 


data CtxTopLevel 
data CtxHeader

header :: [P CtxHeader] -> P CtxTopLevel
header xs = P $ command "header" <$> braces body where
    body = indent 2 $ vsep (map unP xs)

title :: String -> P CtxHeader
title = P . equation "title" . text 


--------------------------------------------------------------------------------
-- Helpers


equation :: String -> Doc -> Doc
equation s d = text s <+> equals <+> d
