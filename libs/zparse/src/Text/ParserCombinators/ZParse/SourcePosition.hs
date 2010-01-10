{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ZParse.SourcePosition
-- Copyright   :  (c) Stephen Tetley 2008, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SrcPos - ...
--
--------------------------------------------------------------------------------

module Text.ParserCombinators.ZParse.SourcePosition 
  ( 
    SrcPos(..)
  , TabStop
  , HasSrcPos(..)

  , initialSrcPos

  , incrCol
  , incrTab
  , incrLine
  , nextPosChar
  , nextPosString
 
  ) where


import Text.ParserCombinators.ZParse.Utils

import Data.List ( foldl' )

type TabStop = Int -> Int

data SrcPos = SrcPos { 
                 src_line       :: Int, 
                 src_column     :: Int, 
                 src_tab_stop   :: TabStop 
               }

instance Show SrcPos where
  show (SrcPos l c _) = "SrcPos" ++ show l ++ " " ++ show c ++ " _"


class HasSrcPos st where
  getSrcPos :: st -> SrcPos
  setSrcPos :: SrcPos -> st -> st



initialSrcPos :: SrcPos   
initialSrcPos = SrcPos { src_line      = 1,
                         src_column    = 1,
                         src_tab_stop  = regular_tab_stop }


-- | This is the used by Parsec and Thomas Hallgren's Haskell 
-- lexer.
--
regular_tab_stop :: TabStop
regular_tab_stop i = (i+8) - (i-1) `mod` 8



--------------------------------------------------------------------------------
-- Changing source pos

incrCol :: SrcPos -> SrcPos
incrCol = subst (\s i -> s { src_column=i+1 }) src_column

incrTab :: SrcPos -> SrcPos
incrTab = subst2 (\s i f -> s { src_column=(f i) }) src_column src_tab_stop


incrLine :: SrcPos -> SrcPos
incrLine = subst (\s i -> s { src_line =i+1, src_column=1 }) src_line



nextPosChar :: Char -> SrcPos -> SrcPos
nextPosChar '\t'  = incrTab
nextPosChar '\CR' = incrLine
nextPosChar '\LF' = incrLine
nextPosChar '\FF' = incrLine
-- nextPos '\n'  = incrLine
nextPosChar _     = incrCol

nextPosString :: [Char] -> SrcPos -> SrcPos
nextPosString xs pos = foldl' (flip nextPosChar) pos xs