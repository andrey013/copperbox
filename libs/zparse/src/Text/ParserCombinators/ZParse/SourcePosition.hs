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
    SrcPos
  , TabStop

  , initial_srcpos

  , incrCol
  , incrTab
  , incrLine
  , nextPos
 
  ) where


import Text.ParserCombinators.ZParse.Utils


type TabStop = Int -> Int

data SrcPos = SrcPos { 
                 sp_line          :: Int, 
                 sp_column        :: Int, 
                 sp_tab_stop      :: TabStop 
               }

instance Show SrcPos where
  show (SrcPos l c _) = "SrcPos" ++ show l ++ " " ++ show c ++ " _"

initial_srcpos :: SrcPos   
initial_srcpos = SrcPos { sp_line     = 1,  
                          sp_column   = 1, 
                          sp_tab_stop = haskell_tab_stop }

haskell_tab_stop :: TabStop
haskell_tab_stop i = (i+8) - (i-1) `mod` 8



--------------------------------------------------------------------------------
-- Changing source pos

incrCol :: SrcPos -> SrcPos
incrCol = subst (\s i -> s { sp_column=i+1 }) sp_column

incrTab :: SrcPos -> SrcPos
incrTab = subst2 (\s i f -> s { sp_column=(f i) }) sp_column sp_tab_stop


incrLine :: SrcPos -> SrcPos
incrLine = subst (\s i -> s { sp_line =i+1, sp_column=1 }) sp_line



nextPos :: Char -> SrcPos -> SrcPos
nextPos '\t'  = incrTab
nextPos '\CR' = incrLine
nextPos '\LF' = incrLine
nextPos '\FF' = incrLine
-- nextPos '\n'  = incrLine
nextPos _     = incrCol

