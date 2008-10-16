--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PrintMonad
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print Abc and LilyPond output - the monad tracks some state 
-- for beaming.
--
--------------------------------------------------------------------------------

module HNotate.PrintMonad where

import Control.Monad.State
import Text.PrettyPrint.Leijen

type PrintM a = State St a

type NoteListOutput = Doc


data St = St { 
    _append_op  :: Doc -> Doc -> Doc, 
    _output     :: Doc, 
    _prefix     :: Doc 
  }

pmZero = St { _append_op = (<+>), _output = empty, _prefix = empty }

execPrintM :: PrintM a -> St -> Doc
execPrintM f s = _output $ execState f s

runPrintM :: PrintM a -> St -> (a,Doc)
runPrintM f s = let (a,st) = runState f s in (a, _output st)

 

glyph :: Doc -> PrintM ()
glyph e = do 
    acc <- gets _output
    op  <- gets _append_op
    pre <- gets _prefix
    modify (\s -> s {_output = acc `op` (pre <> e), _prefix = empty}) 
    
-- element drops any stored prefix 
element :: Doc -> PrintM ()
element e = do
  acc <- gets _output
  modify (\s -> s {_output = acc <+> e, _prefix = empty}) 


breakingElement :: Doc -> PrintM ()
breakingElement e = do
  acc <- gets _output
  modify (\s -> s {_output = acc <+> e <> line, _prefix = empty}) 
    
appendOp :: (Doc -> Doc -> Doc) -> PrintM ()
appendOp op = modify (\s -> s {_append_op = op})

prefix :: Doc -> PrintM ()
prefix pre = modify (\s -> s {_prefix = pre})

attribute :: Doc -> PrintM ()
attribute a = do 
    acc <- gets _output
    modify (\s -> s {_output = acc <> a})

manualbreak :: PrintM ()
manualbreak = do 
    acc <- gets _output
    modify (\s -> s {_output = acc <> line})


