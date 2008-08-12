
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Systems.SystemLilyPond
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- SystemLilyPond - combining multiple concurrent staves.
--
--------------------------------------------------------------------------------

module HNotate.System.SystemLilyPond (
    LilyPondSystem,
    default_ly_system
  ) where

import HNotate.Backend.LilyPond.LyFragments
import HNotate.Backend.LilyPond.LilyPondVersion
import HNotate.Print.OutputLilyPond
import qualified Data.Foldable as F
import Data.Sequence
import Data.Version

type LilyPondSystem = PartLyExprs -> LyCxt_Toplevel

default_ly_system :: String -> LyCxt_Element -> PartLyExprs -> LyCxt_Toplevel
default_ly_system s prefix exprs = 
    toplevelStart
      +++ version (showVersion __lilypond_version)
      +++ header (headerStart +++ title s)
      +++ book
            (block (score $ makeExprs prefix exprs))

makeExprs :: LyCxt_Element -> PartLyExprs -> LyBlock
makeExprs prefix se = case viewl se of
  EmptyL      -> block (prefix)
  (e1 :< sse) -> case viewl sse of
                   EmptyL     -> block (expr1 prefix e1)
                   (e1 :< _)  -> let poly      = F.foldl fn (expr1 prefix e1) sse
                                     fn acc a  = acc \\ (expr1 prefix a)
                                 in block (elementStart +++ openPoly +++ poly +++ closePoly)

expr1 :: LyCxt_Element -> PartLyMusicExpr -> LyCmdRelative
expr1 prefix expr = relative (_c ! raised 1) (prefix <+< expr)
    
    
