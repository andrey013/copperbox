{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.ScoreInternal
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Internal representation for Csound scores.
--
--------------------------------------------------------------------------------

module ZSnd.Core.ScoreInternal
  (

    Timespan(..)
  , Frame(..)
  , Locale
  , Score(..)
  , PrimStmt(..)
  , AbsPrimStmt(..)
  , GenStmtProps(..)
  , InstStmtProps(..)
  , CsoundValue(..)


  , absPrimStart
  , absPrimDuration
 
  ) where

import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.JoinList

-- import Control.Applicative

-- | This is equaivalent to a bounding box.
--
data Timespan u = Timespan
      { timespan_start  :: u
      , timespan_end    :: u
      }

type DTimespan = Timespan Double

-- | This is equivalent to an affine frame.
--
data Frame = Frame 
       { frame_origin   :: Double
       , frame_scaling  :: Double
       }
  deriving (Eq,Ord,Show)

type Locale = (DTimespan, Frame)


data Score = Score Locale (JoinList Score)
           | Leaf  Locale (JoinList PrimStmt)
          



-- | Dynamically typed, delta onset timed statement.
--
data PrimStmt = TableStmt Double GenStmtProps
              | InstStmt  Double InstStmtProps
  deriving (Eq,Ord,Show)


-- | Dynamically typed, absolute timed statement.
--
data AbsPrimStmt = AbsTableStmt Double GenStmtProps
                 | AbsInstStmt  Double InstStmtProps
  deriving (Eq,Ord,Show)


data GenStmtProps = GenStmtProps
       { table_assign_num     :: Int  -- corresponds to table-ref in orch
       , table_size           :: Int
       , table_genroutine     :: Int
       , table_args           :: [CsoundValue]
       }
  deriving (Eq,Ord,Show)

data InstStmtProps = InstStmtProps 
        { inst_num            :: Int
        , inst_dur            :: Double
        , inst_pfields        :: [Double]
        } 
  deriving (Eq,Ord,Show)


data CsoundValue = CsDouble Double
                 | CsInt    Int
                 | CsString String
  deriving (Eq,Ord,Show)


absPrimStart :: AbsPrimStmt -> Double
absPrimStart (AbsTableStmt ot _) = ot
absPrimStart (AbsInstStmt  ot _) = ot

absPrimDuration :: AbsPrimStmt -> Double
absPrimDuration (AbsTableStmt _ _) = 0
absPrimDuration (AbsInstStmt _ p)  = inst_dur p

--------------------------------------------------------------------------------
-- Format instances

instance Format CsoundValue where
  format (CsDouble d) = dtrunc d
  format (CsInt i)    = int i
  format (CsString s) = dquotes $ text s
  

{-

instance Format PrimStmt where
  format (TableStmt i s sz n xs) = 
      char 'f' <+> padr 5 (int i) <+> padl 5 (dtrunc s) <+> padl 5 (int sz)
               <+> padl 5 (int n) <+> hsep (map (padl 5 . format) xs)

  format (InstStmt i s d xs) = 
      char 'i' <+> padr 5 (int i) <+> padl 5 (dtrunc s) <+> padl 5 (dtrunc d)
               <+> hsep (map (padl 5 . dtrunc) xs)


-}