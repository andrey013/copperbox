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

  , standardFrame
  , timespanUnion
  , scoreTimespan

  , moveTimespan
  , moveLocale
  , scaleTimespan
  , scaleLocale
 
  ) where

import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.JoinList hiding ( empty )

-- import Control.Applicative
import qualified Data.Foldable as F


-- | This is equaivalent to a bounding box.
--
data Timespan u = Timespan
      { timespan_start  :: u
      , timespan_end    :: u
      }
  deriving (Show)


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
  deriving (Show)          



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



standardFrame :: Frame
standardFrame = Frame { frame_origin   = 0
                      , frame_scaling  = 1
                      }

timespanUnion :: Ord u => Timespan u -> Timespan u -> Timespan u
timespanUnion (Timespan a0 a1) (Timespan b0 b1) = 
    Timespan (min a0 b0) (max a1 b1)

scoreTimespan :: Score -> DTimespan 
scoreTimespan (Score (t,_) _) = t
scoreTimespan (Leaf  (t,_) _) = t
          

moveTimespan :: Num u => u -> Timespan u -> Timespan u
moveTimespan dx (Timespan a0 a1) = Timespan (dx + a0) (dx + a1)

moveLocale :: Double -> Locale -> Locale
moveLocale dx (tspan, Frame o tx) = (moveTimespan dx tspan, Frame (o+dx) tx)


scaleTimespan :: Num u => u -> Timespan u -> Timespan u
scaleTimespan sx (Timespan a0 a1) = Timespan (sx * a0) (sx * a1)


scaleLocale :: Double -> Locale -> Locale
scaleLocale sx (tspan, Frame o tx) = (scaleTimespan sx tspan, Frame o (sx * tx))


--------------------------------------------------------------------------------
-- Format instances

instance Format Score where
  format (Leaf m prims) = indent 2 $ vcat [ text "** Leaf-score **"
                                          , fmtLocale m 
                                          , fmtPrimlist prims ]

  format (Score m scos) = indent 2 $ vcat [ text "** Tree-score **"
                                          , fmtLocale m
                                          , fmtScos scos ]



fmtScos :: JoinList Score -> Doc
fmtScos ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])


fmtLocale :: Locale -> Doc
fmtLocale (tspan,fr) = fmtTimespan tspan <+> fmtFrame fr


fmtPrimlist :: JoinList PrimStmt -> Doc
fmtPrimlist ones = F.foldl' (\ac e -> vconcat ac (fmtStmt e)) empty ones


fmtTimespan :: DTimespan -> Doc
fmtTimespan (Timespan t0 t1) = tupled [dtrunc t0, dtrunc t1]

fmtFrame :: Frame -> Doc
fmtFrame (Frame o sx) = 
    semiBraces [ text "o:" <> dtrunc o, text "sx:" <> dtrunc sx ]

instance Format CsoundValue where
  format (CsDouble d) = dtrunc d
  format (CsInt i)    = int i
  format (CsString s) = dquotes $ text s
  


fmtStmt :: PrimStmt -> Doc
fmtStmt (TableStmt dt props) = 
      char 'f' <+> padr 5 (int $ table_assign_num props) 
               <+> dtrunc dt 
               <+> int (table_size props)
               <+> int (table_genroutine props)
               <+> list (map format $ table_args props)

fmtStmt (InstStmt dt props) = 
      char 'i' <+> padr 5 (int $ inst_num props)
               <+> dtrunc dt 
               <+> dtrunc (inst_dur props)
               <+> list (map format $ inst_pfields props)


