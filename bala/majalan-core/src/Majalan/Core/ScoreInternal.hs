{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Core.ScoreInternal
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

module Majalan.Core.ScoreInternal
  (

    PrimScore(..)

  , Timespan(..)
  , Frame(..)
  , Locale
  , PrimStmt(..)
  , AbsPrimStmt(..)
  , GenStmt(..)
  , InstStmtProps(..)
  , CsValue(..)

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

import Majalan.Core.Utils.FormatCombinators
import Majalan.Core.Utils.JoinList hiding ( empty )

-- import Control.Applicative
import qualified Data.Foldable as F


data PrimScore = PrimScore Locale (JoinList PrimScore)
               | PrimLeaf  Locale (JoinList PrimStmt)
  deriving (Show)          



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


-- TODO - Table statements could go in the Locale then they 
-- are printed first...
--
type Locale = (DTimespan, Frame, [GenStmt])




-- | Dynamically typed, delta onset timed statement.
--
data PrimStmt = InstStmt  Double InstStmtProps
  deriving (Eq,Ord,Show)


-- | Dynamically typed, absolute timed statement.
--
data AbsPrimStmt = AbsInstStmt Double InstStmtProps
  deriving (Eq,Ord,Show)


data GenStmt = GenStmt
       { table_assign_num     :: Int  -- corresponds to table-ref in orch
       , table_size           :: Int
       , table_genroutine     :: Int
       , table_args           :: [CsValue]
       }
  deriving (Eq,Ord,Show)

data InstStmtProps = InstStmtProps 
        { inst_num            :: Int
        , inst_dur            :: Double
        , inst_params         :: [CsValue]
        } 
  deriving (Eq,Ord,Show)

data CsValue = CsInt    Int
             | CsDouble Double
             | CsString String          -- for file names
  deriving (Eq,Ord,Show)

absPrimStart :: AbsPrimStmt -> Double
absPrimStart (AbsInstStmt  ot _) = ot

absPrimDuration :: AbsPrimStmt -> Double
absPrimDuration (AbsInstStmt _ p)  = inst_dur p



standardFrame :: Frame
standardFrame = Frame { frame_origin   = 0
                      , frame_scaling  = 1
                      }

timespanUnion :: Ord u => Timespan u -> Timespan u -> Timespan u
timespanUnion (Timespan a0 a1) (Timespan b0 b1) = 
    Timespan (min a0 b0) (max a1 b1)

scoreTimespan :: PrimScore -> DTimespan 
scoreTimespan (PrimScore (t,_,_) _) = t
scoreTimespan (PrimLeaf (t,_,_) _) = t
          

moveTimespan :: Num u => u -> Timespan u -> Timespan u
moveTimespan dx (Timespan a0 a1) = Timespan (dx + a0) (dx + a1)

moveLocale :: Double -> Locale -> Locale
moveLocale dx (tspan, Frame o tx, gs) = 
    (moveTimespan dx tspan, Frame (o+dx) tx, gs)


scaleTimespan :: Num u => u -> Timespan u -> Timespan u
scaleTimespan sx (Timespan a0 a1) = Timespan (sx * a0) (sx * a1)


scaleLocale :: Double -> Locale -> Locale
scaleLocale sx (tspan, Frame o tx, gs) = 
    (scaleTimespan sx tspan, Frame o (sx * tx), gs)


--------------------------------------------------------------------------------
-- Format instances

instance Format PrimScore where
  format (PrimScore m scos) = indent 2 $ vcat [ text "** Tree-score **"
                                              , fmtLocale m
                                              , fmtScos scos ]

  format (PrimLeaf m prims) = indent 2 $ vcat [ text "** Leaf-score **"
                                              , fmtLocale m 
                                              , fmtPrimlist prims ]




fmtScos :: JoinList PrimScore -> Doc
fmtScos ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])


fmtLocale :: Locale -> Doc
fmtLocale (tspan,fr,gs) = 
    vconcat (fmtTimespan tspan <+> fmtFrame fr)
            (list $ map fmtGenStmt gs)



fmtPrimlist :: JoinList PrimStmt -> Doc
fmtPrimlist ones = F.foldl' (\ac e -> vconcat ac (fmtStmt e)) empty ones


fmtTimespan :: DTimespan -> Doc
fmtTimespan (Timespan t0 t1) = tupled [dtrunc t0, dtrunc t1]

fmtFrame :: Frame -> Doc
fmtFrame (Frame o sx) = 
    semiBraces [ text "o:" <> dtrunc o, text "sx:" <> dtrunc sx ]


fmtGenStmt :: GenStmt -> Doc
fmtGenStmt stmt = 
      char 'f' <+> padr 5 (int $ table_assign_num stmt) 
               <+> int 0
               <+> int (table_size stmt)
               <+> int (table_genroutine stmt)
               <+> list (map format $ table_args stmt)

fmtStmt :: PrimStmt -> Doc
fmtStmt (InstStmt dt props) = 
      char 'i' <+> padr 5 (int $ inst_num props)
               <+> dtrunc dt 
               <+> dtrunc (inst_dur props)
               <+> list (map format $ inst_params props)


instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d
  format (CsString s) = dquotes $ text s