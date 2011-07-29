{-# LANGUAGE TypeFamilies               #-}
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

    Score(..)

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
  , joinScore
  , consGenStmt

  , moveTimespan
  , moveLocale
  , moveScore

  , scaleLocale
  , extendScoreTime
 
  ) where

import Majalan.Core.Basis
import Majalan.Core.Timespan
import Majalan.Core.Utils.FormatCombinators
import Majalan.Core.Utils.JoinList hiding ( empty )

-- import Control.Applicative
import qualified Data.Foldable as F

-- | Score is a rose tree.
--
-- > tree = Leaf [primitive] | Score [tree]
--
data Score = Score Locale (JoinList Score)
           | Leaf  Locale (JoinList PrimStmt)
  deriving (Show)          

type instance DUnit Score = Double


-- | Locale = (bounding timespan * frame * table inits)
--
-- Locale abstracts the length and onset times of (sub-) scores 
-- so they can be moved and concatenated without traversing into 
-- the child elements (inividual instruemtn statements).
-- 
type Locale = (DTimespan, Frame, [GenStmt])


-- | This is equivalent to an affine frame in geometry. It 
-- abstracts the start point (useful for efficient displacement)
-- and allows scaling. 
-- 
-- Scaling might not be musically useful - this structure was 
-- inherited from Wumpus-Core (drawing / geometry) where scaling 
-- is fundamental.
--
data Frame = Frame 
       { frame_origin   :: Double
       , frame_scaling  :: Double
       }
  deriving (Eq,Ord,Show)

type instance DUnit Frame = Double





-- | Dynamically typed, delta onset timed statement.
--
data PrimStmt = InstStmt  Double InstStmtProps
  deriving (Eq,Ord,Show)

type instance DUnit PrimStmt = Double

-- | Dynamically typed, absolute time statement.
--
data AbsPrimStmt = AbsInstStmt Double InstStmtProps
  deriving (Eq,Ord,Show)

type instance DUnit AbsPrimStmt = Double

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


--------------------------------------------------------------------------------
-- 

instance Timeframe Score where
  timeframe (Score (t,_,_) _) = t
  timeframe (Leaf (t,_,_) _)  = t


--------------------------------------------------------------------------------
-- Contructors / operations

absPrimStart :: AbsPrimStmt -> Double
absPrimStart (AbsInstStmt  ot _) = ot

absPrimDuration :: AbsPrimStmt -> Double
absPrimDuration (AbsInstStmt _ p)  = inst_dur p



standardFrame :: Frame
standardFrame = Frame { frame_origin   = 0
                      , frame_scaling  = 1
                      }


joinScore :: Score -> Score -> Score
joinScore  a b =
    Score (tspan,standardFrame,[]) (join (one a) (one b))
  where
    tspan = timeframe b `timespanUnion` timeframe b


consGenStmt :: GenStmt -> Score -> Score
consGenStmt a (Score (ts,fr,gs) body) = Score (ts,fr,a:gs) body
consGenStmt a (Leaf (ts,fr,gs) body)  = Leaf (ts,fr,a:gs) body



moveTimespan :: Num u => u -> Timespan u -> Timespan u
moveTimespan dx (Timespan a0 a1) = Timespan (dx + a0) (dx + a1)

moveLocale :: Double -> Locale -> Locale
moveLocale dx (tspan, Frame o tx, gs) = 
    (moveTimespan dx tspan, Frame (o+dx) tx, gs)


moveScore :: Double -> Score -> Score
moveScore dx (Score loc body) = Score (moveLocale dx loc) body
moveScore dx (Leaf  loc body) = Leaf (moveLocale dx loc) body


scaleLocale :: Double -> Locale -> Locale
scaleLocale sx (tspan, Frame o tx, gs) = 
    (scaleTimespan sx tspan, Frame o (sx * tx), gs)



extendScoreTime :: Double -> Double -> Score -> Score
extendScoreTime d0 d1 sco = case sco of 
    Score (tspan,fr,gs) body -> Score (extT tspan, extF fr,gs) body
    Leaf  (tspan,fr,gs) body -> Leaf  (extT tspan, extF fr,gs) body
  where
    extT (Timespan a0 a1) = Timespan a0 (a1 + d0 + d1)
    extF (Frame o sx)     = Frame (o+d0) sx

--------------------------------------------------------------------------------
-- Format instances

instance Format Score where
  format (Score m scos) = indent 2 $ vcat [ text "** Tree-score **"
                                          , fmtLocale m
                                          , fmtScos scos ]

  format (Leaf m prims) = indent 2 $ vcat [ text "** Leaf-score **"
                                          , fmtLocale m 
                                          , fmtPrimlist prims ]




fmtScos :: JoinList Score -> Doc
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