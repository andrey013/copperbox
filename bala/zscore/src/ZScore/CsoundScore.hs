{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- CsoundScore
--
--------------------------------------------------------------------------------

module ZScore.CsoundScore
  (

    Section(..)
  , DStmt(..)
  , CsoundType(..)

  , ScoBuilder
  , runScoBuilder

  , advance
  , dyngen
  , dyninst
 
  ) where

import ZScore.Utils.FormatCombinators
import ZScore.Utils.HList

import Control.Applicative

-- Is there a pressing need to build syntax?




newtype Section = Section  { getSection :: [DStmt] }
  deriving (Eq,Ord,Show)
      

-- | Dynamically typed statement.
--
data DStmt = 
      TableStmt
        { table_num       :: Int
        , table_atime     :: Double
        , table_size      :: Int
        , table_gennum    :: Int
        , table_args      :: [CsoundType]
        }
    | InstStmt
        { inst_num        :: Int
        , inst_start      :: Double
        , inst_dur        :: Double
        , inst_pfields    :: [Double]
        } 
    | F0Stmt
        { dummy_atime     :: Double }
  deriving (Eq,Ord,Show)


data CsoundType = CsDouble Double
                | CsInt    Int
                | CsString String
  deriving (Eq,Ord,Show)


newtype ScoBuilder a = Build { getBuild :: (a, H DStmt) }



instance Functor ScoBuilder where
  fmap f ma = Build $ let (a,w) = getBuild ma in (f a, w)


instance Applicative ScoBuilder where
  pure a    = Build $ (a, emptyH)
  mf <*> ma = Build $ let (f,w1) = getBuild mf
                          (a,w2) = getBuild ma
                      in (f a, w1 `appendH` w2)

instance Monad ScoBuilder where
  return a  = Build $ (a, emptyH)
  ma >>= k  = Build $ let (a,w1) = getBuild ma
                          (b,w2) = (getBuild . k) a
                      in (b, w1 `appendH` w2)


runScoBuilder :: ScoBuilder a -> Section
runScoBuilder ma = post $ getBuild ma
  where
    post (_,w) = Section { getSection = toListH w }


advance :: Double -> ScoBuilder ()
advance d = Build $ ((), wrapH $ F0Stmt d )

-- | Dynamically typed gen statement.
--
dyngen :: Int -> Int -> Double -> Int -> [CsoundType] -> ScoBuilder ()
dyngen gnum i d sz args = Build $ ((), wrapH $ stmt1 )
  where
    stmt1 = TableStmt { table_num       = i
                      , table_atime     = d
                      , table_size      = sz
                      , table_gennum    = gnum
                      , table_args      = args 
                      }


-- | Dynamically typed inst statement.
--
dyninst :: Int -> Double -> Double -> [Double] -> ScoBuilder ()
dyninst i d dur args = Build $ ((), wrapH $ stmt1 )
  where
    stmt1 = InstStmt { inst_num         = i
                     , inst_start       = d
                     , inst_dur         = dur
                     , inst_pfields     = args
                     }

--------------------------------------------------------------------------------
-- Format instances

instance Format CsoundType where
  format (CsDouble d) = dtrunc d
  format (CsInt i)    = int i
  format (CsString s) = dquotes $ text s
  

instance Format Section where
  format (Section xs) = vcat (map format xs)

instance Format DStmt where
  format (TableStmt i s sz n xs) = 
      char 'f' <+> padr 6 (int i) <+> padl 10 (dtrunc s) <+> padl 6 (int sz)
               <+> padl 6 (int n) <+> hsep (map (padl 4 . format) xs)

  format (InstStmt i s d xs) = 
      char 'i' <+> padr 6 (int i) <+> padl 10 (dtrunc s) <+> padl 10 (dtrunc d)
               <+> hsep (map (padl 10 . dtrunc) xs)

  format (F0Stmt s) = text "f0" <+> padl 10 (dtrunc s)

