{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundScore
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Build Csound Scores.
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundScore
  (

    Section(..)
  , DStmt(..)
  , CsoundValue(..)

  , ScoBuilder
  , runScoBuilder
  , setTableNum 

  , advance
  , dyngen
  , dyninst
 
  ) where

import ZSnd.Core.Utils.FormatCombinators
import ZSnd.Core.Utils.HList

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
        , table_args      :: [CsoundValue]
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


data CsoundValue = CsDouble Double
                 | CsInt    Int
                 | CsString String
  deriving (Eq,Ord,Show)


type TNum = Int


-- | ScoBuilder is a State-Writer monad.
-- 
-- Writer collects Inst and Table statements.
--
-- State tracks table number for generating fresh tables.
--
newtype ScoBuilder a = Build { getBuild :: TNum -> (a, H DStmt, TNum) }



instance Functor ScoBuilder where
  fmap f ma = Build $ \s -> let (a,w, s1) = getBuild ma s in (f a, w, s1)


instance Applicative ScoBuilder where
  pure a    = Build $ \s -> (a, emptyH, s)
  mf <*> ma = Build $ \s -> let (f,w1,s1) = getBuild mf s
                                (a,w2,s2) = getBuild ma s1
                      in (f a, w1 `appendH` w2,s2)

instance Monad ScoBuilder where
  return a  = Build $ \s -> (a, emptyH, s)
  ma >>= k  = Build $ \s -> let (a,w1,s1) = getBuild ma s
                                (b,w2, s2) = (getBuild . k) a s1
                            in (b, w1 `appendH` w2, s2)


runScoBuilder :: ScoBuilder a -> Section
runScoBuilder ma = post $ getBuild ma 1
  where
    post (_,w,_) = Section { getSection = toListH w }


setTableNum :: Int -> ScoBuilder ()
setTableNum i = Build $ \_ -> ((), emptyH, i)

advance :: Double -> ScoBuilder ()
advance d = Build $ \s -> ((), wrapH $ F0Stmt d, s )



-- | Dynamically typed gen statement.
--
-- > gen_num * time * size * [arg]
--
dyngen :: Int -> Double -> Int -> [CsoundValue] -> ScoBuilder ()
dyngen gnum t sz args = Build $ \s -> ((), wrapH $ stmt1 s, s+1 )
  where
    stmt1 i = TableStmt { table_num       = i
                        , table_atime     = t
                        , table_size      = sz
                        , table_gennum    = gnum
                        , table_args      = args 
                        }


-- | Dynamically typed inst statement.
--
dyninst :: Int -> Double -> Double -> [Double] -> ScoBuilder ()
dyninst i d dur args = Build $ \s -> ((), wrapH $ stmt1, s)
  where
    stmt1 = InstStmt { inst_num         = i
                     , inst_start       = d
                     , inst_dur         = dur
                     , inst_pfields     = args
                     }

--------------------------------------------------------------------------------
-- Format instances

instance Format CsoundValue where
  format (CsDouble d) = dtrunc d
  format (CsInt i)    = int i
  format (CsString s) = dquotes $ text s
  

instance Format Section where
  format (Section xs) = vcat (map format xs)

instance Format DStmt where
  format (TableStmt i s sz n xs) = 
      char 'f' <+> padr 5 (int i) <+> padl 5 (dtrunc s) <+> padl 5 (int sz)
               <+> padl 5 (int n) <+> hsep (map (padl 5 . format) xs)

  format (InstStmt i s d xs) = 
      char 'i' <+> padr 5 (int i) <+> padl 5 (dtrunc s) <+> padl 5 (dtrunc d)
               <+> hsep (map (padl 5 . dtrunc) xs)

  format (F0Stmt s) = text "f0" <+> padl 5 (dtrunc s)

