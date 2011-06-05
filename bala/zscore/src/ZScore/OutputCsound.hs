{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZScore.OutputCsound
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Csound - score (.sco), orrchestra (.orc) or unified 
-- orchestra and score files (.csd). 
--
--------------------------------------------------------------------------------

module ZScore.OutputCsound
  (

    writeSco
--  , writeOrc

  -- * Output a CSD file
--    writeCSD


  ) where


import ZScore.CsoundScore
import ZScore.Utils.FormatCombinators

import Data.List

writeSco :: FilePath -> [Section] -> IO ()
writeSco file_path xs = writeFile file_path $ show $ buildScoreDoc xs

-- | Orchestra file needs a prologue (sr, ksmps, ...)
--
-- writeOrc :: FilePath -> [Inst] -> IO ()
-- writeOrc file_path xs = writeFile file_path $ show $ buildScoreDoc xs



buildScoreDoc :: [Section] -> Doc
buildScoreDoc []     = empty
buildScoreDoc (x:xs) = step x xs
  where
    step a []     = renderSection a `vconcat` char 'e'
    step a (b:bs) = let d1 = renderSection a `vconcat` char 'e' `vconcat` empty
                    in d1 `vconcat` step b bs

-- write Sco files with carry (.) shorthand.
-- (never carry inst).
-- default column width is 5


data CarrySt = CarrySt 
      { instr_num   :: Int
      , args_list   :: [Double]
      }
  deriving (Eq,Show)


init_st :: CarrySt
init_st = CarrySt { instr_num = (-1), args_list = [] }

-- | Differentiate the curent line from the Carry state. 
--
-- Print (.) in the generated output where the values are the 
-- same.
--
oneline :: Int -> Double -> Double -> [Double] -> CarrySt -> (CarrySt, Doc)
oneline inst st dur args s0 
    | inst == instr_num s0 = let doc = diff dprefix (args_list s0) new_args
                             in (CarrySt inst new_args, doc)
    | otherwise            = (CarrySt inst new_args, doc0) 
  where
    new_args              = st:dur:args
    dprefix               = char 'i' <+> padr 5 (int inst)
    field                 = padl 5 . dtrunc
    doc0                  = dprefix <+> hsep (map field (st:dur:args))

    diff ac (x:xs) (y:ys) 
      | x `tEQ` y         = diff (ac <+> padl 5 (char '.')) xs ys
      | otherwise         = diff (ac <+> padl 5 (dtrunc y)) xs ys
    diff ac []     ys     = ac <+> hsep (map field ys)
    diff ac xs     []     = ac <+> hsep (map field xs)
    

renderSection :: Section -> Doc
renderSection = vcat . snd . mapAccumL fn init_st . getSection
  where
    fn _  a@(TableStmt _ _ _ _ _)   = (init_st, format a)
    fn st   (InstStmt i t dur args) = oneline i t dur args st
    fn st a@(F0Stmt _)              = (st, format a)



infix 4 `tEQ`

-- | Tolerant equality - helper function for defining Eq instances
-- that use tolerance.
--
-- Note - the definition actually needs Ord which is 
-- unfortunate (as Ord is /inaccurate/).
--
tEQ :: Double -> Double -> Bool
tEQ a b = (abs (a-b)) < 0.000001