{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Tabular
-- Copyright   :  (c) Stephen Tetley 2009, 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Hex dumps
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.JoinPrint.Tabular
  ( 
    Align(..)
  , alignPad

  , columns2
  , columns3
  , columns4

  ) where


import Text.PrettyPrint.JoinPrint.Core




data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Eq,Show)



alignPad :: Align -> Int -> Char -> Doc -> Doc
alignPad AlignLeft   = column_truncr_padr
alignPad AlignCenter = center_truncr
alignPad AlignRight  = column_truncr_padl


-- | A (long) zipWith and a foldr performed together.
--
longZipWith2 :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
longZipWith2 op a0 b0 = step where
    step []     []        = []
    step []     (b:bs)    = op a0 b  : step [] bs
    step (a:as) []        = op a  b0 : step as []
    step (a:as) (b:bs)    = op a  b  : step as bs


longZipWith3 :: (a -> b -> c -> d) -> a -> b -> c -> [a] -> [b] -> [c] -> [d]
longZipWith3 op a0 b0 c0 = step where
    step []     []     []     = []
    step []     []     (c:cs) = op a0 b0 c  : step [] [] cs
    step []     (b:bs) []     = op a0 b  c0 : step [] bs []
    step []     (b:bs) (c:cs) = op a0 b  c  : step [] bs cs
    step (a:as) []     []     = op a  b0 c0 : step as [] []
    step (a:as) []     (c:cs) = op a  b0 c  : step as [] cs
    step (a:as) (b:bs) []     = op a  b  c0 : step as bs []
    step (a:as) (b:bs) (c:cs) = op a  b  c  : step as bs cs


longZipWith4 :: (a -> b -> c -> d -> e) 
             -> a -> b -> c -> d 
             -> [a] -> [b] -> [c] -> [d] 
             -> [e]
longZipWith4 op a0 b0 c0 d0  = step where
    step []     []     []     []     = []
    step []     []     []     (d:ds) = op a0 b0 c0 d  : step [] [] [] ds    
    step []     []     (c:cs) []     = op a0 b0 c  d0 : step [] [] cs []    
    step []     []     (c:cs) (d:ds) = op a0 b0 c  d  : step [] [] cs ds    
    step []     (b:bs) []     []     = op a0 b  c0 d0 : step [] bs [] []
    step []     (b:bs) []     (d:ds) = op a0 b  c0 d  : step [] bs [] ds    
    step []     (b:bs) (c:cs) []     = op a0 b  c  d0 : step [] bs cs []
    step []     (b:bs) (c:cs) (d:ds) = op a0 b  c  d  : step [] bs cs ds    
    step (a:as) []     []     []     = op a  b0 c0 d0 : step as [] [] []
    step (a:as) []     []     (d:ds) = op a  b0 c0 d  : step as [] [] ds
    step (a:as) []     (c:cs) []     = op a  b0 c  d0 : step as [] cs []
    step (a:as) []     (c:cs) (d:ds) = op a  b0 c  d  : step as [] cs ds
    step (a:as) (b:bs) []     []     = op a  b  c0 d0 : step as bs [] []
    step (a:as) (b:bs) []     (d:ds) = op a  b  c0 d  : step as bs [] ds
    step (a:as) (b:bs) (c:cs) []     = op a  b  c  d0 : step as bs cs []
    step (a:as) (b:bs) (c:cs) (d:ds) = op a  b  c  d  : step as bs cs ds


-- | Build a 2 column table.
--
columns2 :: (Int,Align) -> (Int,Align) -> Doc -> [Doc] -> [Doc] -> Doc
columns2 (w1,a1) (w2,a2) sep as bs = 
    vcat $ longZipWith2 op (spacer w1) (spacer w2) as' bs'
  where
    op x y = x <> sep <> y
    as'    = map (alignPad a1 w1 ' ') as
    bs'    = map (alignPad a2 w2 ' ') bs


-- | Build a 3 column table.
--
columns3 :: (Int,Align) -> (Int,Align) -> (Int,Align)
         -> Doc
         -> [Doc] -> [Doc] -> [Doc]
         -> Doc
columns3 (w1,a1) (w2,a2) (w3,a3) sep as bs cs =
    vcat $ longZipWith3 op (spacer w1) (spacer w2) (spacer w3) as' bs' cs'
  where
    op x y z = x <> sep <> y <> sep <> z
    as'      = map (alignPad a1 w1 ' ') as
    bs'      = map (alignPad a2 w2 ' ') bs
    cs'      = map (alignPad a3 w3 ' ') cs

-- | Build a 4 column table.
--
columns4 :: (Int,Align) -> (Int,Align) -> (Int,Align) -> (Int,Align)
         -> Doc
         -> [Doc] -> [Doc] -> [Doc] -> [Doc]
         -> Doc
columns4 (w1,a1) (w2,a2) (w3,a3) (w4,a4) sep as bs cs ds =
    vcat $ longZipWith4 op (spacer w1) (spacer w2) (spacer w3) (spacer w4) 
                           as' bs' cs' ds'
  where
    op a b c d = a <> sep <> b <> sep <> c <> sep <> d
    as'      = map (alignPad a1 w1 ' ') as
    bs'      = map (alignPad a2 w2 ' ') bs
    cs'      = map (alignPad a3 w3 ' ') cs
    ds'      = map (alignPad a4 w4 ' ') ds
