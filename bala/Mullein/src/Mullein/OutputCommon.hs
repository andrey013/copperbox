{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.OutputCommon
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Common code for LilyPond and ABC output (e.g. same Monad)
--
--------------------------------------------------------------------------------


module Mullein.OutputCommon where

import Mullein.CoreTypes
import Mullein.Duration

import Control.Monad.State
import Data.Sequence ( (><), (<|), (|>), ViewL(..) )
import qualified Data.Sequence as S
import Text.PrettyPrint.Leijen

data St  = St { current_key :: Key, current_meter :: Meter }

type M a = State St a



keyChange :: Key -> (Key -> Doc) -> M (Maybe Doc)
keyChange new pp = do 
    old <- gets current_key 
    if (new==old) 
       then return Nothing
       else do {modify $ \s -> s{current_key=new} ; return $ Just $ pp new } 



meterChange :: Meter -> (Meter -> Doc) -> M (Maybe Doc)
meterChange new pp = do 
    old <- gets current_meter
    if (new==old) 
       then return Nothing
       else do { modify $ \s -> s{current_meter=new} 
               ; return $ Just $ pp new } 


--------------------------------------------------------------------------------
-- output fragments

appendBarFragment :: S.Seq (OutputFragment a) -> Doc -> S.Seq (OutputFragment a)
appendBarFragment se x = se S.|> (BarOutput x)

motifFragment :: Maybe Doc -> Maybe Doc -> [Doc] -> S.Seq (OutputFragment a)
motifFragment opt_key opt_meter xs = 
    mbAdd opt_key $ mbAdd opt_meter $ S.fromList $ map BarOutput xs
  where
    mbAdd a se = maybe se (\x -> (MidtuneCmd x) S.<| se) a



repeated :: S.Seq (OutputFragment BarDiv) -> S.Seq (OutputFragment BarDiv)
repeated se = Prefix RepStart <| (se |> Suffix RepEnd) 

fsrepeat :: S.Seq (OutputFragment BarDiv)
         -> S.Seq (OutputFragment BarDiv)
         -> S.Seq (OutputFragment BarDiv)
         -> S.Seq (OutputFragment BarDiv)
fsrepeat se x y = Prefix  RepStart <| se >< end where
    end = Prefix (NRep 1)  <| x >< (Prefix (NRep 2) <| (y |> Suffix RepEnd)) 


-- If a tune starts with a repeated, the repeat start sign '|:', doesn't
-- need to be printed
dropRepStart :: S.Seq (OutputFragment BarDiv) -> S.Seq (OutputFragment BarDiv)
dropRepStart s0 = step $ S.viewl s0 where
    step (Prefix RepStart :< se) = se
    step _                       = s0


intersperseBars :: [OutputFragment BarDiv] -> [OutputFragment BarDiv]
intersperseBars (BarOutput d1 : BarOutput d2 : xs) = 
    BarOutput d1 : Suffix SglBar : intersperseBars (BarOutput d2 : xs)
intersperseBars (x:xs)      = x : intersperseBars xs
intersperseBars []          = []
