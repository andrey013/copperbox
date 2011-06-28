{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst.Index
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Functions built on Click for handling positional (index) 
-- assignment to ports.
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst.Index
  (
    Config1
  , Opcode1
  , OpcodeList1

  , Config2
  , Opcode2
  , OpcodeList2

  , Config3
  , Opcode3

  , Config4
  , Opcode4

  , Config5
  , Opcode5

  , Config6
  , Opcode6

  , Config7
  , Opcode7

  , Config8
  , Opcode8
  
  , Config9
  , Opcode9

  , Config10
  , Opcode10

  , Config11
  , Opcode11

  , applyOpcode
  , mapOpcode
  , mapOpcode2

  -- * Port 1 (arity 1 - 1 input to the opcode)
  , port0_1
  , port1_1
  , port2_1
  , port3_1
  , port4_1  
  , port5_1  
  , port6_1
  , port7_1
  , port8_1
  , port9_1
  , port10_1
  , port11_1

  -- * Port 2 (arity 2 - 2 inputs to the opcode)
  , port0_2
  , port1_2
  , port2_2
  , port3_2
  , port4_2
  , port5_2
  , port6_2
  , port7_2
  , port8_2
  , port9_2
  , port10_2
  , port11_2

  -- * Port 3 (arity 3 - 3 inputs to the opcode)
  , port0_3
  , port1_3
  , port2_3
  , port3_3
  , port4_3
  , port5_3  
  , port6_3
  , port7_3
  , port8_3
  , port9_3
  , port10_3
  , port11_3

  -- * Port 4 (arity 4 - 4 inputs to the opcode)
  , port0_4
  , port1_4
  , port2_4
  , port3_4
  , port4_4
  , port5_4  
  , port6_4
  , port7_4
  , port8_4
  , port9_4
  , port10_4
  , port11_4

  -- * Port 5 (arity 5 - 5 inputs to the opcode)
  , port0_5
  , port1_5
  , port2_5
  , port3_5
  , port4_5
  , port5_5  
  , port6_5
  , port7_5
  , port8_5
  , port9_5
  , port10_5
  , port11_5

  -- * Port 6 (arity 6 - 6 inputs to the opcode)
  , port0_6
  , port1_6
  , port2_6
  , port3_6
  , port4_6
  , port5_6  
  , port6_6
  , port7_6
  , port8_6
  , port9_6
  , port10_6
  , port11_6

  -- * Port 7 (arity 7 - 7 inputs to the opcode)
  , port0_7
  , port1_7
  , port2_7
  , port3_7
  , port4_7
  , port5_7  
  , port6_7
  , port7_7
  , port8_7
  , port9_7
  , port10_7
  , port11_7

  -- * Port 8 (arity 8 - 8 inputs to the opcode)
  , port0_8
  , port1_8
  , port2_8
  , port3_8
  , port4_8
  , port5_8  
  , port6_8
  , port7_8
  , port8_8
  , port9_8
  , port10_8
  , port11_8

  -- * Port 9 (arity 9 - 9 inputs to the opcode)
  , port0_9
  , port1_9
  , port2_9
  , port3_9
  , port4_9
  , port5_9  
  , port6_9
  , port7_9
  , port8_9
  , port9_9
  , port10_9
  , port11_9

  -- * Port 10 (arity 10 - 10 inputs to the opcode)
  , port0_10
  , port1_10
  , port2_10
  , port3_10
  , port4_10
  , port5_10 
  , port6_10
  , port7_10
  , port8_10
  , port9_10
  , port10_10
  , port11_10

  -- * Port 11 (arity 11 - 11 inputs to the opcode)
  , port0_11
  , port1_11
  , port2_11
  , port3_11
  , port4_11
  , port5_11 
  , port6_11
  , port7_11
  , port8_11
  , port9_11
  , port10_11
  , port11_11
  
  ) where

import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed

import Control.Applicative
import qualified Data.Map       as M



type Config1 rate1 = Conf rate1

type Opcode1 rate1 = 
      ElemRef -> PortDict -> Either FailMsg (Config1 rate1)


type OpcodeList1 rate1 = 
      ElemRef -> PortDict -> Either FailMsg [Config1 rate1]

type Config2 rate1 rate2 = (Conf rate1, Conf rate2)

type Opcode2 rate1 rate2 = 
      ElemRef -> PortDict -> Either FailMsg (Config2 rate1 rate2)

type OpcodeList2 rate1 rate2 = 
      ElemRef -> PortDict -> Either FailMsg [Config2 rate1 rate2]

type Config3 rate1 rate2 rate3 = (Conf rate1, Conf rate2, Conf rate3)

type Opcode3 rate1 rate2 rate3 = 
      ElemRef -> PortDict -> Either FailMsg (Config3 rate1 rate2 rate3)

type Config4 rate1 rate2 rate3 rate4 = 
      (Conf rate1, Conf rate2, Conf rate3, Conf rate4)

type Opcode4 rate1 rate2 rate3 rate4 = 
      ElemRef -> PortDict -> Either FailMsg (Config4 rate1 rate2 rate3 rate4)

type Config5 rate1 rate2 rate3 rate4 rate5 = 
      (Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5)

type Opcode5 rate1 rate2 rate3 rate4 rate5 = 
      ElemRef -> PortDict -> Either FailMsg (Config5 rate1 rate2 rate3 
                                                     rate4 rate5)

type Config6 rate1 rate2 rate3 rate4 rate5 rate6 = 
      (Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5, Conf rate6)

type Opcode6 rate1 rate2 rate3 rate4 rate5 rate6 = 
      ElemRef -> PortDict -> Either FailMsg (Config6 rate1 rate2 rate3 
                                                     rate4 rate5 rate6)

type Config7 rate1 rate2 rate3 rate4 rate5 rate6 rate7 = 
      ( Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5
      , Conf rate6, Conf rate7)

type Opcode7 rate1 rate2 rate3 rate4 rate5 rate6 rate7 = 
      ElemRef -> PortDict -> Either FailMsg (Config7 rate1 rate2 rate3 
                                                     rate4 rate5 rate6
                                                     rate7)

type Config8 rate1 rate2 rate3 rate4 rate5 rate6 rate7 rate8 = 
      ( Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5
      , Conf rate6, Conf rate7, Conf rate8)

type Opcode8 rate1 rate2 rate3 rate4 rate5 rate6 rate7 rate8 = 
      ElemRef -> PortDict -> Either FailMsg (Config8 rate1 rate2 rate3 
                                                     rate4 rate5 rate6
                                                     rate7 rate8)


type Config9 rate1 rate2 rate3 rate4 rate5 rate6 rate7 rate8 rate9 = 
      ( Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5
      , Conf rate6, Conf rate7, Conf rate8, Conf rate9)

type Opcode9 rate1 rate2 rate3 rate4 rate5 rate6 rate7 rate8 rate9 = 
      ElemRef -> PortDict -> Either FailMsg (Config9 rate1 rate2 rate3 
                                                     rate4 rate5 rate6
                                                     rate7 rate8 rate9)


type Config10 rate1 rate2 rate3 rate4 rate5 
              rate6 rate7 rate8 rate9 rate10 = 
      ( Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5
      , Conf rate6, Conf rate7, Conf rate8, Conf rate9, Conf rate10)

type Opcode10 rate1 rate2 rate3 rate4 rate5 
              rate6 rate7 rate8 rate9 rate10 = 
      ElemRef -> PortDict -> Either FailMsg (Config10 rate1 rate2 rate3 
                                                      rate4 rate5 rate6
                                                      rate7 rate8 rate9
                                                      rate10)


type Config11 rate1 rate2 rate3 rate4 rate5 
              rate6 rate7 rate8 rate9 rate10 rate11 = 
      ( Conf rate1, Conf rate2, Conf rate3, Conf rate4, Conf rate5
      , Conf rate6, Conf rate7, Conf rate8, Conf rate9, Conf rate10
      , Conf rate11)

type Opcode11 rate1 rate2 rate3 rate4 rate5 
              rate6 rate7 rate8 rate9 rate10 rate11 = 
      ElemRef -> PortDict -> Either FailMsg (Config11 rate1 rate2 rate3 
                                                      rate4 rate5 rate6
                                                      rate7 rate8 rate9
                                                      rate10 rate11)



applyOpcode :: (ElemRef -> PortDict -> Either FailMsg a) -> (a -> b) 
            -> ElemRef -> PortDict -> Either FailMsg b
applyOpcode opF f  = \eref dict -> opF eref dict >>= \ans -> return (f ans)


mapOpcode :: (ElemRef -> PortDict -> Either FailMsg [a]) -> (a -> b) 
          -> ElemRef -> PortDict -> Either FailMsg [b]
mapOpcode opF f  = \eref dict -> opF eref dict >>= \ans -> return (map f ans)


mapOpcode2 :: (ElemRef -> PortDict -> Either FailMsg [(a,b)]) 
           -> ((a,b) -> [c])
          -> ElemRef -> PortDict -> Either FailMsg [c]
mapOpcode2 opF f = \eref dict -> opF eref dict >>= \ans -> 
                                 return (concatMap f ans)


--------------------------------------------------------------------------------
-- Implementations with most general types

port0_gen :: ans -> ElemRef -> PortDict -> Either FailMsg ans
port0_gen inns = \_ _ -> Right inns

port1_gen :: (Conf ri1 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port1_gen specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk            = Left "unassigned port"
    sk a          = Right $ specF (mkConf $ VarE a)


port2_gen :: (Conf ri1 -> Conf ri2 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port2_gen specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk = Left "unassigned port"
    sk (a,b) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)


port3_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port3_gen specF = \elt dict -> 
    maybe fk sk ((,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c)


port4_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port4_gen specF = \elt dict -> 
    maybe fk sk ((,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)


port5_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port5_gen specF = \elt dict -> 
    maybe fk sk ((,,,,) 
      <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,3) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e)


port6_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port6_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4) dict <*> M.lookup (elt,3) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)


port7_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> Conf ri7
                       -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port7_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4) dict <*> M.lookup (elt,5) dict
      <*> M.lookup (elt,6) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f,g) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)
                      (mkConf $ VarE g)


port8_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> Conf ri7
                       -> Conf ri8 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port8_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4) dict <*> M.lookup (elt,5) dict
      <*> M.lookup (elt,6) dict <*> M.lookup (elt,5) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f,g,h) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)
                      (mkConf $ VarE g) (mkConf $ VarE h)


port9_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> Conf ri7
                       -> Conf ri8 -> Conf ri9 -> ans) 
          -> ElemRef -> PortDict -> Either FailMsg ans
port9_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4) dict <*> M.lookup (elt,5) dict
      <*> M.lookup (elt,6) dict <*> M.lookup (elt,7) dict
      <*> M.lookup (elt,8) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f,g,h,i) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)
                      (mkConf $ VarE g) (mkConf $ VarE h)
                      (mkConf $ VarE i)


port10_gen :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                        -> Conf ri5 -> Conf ri6 -> Conf ri7
                        -> Conf ri8 -> Conf ri9 -> Conf ri10
                        -> ans) 
           -> ElemRef -> PortDict -> Either FailMsg ans
port10_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,,,,,) <$> 
          M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4) dict <*> M.lookup (elt,5) dict
      <*> M.lookup (elt,6) dict <*> M.lookup (elt,7) dict
      <*> M.lookup (elt,8) dict <*> M.lookup (elt,9) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f,g,h,i,j) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)
                      (mkConf $ VarE g) (mkConf $ VarE h)
                      (mkConf $ VarE i) (mkConf $ VarE j)


port11_gen :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                        -> Conf ri5  -> Conf ri6 -> Conf ri7
                        -> Conf ri8  -> Conf ri9 -> Conf ri10
                        -> Conf ri11 -> ans) 
           -> ElemRef -> PortDict -> Either FailMsg ans
port11_gen specF = \elt dict -> 
    maybe fk sk ((,,,,,,,,,,) <$> 
          M.lookup (elt,0)  dict <*> M.lookup (elt,1) dict
      <*> M.lookup (elt,2)  dict <*> M.lookup (elt,3) dict
      <*> M.lookup (elt,4)  dict <*> M.lookup (elt,5) dict
      <*> M.lookup (elt,6)  dict <*> M.lookup (elt,7) dict
      <*> M.lookup (elt,8)  dict <*> M.lookup (elt,9) dict
      <*> M.lookup (elt,10) dict)
  where
    fk = Left "unassigned port"
    sk (a,b,c,d,e,f,g,h,i,j,k) = 
        Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                      (mkConf $ VarE c) (mkConf $ VarE d)
                      (mkConf $ VarE e) (mkConf $ VarE f)
                      (mkConf $ VarE g) (mkConf $ VarE h)
                      (mkConf $ VarE i) (mkConf $ VarE j)
                      (mkConf $ VarE k)


--------------------------------------------------------------------------------
-- Port 1

port0_1 :: Config1 ro1 -> Opcode1 ro1
port0_1 = port0_gen 

port1_1 :: (Conf ri1 -> Config1 ro1) 
        -> Opcode1 ro1
port1_1 = port1_gen


port2_1 :: (Conf ri1 -> Conf ri2 -> Config1 ro1) 
        -> Opcode1 ro1
port2_1 = port2_gen


port3_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config1 ro1) 
        -> Opcode1 ro1
port3_1 = port3_gen

port4_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 -> Config1 ro1) 
        -> Opcode1 ro1
port4_1 = port4_gen


port5_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Config1 ro1) 
        -> Opcode1 ro1
port5_1 = port5_gen


port6_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Config1 ro1) 
        -> Opcode1 ro1
port6_1 = port6_gen


port7_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config1 ro1) 
        -> Opcode1 ro1
port7_1 = port7_gen


port8_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Config1 ro1) 
        -> Opcode1 ro1
port8_1 = port8_gen 


port9_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 -> Config1 ro1) 
        -> Opcode1 ro1
port9_1 = port9_gen


port10_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config1 ro1) 
         -> Opcode1 ro1
port10_1 = port10_gen


port11_1 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 -> Config1 ro1) 
         -> Opcode1 ro1
port11_1 = port11_gen

--------------------------------------------------------------------------------
-- Port 2

port0_2 :: Config2 ro1 ro2 -> Opcode2 ro1 ro2
port0_2 = port0_gen


port1_2 :: (Conf ri1 -> Config2 ro1 ro2) -> Opcode2 ro1 ro2
port1_2 = port1_gen


port2_2 :: (Conf ri1 -> Conf ri2 -> Config2 ro1 ro2) -> Opcode2 ro1 ro2
port2_2 = port2_gen


port3_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port3_2 = port3_gen


port4_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port4_2 = port4_gen


port5_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port5_2 = port5_gen


port6_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port6_2 = port6_gen


port7_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port7_2 = port7_gen


port8_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port8_2 = port8_gen 


port9_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port9_2 = port9_gen


port10_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config2 ro1 ro2) 
         -> Opcode2 ro1 ro2
port10_2 = port10_gen


port11_2 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 -> Config2 ro1 ro2) 
         -> Opcode2 ro1 ro2
port11_2 = port11_gen

--------------------------------------------------------------------------------
-- Port 3

port0_3 :: Config3 ro1 ro2 ro3 -> Opcode3 ro1 ro2 ro3
port0_3 = port0_gen


port1_3 :: (Conf ri1 -> Config3 ro1 ro2 ro3) -> Opcode3 ro1 ro2 ro3
port1_3 = port1_gen


port2_3 :: (Conf ri1 -> Conf ri2 -> Config3 ro1 ro2 ro3) -> Opcode3 ro1 ro2 ro3
port2_3 = port2_gen


port3_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port3_3 = port3_gen


port4_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port4_3 = port4_gen


port5_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port5_3 = port5_gen


port6_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port6_3 = port6_gen


port7_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port7_3 = port7_gen


port8_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port8_3 = port8_gen 


port9_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port9_3 = port9_gen


port10_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config3 ro1 ro2 ro3) 
         -> Opcode3 ro1 ro2 ro3
port10_3 = port10_gen


port11_3 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 -> Config3 ro1 ro2 ro3) 
         -> Opcode3 ro1 ro2 ro3
port11_3 = port11_gen

--------------------------------------------------------------------------------
-- Port 4 

port0_4 :: Config4 ro1 ro2 ro3 ro4 -> Opcode4 ro1 ro2 ro3 ro4
port0_4 = port0_gen


port1_4 :: (Conf ri1 -> Config4 ro1 ro2 ro3 ro4) -> Opcode4 ro1 ro2 ro3 ro4
port1_4 = port1_gen


port2_4 :: (Conf ri1 -> Conf ri2 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port2_4 = port2_gen


port3_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port3_4 = port3_gen


port4_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port4_4 = port4_gen



port5_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port5_4 = port5_gen


port6_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port6_4 = port6_gen


port7_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port7_4 = port7_gen


port8_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port8_4 = port8_gen 


port9_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port9_4 = port9_gen


port10_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config4 ro1 ro2 ro3 ro4) 
         -> Opcode4 ro1 ro2 ro3 ro4
port10_4 = port10_gen


port11_4 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 -> Config4 ro1 ro2 ro3 ro4) 
         -> Opcode4 ro1 ro2 ro3 ro4
port11_4 = port11_gen


--------------------------------------------------------------------------------
-- Port 5

port0_5 :: Config5 ro1 ro2 ro3 ro4 ro5
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port0_5 = port0_gen


port1_5 :: (Conf ri1 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port1_5 = port1_gen


port2_5 :: (Conf ri1 -> Conf ri2 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port2_5 = port2_gen


port3_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port3_5 = port3_gen


port4_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port4_5 = port4_gen



port5_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port5_5 = port5_gen


port6_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port6_5 = port6_gen


port7_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port7_5 = port7_gen


port8_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port8_5 = port8_gen 


port9_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 -> Config5 ro1 ro2 ro3 ro4 ro5) 
        -> Opcode5 ro1 ro2 ro3 ro4 ro5
port9_5 = port9_gen


port10_5 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config5 ro1 ro2 ro3 ro4 ro5) 
         -> Opcode5 ro1 ro2 ro3 ro4 ro5
port10_5 = port10_gen


port11_5 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 -> Config5 ro1 ro2 ro3 ro4 ro5) 
         -> Opcode5 ro1 ro2 ro3 ro4 ro5
port11_5 = port11_gen


--------------------------------------------------------------------------------
-- Port 6

port0_6 :: Config6 ro1 ro2 ro3 ro4 ro5 ro6
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port0_6 = port0_gen


port1_6 :: (Conf ri1 -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port1_6 = port1_gen


port2_6 :: (Conf ri1 -> Conf ri2 -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port2_6 = port2_gen


port3_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port3_6 = port3_gen


port4_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port4_6 = port4_gen



port5_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port5_6 = port5_gen


port6_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port6_6 = port6_gen


port7_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port7_6 = port7_gen


port8_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port8_6 = port8_gen 


port9_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 
                     -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
        -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port9_6 = port9_gen


port10_6 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
         -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port10_6 = port10_gen


port11_6 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 
                      -> Config6 ro1 ro2 ro3 ro4 ro5 ro6) 
         -> Opcode6 ro1 ro2 ro3 ro4 ro5 ro6
port11_6 = port11_gen



--------------------------------------------------------------------------------
-- Port 7

port0_7 :: Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port0_7 = port0_gen


port1_7 :: (Conf ri1 -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port1_7 = port1_gen


port2_7 :: (Conf ri1 -> Conf ri2 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port2_7 = port2_gen


port3_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port3_7 = port3_gen


port4_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port4_7 = port4_gen



port5_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port5_7 = port5_gen


port6_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port6_7 = port6_gen


port7_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port7_7 = port7_gen


port8_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port8_7 = port8_gen 


port9_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 
                     -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
        -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port9_7 = port9_gen


port10_7 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
         -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port10_7 = port10_gen


port11_7 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 
                      -> Config7 ro1 ro2 ro3 ro4 ro5 ro6 ro7) 
         -> Opcode7 ro1 ro2 ro3 ro4 ro5 ro6 ro7
port11_7 = port11_gen



--------------------------------------------------------------------------------
-- Port 8

port0_8 :: Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port0_8 = port0_gen


port1_8 :: (Conf ri1 -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port1_8 = port1_gen


port2_8 :: (Conf ri1 -> Conf ri2 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port2_8 = port2_gen


port3_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port3_8 = port3_gen


port4_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port4_8 = port4_gen



port5_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port5_8 = port5_gen


port6_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port6_8 = port6_gen


port7_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port7_8 = port7_gen


port8_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port8_8 = port8_gen 


port9_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 
                     -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
        -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port9_8 = port9_gen


port10_8 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
         -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port10_8 = port10_gen


port11_8 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 
                      -> Config8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8) 
         -> Opcode8 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8
port11_8 = port11_gen




--------------------------------------------------------------------------------
-- Port 9 

port0_9 :: Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port0_9 = port0_gen


port1_9 :: (Conf ri1 -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port1_9 = port1_gen


port2_9 :: (Conf ri1 -> Conf ri2 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port2_9 = port2_gen


port3_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port3_9 = port3_gen


port4_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port4_9 = port4_gen



port5_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port5_9 = port5_gen


port6_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port6_9 = port6_gen


port7_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port7_9 = port7_gen


port8_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port8_9 = port8_gen 


port9_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Conf ri5 -> Conf ri6 -> Conf ri7
                     -> Conf ri8 -> Conf ri9 
                     -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
        -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port9_9 = port9_gen


port10_9 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 -> Conf ri10
                      -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
         -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port10_9 = port10_gen


port11_9 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                      -> Conf ri5  -> Conf ri6 -> Conf ri7
                      -> Conf ri8  -> Conf ri9 -> Conf ri10
                      -> Conf ri11 
                      -> Config9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9) 
         -> Opcode9 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9
port11_9  = port11_gen



--------------------------------------------------------------------------------
-- Port 10

port0_10 :: Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
        -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port0_10 = port0_gen


port1_10 :: (Conf ri1 -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port1_10 = port1_gen


port2_10 :: (Conf ri1 -> Conf ri2 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port2_10 = port2_gen


port3_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port3_10 = port3_gen


port4_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port4_10 = port4_gen



port5_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port5_10 = port5_gen


port6_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port6_10 = port6_gen


port7_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port7_10 = port7_gen


port8_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port8_10 = port8_gen 


port9_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 
                      -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
         -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port9_10 = port9_gen


port10_10 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> Conf ri7
                       -> Conf ri8 -> Conf ri9 -> Conf ri10
                       -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
          -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port10_10 = port10_gen


port11_10 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                       -> Conf ri5  -> Conf ri6 -> Conf ri7
                       -> Conf ri8  -> Conf ri9 -> Conf ri10
                       -> Conf ri11 
                       -> Config10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10) 
          -> Opcode10 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10
port11_10  = port11_gen




--------------------------------------------------------------------------------
-- Port 11

port0_11 :: Config11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
        -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port0_11 = port0_gen


port1_11 :: (Conf ri1 -> Config11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port1_11 = port1_gen


port2_11 :: (Conf ri1 -> Conf ri2 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port2_11 = port2_gen


port3_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 
                      -> Config11 ro1 ro2 ro3 ro4 
                                  ro5 ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port3_11 = port3_gen


port4_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port4_11 = port4_gen



port5_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port5_11 = port5_gen


port6_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port6_11 = port6_gen


port7_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port7_11 = port7_gen


port8_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port8_11 = port8_gen 


port9_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                      -> Conf ri5 -> Conf ri6 -> Conf ri7
                      -> Conf ri8 -> Conf ri9 
                      -> Config11 ro1 ro2 ro3 ro4 ro5 
                                  ro6 ro7 ro8 ro9 ro10 ro11) 
         -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port9_11 = port9_gen


port10_11 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                       -> Conf ri5 -> Conf ri6 -> Conf ri7
                       -> Conf ri8 -> Conf ri9 -> Conf ri10
                       -> Config11 ro1 ro2 ro3 ro4 ro5 
                                   ro6 ro7 ro8 ro9 ro10 ro11) 
          -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port10_11 = port10_gen


port11_11 :: (Conf ri1 -> Conf ri2  -> Conf ri3 -> Conf ri4 
                       -> Conf ri5  -> Conf ri6 -> Conf ri7
                       -> Conf ri8  -> Conf ri9 -> Conf ri10
                       -> Conf ri11 
                       -> Config11 ro1 ro2 ro3 ro4 ro5 
                                   ro6 ro7 ro8 ro9 ro10 ro11) 
          -> Opcode11 ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11
port11_11  = port11_gen
