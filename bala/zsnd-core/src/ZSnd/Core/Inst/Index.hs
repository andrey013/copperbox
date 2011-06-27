{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Inst.Index
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

module ZSnd.Core.Inst.Index
  (
    Config1
  , Opcode1

  , Config2
  , Opcode2
  
  , Config3
  , Opcode3

  , Config4
  , Opcode4
  
  , applyOpcode

  -- * Port 1 (arity 1 - 1 input to the opcode)
  , port0_1
  , port1_1
  , port2_1
  , port3_1
  , port4_1

  -- * Port 2 (arity 2 - 2 inputs to the opcode)
  , port0_2
  , port1_2
  , port2_2
  , port3_2
  , port4_2

  -- * Port 3 (arity 3 - 3 inputs to the opcode)
  , port0_3
  , port1_3
  , port2_3
  , port3_3
  , port4_3

  -- * Port 4 (arity 4 - 4 inputs to the opcode)
  , port0_4
  , port1_4
  , port2_4
  , port3_4
  , port4_4


  
  ) where

import ZSnd.Core.Inst.Click
import ZSnd.Core.Inst.HighLevel
import ZSnd.Core.Inst.Prim

import Control.Applicative
import qualified Data.Map       as M



type Config1 rate1 = Conf rate1

type Opcode1 rate1 = 
      ElemRef -> PortDict -> Either FailMsg (Config1 rate1)

type Config2 rate1 rate2 = (Conf rate1, Conf rate2)

type Opcode2 rate1 rate2 = 
      ElemRef -> PortDict -> Either FailMsg (Config2 rate1 rate2)


type Config3 rate1 rate2 rate3 = (Conf rate1, Conf rate2, Conf rate3)

type Opcode3 rate1 rate2 rate3 = 
      ElemRef -> PortDict -> Either FailMsg (Config3 rate1 rate2 rate3)

type Config4 rate1 rate2 rate3 rate4 = 
      (Conf rate1, Conf rate2, Conf rate3, Conf rate4)

type Opcode4 rate1 rate2 rate3 rate4 = 
      ElemRef -> PortDict -> Either FailMsg (Config4 rate1 rate2 rate3 rate4)


applyOpcode :: (ElemRef -> PortDict -> Either FailMsg a) -> (a -> b) 
            -> ElemRef -> PortDict -> Either FailMsg b
applyOpcode opF f  = \eref dict -> opF eref dict >>= \ans -> return (f ans)


--------------------------------------------------------------------------------
-- Port 1

port0_1 :: Config1 ro1 -> Opcode1 ro1
port0_1 inns = \_ _ -> Right inns

port1_1 :: (Conf ri1 -> Config1 ro1) 
        -> Opcode1 ro1
port1_1 specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk            = Left "unassigned port"
    sk a          = Right $ specF (mkConf $ VarE a)

port2_1 :: (Conf ri1 -> Conf ri2 -> Config1 ro1) 
        -> Opcode1 ro1
port2_1 specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b)      = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)

port3_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config1 ro1) 
        -> Opcode1 ro1
port3_1 specF = \elt dict -> 
    maybe fk sk ((,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                      <*> M.lookup (elt,2) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b,c)    = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                                  (mkConf $ VarE c)


port4_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 -> Config1 ro1) 
        -> Opcode1 ro1
port4_1 specF = \elt dict -> 
    maybe fk sk ((,,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                       <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b,c,d)  = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                                  (mkConf $ VarE c) (mkConf $ VarE d)


--------------------------------------------------------------------------------
-- Port 2

port0_2 :: Config2 ro1 ro2 -> Opcode2 ro1 ro2
port0_2 inns = \_ _ -> Right inns

port1_2 :: (Conf ri1 -> Config2 ro1 ro2) -> Opcode2 ro1 ro2
port1_2 specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk   = Left "unassigned port"
    sk a = Right $ specF (mkConf $ VarE a)

port2_2 :: (Conf ri1 -> Conf ri2 -> Config2 ro1 ro2) -> Opcode2 ro1 ro2
port2_2 specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk       = Left "unassigned port"
    sk (a,b) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)

port3_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port3_2 specF = \elt dict -> 
    maybe fk sk ((,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                      <*> M.lookup (elt,2) dict)
  where
    fk         = Left "unassigned port"
    sk (a,b,c) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) (mkConf $ VarE c)


port4_2 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 -> Config2 ro1 ro2) 
        -> Opcode2 ro1 ro2
port4_2 specF = \elt dict -> 
    maybe fk sk ((,,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                       <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b,c,d)  = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                                  (mkConf $ VarE c) (mkConf $ VarE d)


--------------------------------------------------------------------------------
-- Port 3

port0_3 :: Config3 ro1 ro2 ro3 -> Opcode3 ro1 ro2 ro3
port0_3 inns = \_ _ -> Right inns

port1_3 :: (Conf ri1 -> Config3 ro1 ro2 ro3) -> Opcode3 ro1 ro2 ro3
port1_3 specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk   = Left "unassigned port"
    sk a = Right $ specF (mkConf $ VarE a)

port2_3 :: (Conf ri1 -> Conf ri2 -> Config3 ro1 ro2 ro3) -> Opcode3 ro1 ro2 ro3
port2_3 specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk       = Left "unassigned port"
    sk (a,b) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)


port3_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port3_3 specF = \elt dict -> 
    maybe fk sk ((,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                      <*> M.lookup (elt,2) dict)
  where
    fk         = Left "unassigned port"
    sk (a,b,c) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) (mkConf $ VarE c)


port4_3 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config3 ro1 ro2 ro3) 
        -> Opcode3 ro1 ro2 ro3
port4_3 specF = \elt dict -> 
    maybe fk sk ((,,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                       <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b,c,d)  = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                                  (mkConf $ VarE c) (mkConf $ VarE d)


--------------------------------------------------------------------------------
-- Port 4 

port0_4 :: Config4 ro1 ro2 ro3 ro4 -> Opcode4 ro1 ro2 ro3 ro4
port0_4 inns = \_ _ -> Right inns

port1_4 :: (Conf ri1 -> Config4 ro1 ro2 ro3 ro4) -> Opcode4 ro1 ro2 ro3 ro4
port1_4 specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk   = Left "unassigned port"
    sk a = Right $ specF (mkConf $ VarE a)

port2_4 :: (Conf ri1 -> Conf ri2 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port2_4 specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk       = Left "unassigned port"
    sk (a,b) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)


port3_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port3_4 specF = \elt dict -> 
    maybe fk sk ((,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                      <*> M.lookup (elt,2) dict)
  where
    fk         = Left "unassigned port"
    sk (a,b,c) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) (mkConf $ VarE c)


port4_4 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Conf ri4 
                     -> Config4 ro1 ro2 ro3 ro4) 
        -> Opcode4 ro1 ro2 ro3 ro4
port4_4 specF = \elt dict -> 
    maybe fk sk ((,,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                       <*> M.lookup (elt,2) dict <*> M.lookup (elt,3) dict)
  where
    fk            = Left "unassigned port"
    sk (a,b,c,d)  = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) 
                                  (mkConf $ VarE c) (mkConf $ VarE d)

