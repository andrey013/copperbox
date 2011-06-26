{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}



module NewPortSpec where

import ZSnd.Core.Inst.ClickNew
import ZSnd.Core.Inst.Prim

import ZSnd.Core.Utils.FormatCombinators hiding ( line )

import Control.Applicative
import qualified Data.Map       as M


demo01 = either print (print . format) $ translateDesc 0 instr0
demo02 = either print (print . format) $ translateDesc 115 instr115


instr0 :: [CStmt]
instr0 = 
    [ DeclE o1 (getElementUniv $ out $ port0_1 440.0)
    , Out o1
    ]
  where
    o1 = mkElemRef 0



instr115 :: [CStmt]
instr115 =  
    [ DeclE o1 (getElementA $ out $ port2_1 $ \k1 a1 -> a1 * cast k1 )
    , DeclE k1 (getElementK $ linen $ port0_4 (pfield 4, pfield 7, pfield 3, pfield 8))
    , DeclE k2 (getElementK $ expon $ port0_3 (pfield 9, pfield 3, pfield 10))
    , DeclE a1 (getElementA $ buzz  $ port1_4 $ \k2 -> (1, pfield 5, k2 + 1, pfield 6))
    , Conn (k1,0) (o1,0)
    , Conn (k2,0) (a1,0)
    , Conn (a1,0) (o1,1)
    , Out o1
    ]          
  where
    k1 = mkElemRef 1 
    k2 = mkElemRef 2
    a1 = mkElemRef 3
    o1 = mkElemRef 4
    a2 = mkElemRef 5

out :: Opcode1 ARate -> Element ARate
out opF =  mkElement "out" inspec Out0
  where
    inspec = applyOpcode opF $ \a -> [ getConfA a ]



linen :: forall rate1 rate. (KA_Rate rate) 
       => Opcode4 rate1 IRate IRate IRate -> Element rate
linen opF = 
    mkElement "linen" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(a,b,c,d) -> [ getConfUniv a
                                             , getConfI b
                                             , getConfI c
                                             , getConfI d ]


expon :: forall rate. (KA_Rate rate) 
       => Opcode3 IRate IRate IRate -> Element rate
expon opF = 
    mkElement "expon" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \(a,b,c) -> [ getConfI a
                                           , getConfI b
                                           , getConfI c ]

 

buzz :: Opcode4 rate1 rate2 KRate IRate -> Element ARate
buzz opF = 
    mkElement "buzz" inspec (Out1 A)
  where
    inspec = applyOpcode opF $ \(a,b,c,d) -> [ getConfUniv a
                                             , getConfUniv b
                                             , getConfK c
                                             , getConfI d ]




newtype Element rate = Element { getElement :: UElement }

mkElement :: String -> InputConfig -> OutConf -> Element rate
mkElement ss ins outspec = Element $ UElement ss ins outspec

getElementI :: Element IRate -> UElement
getElementI = getElement

getElementK :: Element KRate -> UElement
getElementK = getElement

getElementA :: Element ARate -> UElement
getElementA = getElement

getElementUniv :: Element rate -> UElement
getElementUniv = getElement

newtype Conf rate = Conf { getConf :: Expr }
  deriving (Eq,Ord)

instance Show (Conf rate) where
  showsPrec p = showsPrec p . getConf

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





line :: Opcode3 IRate IRate IRate -> UElement
line opF =  UElement "line" portspec (Out1 A)
  where
    portspec = applyOpcode opF $ \(a,b,c) -> [ getConfI a, getConfI b, getConfI c]



dummy1 :: Conf IRate -> Conf KRate -> Config3 IRate IRate IRate
dummy1 = \a b -> (a * 440, pfield 4, cast b)


port0_1 :: Config1 ro1 -> Opcode1 ro1
port0_1 inns = \_ _ -> Right inns

port1_1 :: (Conf ri1 -> Config1 ro1) -> Opcode1 ro1
port1_1 specF = \elt dict ->  
    maybe fk sk $ M.lookup (elt,0) dict
  where
    fk   = Left "unassigned port"
    sk a = Right $ specF (mkConf $ VarE a)

port2_1 :: (Conf ri1 -> Conf ri2 -> Config1 ro1) -> Opcode1 ro1
port2_1 specF = \elt dict -> 
    maybe fk sk ((,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict)
  where
    fk       = Left "unassigned port"
    sk (a,b) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b)

port3_1 :: (Conf ri1 -> Conf ri2 -> Conf ri3 -> Config1 ro1) -> Opcode1 ro1
port3_1 specF = \elt dict -> 
    maybe fk sk ((,,) <$> M.lookup (elt,0) dict <*> M.lookup (elt,1) dict
                      <*> M.lookup (elt,2) dict)
  where
    fk         = Left "unassigned port"
    sk (a,b,c) = Right $ specF (mkConf $ VarE a) (mkConf $ VarE b) (mkConf $ VarE c)



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


-- Port4 

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





dummy2 :: UElement
dummy2 = line $ port2_3 dummy1

dummy3 :: UElement
dummy3 = line $ port0_3 (440, pfield 1, pfield 3)




mkConf :: Expr -> Conf rate
mkConf = Conf

class Rate rate where
  dataRate :: rate -> DataRate

data IRate 
data KRate
data ARate

instance Rate IRate where
  dataRate _ = I

instance Rate KRate where
  dataRate _ = K

instance Rate ARate where
  dataRate _ = A


class Rate rate => KA_Rate rate

instance KA_Rate KRate
instance KA_Rate ARate


class Rate rate => IK_Rate rate

instance IK_Rate IRate
instance IK_Rate KRate


getConfI :: Conf IRate -> Expr
getConfI = getConf

getConfK :: Conf KRate -> Expr
getConfK = getConf

getConfA :: Conf ARate -> Expr
getConfA = getConf

getConfUniv :: Conf rate -> Expr
getConfUniv = getConf

cast :: Conf r1 -> Conf r2
cast a = Conf (getConf a)


pfield  :: Int -> Conf rate
pfield  = Conf . PField


funcall :: String -> Conf rate -> Conf rate
funcall s a = Conf $ Funcall s (getConf a)


binop :: Rator -> Conf rate -> Conf rate -> Conf rate
binop op a b = Conf $ BinOp op (getConf a) (getConf b)

unop :: Rator -> Conf rate -> Conf rate
unop op a = Conf $ UnOp op (getConf a)


instance Num (Conf rate) where
  (+)     = binop (infixL 6 "+")
  (-)     = binop (infixL 6 "-")
  (*)     = binop (infixL 7 "*")
  abs     = funcall "abs"
  negate  = unop (prefix 9 "-")
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Conf $ Literal $ CsInt $ fromInteger i


instance Fractional (Conf rate) where
  (/)     = binop (infixL 7 "/")
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = Conf $ Literal $ CsDouble (fromRational d)




-- Can specify @line@ as 3 /Arg ports/ - promoting them to a 
-- decl-level port of 3?
--


-- Statically we know opcode name (\"line\") and output arity is 1.
-- 
-- We want a Port description that generates three values...
--
