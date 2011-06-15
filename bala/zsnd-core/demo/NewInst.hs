{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

-- 101 instrument from the Csound book...

module NewInst where


import ZSnd.Core.Utils.FormatCombinators


import Control.Applicative hiding ( Const )

demo01 = format $ pfield 4

pfield :: Int -> Expr rate
pfield = Expr . PField

{-
orch01 :: Orch
orch01 = Orch default_mono_header [inst1]
  where
    inst1 = runInstBuilder 1 $ do 
      a1   <- oscil 10000 440 1
      out a1
-}

type Signal rate = SignalF rate (Expr rate)


newtype SignalF rate a = SignalF { getSignalF :: a }

instance Functor (SignalF rate) where
  fmap f = SignalF . f . getSignalF


instance Applicative (SignalF rate) where
  pure a = SignalF a
  mf <*> ma = SignalF $ let f = getSignalF mf
                            a = getSignalF ma
                        in f a

instance Monad (SignalF rate) where
  return a = SignalF a
  ma >>= k = SignalF $ getSignalF (k $ getSignalF ma)

infixl 1 ->-

(->-) :: SignalF r1 a -> (a -> SignalF r1 b) -> SignalF r1 b
(->-) = (>>=)

coerce :: SignalF r1 a -> SignalF r2 a
coerce (SignalF a) = SignalF a

both :: SignalF r1 a -> SignalF r2 b -> SignalF r3 (a,b)
both af ag = (,) <$> coerce af <*> coerce ag

idur :: Expr IR
idur = pfield 3


kenv :: Signal KR
kenv = linen (ampdb $ pfield 4) (pfield 6) idur (pfield 7)


ivibdel :: Expr IR
ivibdel = pfield 8


kvibenv :: Signal KR
kvibenv = linseg 0 ivibdel 1 (idur - ivibdel) 0.3


klfo :: Signal KR
klfo = kvibenv ->- oscil'
  where
    oscil' a = oscil (a * pfield 9) (pfield 10) 1


asig :: Signal AR 
asig = both kenv klfo ->- buzz'
  where
    buzz' (a,b) = buzz a (b + cpspch (pfield 5)) (pfield 11) 1

instr :: Signal AR 
instr = both kenv klfo ->- buzz' ->- out
  where
    buzz' (a,b) = buzz a (b + cpspch (pfield 5)) (pfield 11) 1
  

-- | 
ampdb     :: Expr a -> Expr a
ampdb     = Expr . Funcall "ampdb" . getExpr

-- | 
cpspch    :: Expr a -> Expr a
cpspch    = Expr . Funcall "cpspch" . getExpr


linen :: Expr a -> Expr b -> Expr c -> Expr d -> Signal rate
linen _ _ _ _ = pure 1

linseg :: Expr a -> Expr b -> Expr c -> Expr d -> Expr e -> Signal rate
linseg _ _ _ _ _ = pure 1

oscil :: Expr a -> Expr b -> Expr c -> Signal rate
oscil _ _ _ = pure 1

buzz :: Expr a -> Expr b -> Expr c -> Expr d -> Signal rate
buzz _ _ _ _ = pure 1



out :: Expr a -> Signal rate
out _ = pure 1



data IR
data KR
data AR

data DExpr = PField  Int
           | Const   CsValue
           | UnOp    String DExpr
           | BinOp   String DExpr DExpr
           | Funcall String DExpr
  deriving (Eq,Ord,Show) 

data CsValue = CsInt Int
             | CsDouble Double
  deriving (Eq,Ord,Show)

newtype Expr rate = Expr { getExpr :: DExpr }
  deriving (Eq,Ord,Show)


binop :: String -> Expr rate -> Expr rate -> Expr rate
binop name a b = Expr $ BinOp name (getExpr a) (getExpr b)

unop :: String -> Expr rate -> Expr rate
unop name a = Expr $ UnOp name (getExpr a)

instance Num (Expr a) where
  (+) = binop "+"
  (-) = binop "-"
  (*) = binop "*"
  abs    = Expr . UnOp "abs" . getExpr
  negate = Expr . UnOp "-"   . getExpr
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Expr $ Const $ CsInt $ fromInteger i


instance Fractional (Expr a) where
  (/) = binop "/"  
  recip _        = error "recip - no interpretation of recip in Csound."  
  fromRational i = Expr $ Const $ CsDouble $ fromRational i

instance Format (Expr rate) where
  format = format . getExpr

instance Format DExpr where
  format (PField i)     = char 'p' <> int i
  format (Const val)    = format val
  format (UnOp ss a)    = text ss <> format a
  format (BinOp ss a b) = format a <> text ss <> format b
  format (Funcall ss a) = text ss <> parens (format a)

instance Format CsValue where
  format (CsInt i)    = int i
  format (CsDouble d) = dtrunc d