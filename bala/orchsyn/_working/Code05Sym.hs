{-# OPTIONS -Wall #-}


module Code05Sym where


import Code05 ( Expr(..), ppExpr )

import Prelude hiding ( (*), (+), (==) )


-- | This is cool but weird, we can assert ref types with a type 
-- proxy and disallow bad user code, /but/ the proxy types 
-- do not need to be accommodated in the interpretor function 
-- @Code@.
--
data Ref

-- | Adding ref cells and sequencing.
--
class Sym repr where
  pfield        :: Int -> repr Expr
  signal        :: Double -> repr Expr
  opcode        :: String -> [repr Expr] -> repr Expr
  binop         :: String -> repr Expr  -> repr Expr -> repr a
  sequ          :: repr Expr -> repr Expr -> repr Expr
  let_          :: repr a -> (repr a -> repr b) -> repr b
  letv_         :: repr a -> (repr Ref -> repr b) -> repr b
  set_ref       :: repr Ref -> repr Expr -> repr Expr
  get_ref       :: repr Ref -> repr Expr
  lam           :: (repr a -> repr b) -> repr (a -> b)
  app           :: repr (a -> b) -> repr a -> repr b
  cond          :: repr Bool -> repr a -> repr a -> repr a


out :: Sym repr => repr Expr -> repr Expr
out e = opcode "out" [e]

oscil :: Sym repr => repr Expr -> repr Expr -> repr Expr -> repr Expr
oscil iamp ihz ifn = opcode "oscil" [ iamp, ihz, ifn ]

infixl 7 *

(*) :: Sym repr => repr Expr -> repr Expr -> repr Expr
(*) = binop "*"

infixl 6 +

(+) :: Sym repr => repr Expr -> repr Expr -> repr Expr
(+) = binop "+"


infix 4 ==

(==) :: Sym repr => repr Expr -> repr Expr -> repr Bool
(==) = binop "=="

-- Same fixity and assoc as (>>)
infixl 1 ##

(##) :: Sym repr => repr Expr -> repr Expr -> repr Expr
(##) = sequ

-- Code must provide variables so it keeps a count (cf. State monad).



newtype Code a = Code { unC :: Int -> (Expr,Int) }

-- | Variable numbering starts at 1
runCode :: Code a -> Expr 
runCode s = fst $ (unC s) 1


instance Sym Code where
  pfield i        = Code $ \s -> (Pfield i, s)
  signal d        = Code $ \s -> (Signal d, s)
  opcode nm es    = Code $ \s -> let (o1,s1) = stmap unC es s
                                 in (Opcode nm o1, s1)
  binop nm e1 e2  = Code $ \s -> let (o1,s1) = unC e1 s
                                     (o2,s2) = unC e2 s1
                                 in (BinOp nm o1 o2, s2)

  sequ e1 e2      = Code $ \s -> let (o1,s1) = unC e1 s
                                     (o2,s2) = unC e2 s1
                                 in (Seq o1 o2, s2)
                                 
  let_ e f        = Code $ \s -> let v       = 'a' : show s
                                     (o1,s1) = unC e (succ s)
                                     (o2,s2) = unC (f $ Code $ \i -> (Var v,i)) s1
                                 in (Let v o1 o2, s2)
                                     
  letv_ e f       = Code $ \s -> let v       = 'a' : show s
                                     (o1,s1) = unC e (succ s)
                                     (o2,s2) = unC (f $ Code $ \i -> (Var v,i)) s1
                                 in (Let v (CreateRef o1) o2, s2)
                                     
  set_ref e1 e2   = Code $ \s -> let (o1,s1) = unC e1 s
                                     (o2,s2) = unC e2 s1
                                 in (WriteRef o1 o2, s2)

  get_ref e       = Code $ \s -> let (o1,s1) = unC e s
                                 in (ReadRef o1, s1)

  lam f           = Code $ \s -> let v       = 'z' : show s
                                     (o1,s1) = unC (f $ Code $ \i -> (Var v,i)) (succ s)
                                 in (Lam v o1, s1)

  app e1 e2       = Code $ \s -> let (o1,s1) = unC e1 s
                                     (o2,s2) = unC e2 s1
                                 in (App o1 o2, s2)

  cond ce e1 e2   = Code $ \s -> let (o1,s1) = unC ce s
                                     (o2,s2) = unC e1 s1
                                     (o3,s3) = unC e2 s1
                                 in (Cond o1 o2 o3, s3)


stmap :: (a -> st -> (b,st)) -> [a] -> st -> ([b],st)
stmap f xs s0 = go s0 xs
  where
    go s []     = ([],s)
    go s (x:xs) = let (b,s1) = f x s
                      (bs,s2) = go s1 xs
                  in (b:bs,s2)


d01 :: Sym repr => repr Expr
d01 = pfield 1

test01 = ppExpr $ runCode d01

code01 :: Sym repr => repr Expr
code01 = let_ a1 $ \x1 -> out x1
  where
    a1 = oscil (signal 1.0) (signal 440.0) (pfield 1)

test02 = ppExpr $ runCode code02


code02 :: Sym repr => repr Expr
code02 = let_ a1 (\x1 -> 
         let_ a2 (\x2 -> 
         out (x1 + x2)))
  where
    a1 = oscil (signal 1.0) (signal 440.0) (pfield 1)
    a2 = oscil (signal 1.0) (signal 660.0) (pfield 1)


test03 = ppExpr $ runCode code03

-- | What about get_ref?
--
code03 :: Sym repr => repr Expr
code03 = letv_ (signal 0) (\x1 -> 
         let_  a1         (\x2 -> 
             out (get_ref x1 + x2) ## set_ref x1 (get_ref x1 + x2) ))
  where
    a1 = oscil (signal 1.0) (signal 440.0) (pfield 1)



-- | What about get_ref?
--
code04 :: Sym repr => repr Expr
code04 = let_  (pfield 5) (\x1 -> 
         let_  final      (\fn ->
           cond (x1 == signal 1)
                (let_ a1 (\x2 -> app fn x2))
                (let_ a2 (\x2 -> app fn x2))))
  where
    final = lam (\x1 -> out x1)
    a1 = oscil (signal 1.0) (signal 440.0) (pfield 1)
    a2 = oscil (signal 1.0) (signal 660.0) (pfield 1)


test04 = ppExpr $ runCode code04
