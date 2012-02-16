{-# OPTIONS -Wall #-}


module Code04Sym where


import Code04 ( Expr(..), ppExpr )

import Prelude hiding ( (*), (+) )


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
  binop         :: String -> repr Expr  -> repr Expr -> repr Expr
  sequ          :: repr Expr -> repr Expr -> repr Expr
  let_          :: repr a -> (repr a -> repr b) -> repr b
  letv_         :: repr a -> (repr Ref -> repr b) -> repr b
  set_ref       :: repr Ref -> repr Expr -> repr Expr
  get_ref       :: repr Ref -> repr Expr



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



