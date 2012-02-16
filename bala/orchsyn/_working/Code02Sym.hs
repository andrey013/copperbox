{-# OPTIONS -Wall #-}


module Code02Sym where


import Code02 ( Expr(..), ppExpr )


-- Unit = ()

-- | Note we use idiomatic final-tagless @let_@ - variables are 
-- provided by the host language (Haskell), they are not explicit 
-- in the Sym representation, but they do need to be introduced 
-- during translation into Expr (the target language with explicit 
-- variables). 
--
class Sym repr where
  pfield        :: Int -> repr Expr
  signal        :: Double -> repr Expr
  oscil         :: repr Expr -> repr Expr -> repr Expr  -> repr Expr
  out           :: repr Expr -> repr Expr
  let_          :: repr a -> (repr a -> repr b) -> repr b


-- Code must provide variables so it keeps a count (cf. State monad).

newtype Code a = Code { unC :: Int -> (Expr,Int) }

-- | Variable numbering starts at 1
runCode :: Code a -> Expr 
runCode s = fst $ (unC s) 1


instance Sym Code where
  pfield i        = Code $ \s -> (Pfield i, s)
  signal d        = Code $ \s -> (Signal d, s)
  oscil e1 e2 e3  = Code $ \s -> let (o1,s1) = unC e1 s
                                     (o2,s2) = unC e2 s1
                                     (o3,s3) = unC e3 s2
                                 in (Oscil o1 o2 o3, s3)
  out e           = Code $ \s -> let (o1,s1) = unC e s
                                 in (Out o1, s1)

  let_ e f        = Code $ \s -> let v       = 'a' : show s
                                     (o1,s1) = unC e (s+1)
                                     (o2,s2) = unC (f $ Code $ \i -> (Var v,i)) s1
                                 in (Let v o1 o2, s2)
                                     


d01 :: Sym repr => repr Expr
d01 = pfield 1

test01 = ppExpr $ runCode d01

code01 :: Sym repr => repr Expr
code01 = let_ a1 $ \x1 -> out x1
  where
    a1 = oscil (signal 1.0) (signal 440.0) (pfield 1)

test02 = ppExpr $ runCode code01



