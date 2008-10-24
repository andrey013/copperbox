{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.CommonUtils
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Common utils - recursion schemes for Data.Sequences 
-- and pretty print functions
--
--------------------------------------------------------------------------------

module HNotate.CommonUtils where

-- Should be no deps on other HNotate modules

import qualified Data.Foldable as F
import Data.List (unfoldr)
import Data.Sequence hiding (empty, length, reverse)
import qualified Data.Sequence as S
import Prelude hiding (null)
import System.IO
import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- HOF's

-- Reverse application and composition

infixl 7 #

( # ) :: a -> (a -> b) -> b 
x # f = f x


infixl 7 #.

( #. ) :: (a -> b) -> (b -> c) -> (a -> c) 
g #. f = f . g


-- variantions of 'on' 

onr :: (a -> c -> d) -> (b -> c) -> a -> b -> d
op `onr` f = \x y -> x `op` f y

onl :: (c -> b -> d) -> (a -> c) -> a -> b -> d
op `onl` f = \x y -> f x `op` y


-- variations of flipped bind with higher arity

-- (=<<) i.e. do { a <- m1; m a}
effectM :: (Monad m) => (a -> m b) -> m a -> m b
effectM m m1 = m =<< m1 

effectM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
effectM2 m m1 m2 = do { a <- m1; b <- m2; m a b}

effectM3 :: (Monad m) => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
effectM3 m m1 m2 m3 = do { a <- m1; b <- m2; c <- m3; m a b c}

effectM4 :: (Monad m) => (a -> b -> c -> d -> m e) 
                            -> m a -> m b -> m c -> m d -> m e
effectM4 m m1 m2 m3 m4 = do { a <- m1; b <- m2; c <- m3; d <- m4; m a b c d}

-- pairs
fork :: (a -> b) -> (a,a) -> (b,b)
fork f (a,b) = (f a, f b)

forkM :: Monad m => (a -> m b) -> (a,a) -> m (b,b)
forkM f (a,b) = f a >>= \a' -> f b >>= \b' -> return (a',b')


prod :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
prod f g (a,b) = (f a, g b)

prodM :: Monad m => (a -> m c) -> (b -> m d) -> (a,b) -> m (c,d)
prodM f g (a,b) = f a >>= \a' -> g b >>= \b' -> return (a',b') 

dup :: a -> (a,a)
dup a = (a,a)


flipper :: (a -> b -> c -> d) -> b -> c -> a -> d
flipper f y z = \x -> f x y z

--------------------------------------------------------------------------------
-- Morphisms

-- Catamorphism - foldr
cata :: (a -> b -> b) -> b -> Seq a -> b
cata phi b = step . viewl 
  where step EmptyL  = b
        step (e:<se) = phi e (step (viewl se))


-- anamorphism - unfoldr
ana :: (b -> Maybe (a,b)) -> b -> Seq a
ana phi = step
  where step b = case phi b of
                  Nothing -> S.empty
                  Just (a,s) -> a <| step s

-- hylomorphism
hylo :: (b -> Maybe (a, b)) -> (a -> c -> c) -> c -> b -> c
hylo f g a = step 
  where step b = case f b of
                   Nothing -> a
                   Just (x,s) -> g x (step s)

-- paramorphism (generalizes cata)
para :: (a -> (Seq a, b) -> b) -> b -> Seq a -> b
para phi b = step . viewl
  where step EmptyL  = b
        step (e:<se) = phi e (se, step (viewl se))

-- apomorphism (generalizes ana)
apo :: (b -> Maybe (a, b)) -> (b -> Seq a) -> b -> Seq a
apo phi chi b = case phi b of
              Just (a, b') -> a <| apo phi chi b'
              Nothing -> chi b


-- order of args is (cata)para
zygo :: (a -> b -> b) -> (a -> (Seq a, b) -> b) -> b -> Seq a -> b
zygo f g b = step . viewl 
  where step EmptyL  = b
        step (e:<se) = f e (g e (se, (step (viewl se))))


--------------------------------------------------------------------------------
-- Monadic versions

cataM :: Monad m => (a -> b -> m b) -> b -> Seq a -> m b
cataM phi b = mf . viewl
  where
    mf EmptyL   = return b
    mf (e:<se)  = do y <- mf (viewl se)
                     phi e y


anaM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (Seq a)
anaM g b = g b >>= maybe (return S.empty)
                         (\(x,b') -> anaM g b' >>= return . (x <|)) 

-- hylomorphism
hyloM :: Monad m => (a -> c -> m c) -> (b -> m (Maybe (a, b))) -> c -> b -> m c
hyloM d f a = step 
  where step b = f b >>= maybe (return a) (\(x,s) -> step s >>= (d x))

-- paramorphism 
paraM :: Monad m => (a -> (Seq a, b) -> m b) -> b -> Seq a -> m b
paraM phi b = step . viewl
  where step EmptyL  = return b
        step (e:<se) = do se' <- step (viewl se) 
                          phi e (se, se')

-- apomorphism 
apoM :: Monad m => (b -> m (Maybe (a, b))) -> (b -> m (Seq a)) -> b -> m (Seq a)
apoM phi chi b = 
  phi b >>= maybe (chi b)
                  (\(a, b') -> apoM phi chi b' >>= return . (a <|))
              



-- build 
build :: (forall b. (a -> b -> b) -> b -> b) -> Seq a
build g = g (<|) S.empty

-- augment (generalizes build)
augment :: (forall b. (a -> b -> b) -> b -> b) -> Seq a -> Seq a
augment g se = g (<|) se

-- destroy
destroy :: (forall a. (a -> Maybe (b,a)) -> a -> c) -> Seq b -> c
destroy g se = g step se
  where step :: Seq a -> Maybe (a, Seq a)
        step se = case viewl se of
                    EmptyL -> Nothing
                    e :< sse -> Just (e,sse)

--------------------------------------------------------------------------------
-- enum functions for cycles (primarily helpful for pitch letters)

enumFromCyc :: (Bounded a, Enum a, Eq a) => a -> [a]
enumFromCyc a = a : (unfoldr f $ nextOf a)
  where 
    f x | x == a    = Nothing
        | otherwise = Just (x,nextOf x)
    
    nextOf a | a == maxBound = minBound
             | otherwise     = succ a

enumFromToCyc :: (Bounded a, Enum a, Eq a) => a -> a -> [a]
enumFromToCyc a b | a == b    = [a]
                  | otherwise = a : (unfoldr f $ nextOf a) ++ [b]
  where 
    f x | x == a || x == b   = Nothing
        | otherwise                 = Just (x,nextOf x)
    
    nextOf a | a == maxBound = minBound
             | otherwise     = succ a
             
--------------------------------------------------------------------------------
-- splitting a sequence 

-- genSplit c.f. splitAt but with a predicate and state rather 
-- than an index. 
-- Also returns the state, the left seq, the pivot (if found) 
-- and the right seq.
  
genSplit :: (st -> st -> Bool) -> (st -> a -> st) 
             -> st -> Seq a -> (st, Seq a, Maybe a, Seq a)
genSplit test update initial_state = split initial_state S.empty . viewl
  where
    split st acc EmptyL     = (st, acc, Nothing, S.empty)
    split st acc (e :< se)  = let st' = update st e in
                              if (st `test` st' == False)
                                then (st , acc, Just e, se)
                                else split st' (acc |> e) (viewl se)

-- lgs less general splitter             
lgs :: (st -> Bool) -> (st -> a -> st) 
          -> st -> Seq a -> (st, Seq a, Seq a)             
lgs test update st0 = together . genSplit (adapt test) update st0
  where
    adapt f = \old new -> f new
    together (st, l, Nothing, r) = (st,l,r)  
    together (st, l, Just a, r)  = (st,l,a <|r) 


--------------------------------------------------------------------------------
-- other functions
    
-- elsethenif c.f maybe and either
elsethenif :: a -> a -> Bool -> a
elsethenif fk sk p = if p then sk else fk

ifte :: Bool -> a -> a -> a
ifte p sk fk = if p then sk else fk

worklist :: (a -> (b, [a])) -> [a] -> [b]
worklist f = step [] 
  where step cca []     = reverse cca
        step cca (y:ys) = let (b,zs) = f y in step (b:cca) (ys++zs)
        
        
               
unseq :: Seq a -> [a]
unseq = F.foldr (:) [] 


--------------------------------------------------------------------------------
-- PPrint helpers

outputDoc :: FilePath -> Doc -> IO ()
outputDoc filepath doc = do
    h <- openFile filepath WriteMode
    displayIO h (renderPretty 0.7 80 doc)
    hClose h

putDoc80 :: Doc -> IO ()
putDoc80 doc = displayIO stdout (renderPretty 0.7 80 doc)

showDocS :: Doc -> ShowS
showDocS d = displayS (renderPretty 0.7 80 d)

lbanana, rbanana :: Doc
lbanana = text "(|"
rbanana = text "|)"

bananas :: Doc -> Doc
bananas d = lbanana <> d <> rbanana 



underline :: String -> Doc
underline s = text s <$> text (replicate (length s) '-') <> line 
    
-- finger -- a pretty printer for seqeunces 
-- c.f. tupled or list in PPrint - type of the param is different: 
-- Pretty a => Seq a, rather than Seq Doc
finger :: Pretty a => Seq a -> Doc
finger = enclose (text "(|") (text "|)") . genPunctuateSeq pretty comma


    
    

genFinger :: (a -> Doc) -> Seq a -> Doc
genFinger f = enclose (text "(|") (text "|)") . genPunctuateSeq f comma



genPunctuateSeq :: (a -> Doc) -> Doc -> Seq a -> Doc
genPunctuateSeq pp p = para phi empty
  where 
    phi c (se,  d)  | null se        = pp c <> d 
                    | otherwise      = pp c <> p <> d
                   
                        

--------------------------------------------------------------------------------
-- ShowS helpers

spaceS :: Int -> ShowS
spaceS i = showString (replicate i ' ')  
    
    
showNewline, showLBrace, showRBrace :: ShowS
showNewline   = showChar '\n'
showLBrace    = showString "{\n"
showRBrace    = showString "}\n"

foldS :: (ShowS -> ShowS -> ShowS) -> [ShowS] -> ShowS
foldS f []      = id
foldS f xs      = foldr1 f xs

