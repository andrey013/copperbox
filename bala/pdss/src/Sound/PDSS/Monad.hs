{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.PDSS.Monad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Gen monad
--
--------------------------------------------------------------------------------


module Sound.PDSS.Monad
  ( 

    GenMonad
  , run
  , text_

  ) where 


import Control.Applicative hiding ( empty ) 
import Text.PrettyPrint.HughesPJ

data St = St () -- to fill


-- | At least a State and Writer....

newtype GenMonad a = GenMonad { getGenMonad :: St -> (a,St,Doc) }

instance Functor GenMonad where
  fmap f ma = GenMonad $ \s -> let (a,s1,d1) = getGenMonad ma s
                               in (f a,s1,d1)


instance Applicative GenMonad where
  pure a    = GenMonad $ \s -> (a,s,empty)
  mf <*> ma = GenMonad $ \s -> let (f,s1,d1) = getGenMonad mf s
                                   (a,s2,d2) = getGenMonad ma s1
                               in (f a, s2, d1 $+$ d2)



instance Monad GenMonad where
  return  = pure
  m >>= k = GenMonad $ \s -> let (a,s1,d1) = getGenMonad m s
                                 (b,s2,d2) = getGenMonad (k a) s1
                             in (b, s2, d1 $+$ d2)

tell :: Doc -> GenMonad ()
tell d1 = GenMonad $ \s -> ((),s,d1)


run :: (Int,Int,Int,Int) -> Int -> GenMonad a -> String
run props sz ma = render (dcanvas props sz $+$ body)
  where
    body = let (_,_,d1) = getGenMonad ma (St ()) in d1


dcanvas :: (Int,Int,Int,Int) -> Int -> Doc
dcanvas (xp,yp,xsz,ysz) font_size = 
    char '#' <> char 'N' <+> text "canvas" 
             <+> (hsep $ map int [xp,yp,xsz,ysz,font_size]) <> char ';'


dtext :: (Int,Int) -> String -> Doc
dtext (xp,yp) ss = 
    char '#' <> char 'X' <+> text "text" <+> int xp <+> int yp <+> text ss <> char ';'


text_ :: Int -> Int -> String -> GenMonad ()
text_ xp yp ss = tell $ dtext (xp,yp) ss