{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.Utils.FormatExpr
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty print expressions with parens.
--
--------------------------------------------------------------------------------

module Sound.FMSS.Utils.FormatExpr
  (

    DocExpr(..)
  , Precedence
  , Rator
  , prefix
  , postfix
  , infixL
  , infixR

  , unparse


  ) where

import Sound.FMSS.Utils.FormatCombinators


type Precedence = Int

data Assoc = AssocNone
           | AssocLeft 
           | AssocRight 
  deriving (Eq,Ord,Show)

data Fixity = Prefix 
            | Postfix 
            | Infix Assoc
  deriving (Eq,Ord,Show)

type Rator = (String, Precedence, Fixity)

data DocExpr = Atom Doc
             | Binary DocExpr Rator DocExpr
             | Unary Rator DocExpr
  deriving (Eq,Show)

instance Format DocExpr where
  format = unparse


prefix :: Precedence -> String -> Rator 
prefix i s = (s,i,Prefix)

postfix :: Precedence -> String -> Rator 
postfix i s = (s,i,Postfix)

infixL :: Precedence -> String -> Rator 
infixL i s = (s,i, Infix AssocLeft)

infixR :: Precedence -> String -> Rator 
infixR i s = (s,i, Infix AssocRight)


higher :: Rator -> Rator -> Bool
higher (_,i,_) (_,j,_) = i > j

equalPrec :: Rator -> Rator -> Bool
equalPrec (_,i,_) (_,j,_) = i == j


bothFixity :: Fixity -> Rator -> Rator -> Bool
bothFixity fx (_,_,a) (_,_,b) = fx == a && fx == b
        
rator_fixity :: Rator -> Fixity
rator_fixity (_,_,fx) = fx

isPostfix :: Rator -> Bool
isPostfix (_,_,a) = a == Postfix

isPrefix :: Rator -> Bool
isPrefix (_,_,a) = a == Prefix


ppInfix :: Doc -> Rator -> Doc -> Doc
ppInfix a (op,_,_) b = a <+> text op <+> b
  
ppPrefix :: Rator -> Doc -> Doc
ppPrefix  (op,_,_) a = text op <> a

ppPostfix :: Doc -> Rator -> Doc
ppPostfix a (op,_,_) = a <> text op 
        
type Fragment = (Rator, Doc)  

bracket :: Fragment -> Assoc -> Rator -> Doc
bracket (kid,doc) assoc parent = deco assoc $ doc
  where
    deco AssocLeft  = if noparensLeft  kid parent then id else parens
    deco AssocRight = if noparensRight kid parent then id else parens
    deco AssocNone  = if noparensUnary kid parent then id else parens



noparensUnary :: Rator -> Rator -> Bool
noparensUnary kid parent
    | higher kid parent = True
    | otherwise         = rator_fixity kid == rator_fixity parent

noparensLeft :: Rator -> Rator -> Bool
noparensLeft kid parent     
    | higher kid parent     = True
    | isPostfix kid         = True
    | equalPrec kid parent  = bothFixity (Infix AssocLeft) kid parent
    | otherwise             = False


noparensRight :: Rator -> Rator -> Bool
noparensRight kid parent     
    | higher kid parent     = True
    | isPrefix kid          = True
    | equalPrec kid parent  = bothFixity (Infix AssocRight) kid parent
    | otherwise             = False


maxprec :: Int                  
maxprec = 16

unparse :: DocExpr -> Doc      
unparse = snd . step
  where
    step (Atom a)        = (maxrator,a)

    step (Binary a op b) = (op, ppInfix l op r)
      where
        l = bracket (step a) AssocLeft op
        r = bracket (step b) AssocRight op

    step (Unary op a)    = fn (rator_fixity op) $ bracket (step a) AssocNone op
      where
        fn Prefix    e = (op, ppPrefix op e)
        fn Postfix   e = (op, ppPostfix e op)
        fn (Infix _) _ = error "imposible"


maxrator :: Rator
maxrator = ("<max-precedence sentinel>", maxprec, Infix AssocNone)
        
        

