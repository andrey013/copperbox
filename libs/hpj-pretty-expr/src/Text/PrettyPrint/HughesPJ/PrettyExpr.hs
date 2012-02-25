{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.HughesPJ.PrettyExpr
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty print expressions with infix, prefix and postfix
-- operators.
--
--------------------------------------------------------------------------------

module Text.PrettyPrint.HughesPJ.PrettyExpr 
  (

    -- * Types
    Assoc(..)
  , Fixity(..)
  , Precedence
  , Spacing(..)
  , Rator
  , DocE(..)

  -- * Specifying operators
  , prefix
  , postfix
  , infixL
  , infixR
  , infixNone


  -- * Useful 
  , literal

  -- * Turn a DocE into a Doc with an Unparser
  , Unparser(..)
  , makeUnparser

  ) where



import Text.PrettyPrint.HughesPJ            -- package: pretty

import Data.List ( foldl') 


-- | Specify the associativity of operators: left, right or
-- none:
--
-- > AssocLeft
-- > AssocRight
-- > AssocNone
--
data Assoc = AssocLeft 
           | AssocRight
           | AssocNone 
  deriving (Bounded, Enum, Eq, Ord, Show)


-- | Specify the fixity of an operator, either prefix, postfix
-- or infix. 
--
-- Infix operators have an associated associativity.
--
data Fixity = Prefix 
            | Postfix 
            | Infix Assoc
  deriving (Eq,Ord,Show)

-- | Precedence level of an operator - this is just a synonym for 
-- 'Int'.
--
type Precedence = Int


data Spacing = Space
             | NoSpace
  deriving (Bounded, Enum, Eq, Ord, Show)

type Rator = (String, Spacing, Precedence, Fixity)

data DocE = Atom Doc
          | Binary Rator DocE DocE
          | Unary Rator DocE
          | Nary Rator [DocE]

instance Show DocE where
  show de = let fn = unparser (makeUnparser 16) in show (fn de)


-- | Specify a prefix operator, supplying its precedence, spacing 
-- and its representation as a String.
--
prefix :: Precedence -> Spacing -> String -> Rator 
prefix i sp s = (s,sp,i,Prefix)


-- | Specify a postfix operator, supplying its precedence, spacing 
-- and its representation as a String.
--
postfix :: Precedence -> Spacing -> String -> Rator 
postfix i sp s = (s,sp,i,Postfix)

-- | Specify a left-associative, infix operator, supplying its 
-- precedence, spacing and its representation as a String.
--
infixL :: Precedence -> Spacing -> String -> Rator 
infixL i sp s = (s,sp,i, Infix AssocLeft)

-- | Specify a right-associative, infix operator, supplying its 
-- precedence, spacing and its representation as a String.
--
infixR :: Precedence -> Spacing -> String -> Rator 
infixR i sp s = (s,sp,i, Infix AssocRight)

-- | Specify a none-associative, infix operator, supplying its 
-- precedence, spacing and its representation as a String.
--
infixNone :: Precedence -> Spacing -> String -> Rator 
infixNone i sp s = (s,sp,i, Infix AssocNone)



-- | Local helper - ordering based on second element of the
-- tuple.
--
higher :: Rator -> Rator -> Bool
higher (_,_,i,_) (_,_,j,_) = i > j

-- | Local helper.
--
equalPrec :: Rator -> Rator -> Bool
equalPrec (_,_,i,_) (_,_,j,_) = i == j


bothFixity :: Fixity -> Rator -> Rator -> Bool
bothFixity fx (_,_,_,a) (_,_,_,b) = fx == a && fx == b
        
rator_fixity :: Rator -> Fixity
rator_fixity (_,_,_,fx) = fx

isPostfix :: Rator -> Bool
isPostfix (_,_,_,a) = a == Postfix

isPrefix :: Rator -> Bool
isPrefix (_,_,_,a) = a == Prefix

ppInfix :: Doc -> Rator -> Doc -> Doc
ppInfix a (op,sp,_,_) b
    | sp == Space = a <+> text op <+> b
    | otherwise   = a <> text op <> b
  
ppPrefix :: Rator -> Doc -> Doc
ppPrefix  (op,sp,_,_) a 
    | sp == Space = text op <+> a
    | otherwise   = text op <> a

ppPostfix :: Doc -> Rator -> Doc
ppPostfix a (op,sp,_,_) 
    | sp == Space = a <+> text op 
    | otherwise   = a <> text op 


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


--------------------------------------------------------------------------------
-- Utility functions


literal                 :: String -> DocE
literal                 = Atom . text




--------------------------------------------------------------------------------

-- | An Unparser is a so-called /first-class module/ where
-- the maximum precedence level is configurable.
-- 
-- Internally an unparser needs to know the maximum precedence
-- level which may vary - for Haskell it is 9, for C it is 16.
--
-- Instead of using a reader monad to supply this information
-- and Unparser is a /module/ instantatiated with the maximum 
-- precedence.
-- 
-- This pattern is used by Parsec\'s Token module for instance.
-- 
data Unparser = Unparser { unparser :: DocE -> Doc }


-- | Create an Unparser with the supplied maximum precedence.
--
makeUnparser :: Precedence -> Unparser
makeUnparser maxprec = Unparser { unparser = unP }
  where
    maxrator :: Rator
    maxrator =  ("<max-precedence sentinel>", Space, maxprec, Infix AssocNone)

    unP :: DocE -> Doc      
    unP = snd . go
      where
        go (Atom a)         = (maxrator,a)

        go (Binary op a b)  = (op, ppInfix l op r)
          where
            l = bracket (go a) AssocLeft op
            r = bracket (go b) AssocRight op

        go (Unary op a)     = fn (rator_fixity op) $ bracket (go a) AssocNone op
          where
            fn Prefix    e  = (op, ppPrefix op e)
            fn Postfix   e  = (op, ppPostfix e op)
            fn (Infix _) _  = error "imposible"

        go (Nary _  [])     = error "unparse - Nary supplied with the empty list"
        go (Nary _  [e])    = go e
        go (Nary op (e:es)) = (op, foldl' addOne leftmost es)
          where
            leftmost        = bracket (go e) AssocLeft op
            addOne leftp r  = ppInfix leftp op (bracket (go r) AssocRight op)
