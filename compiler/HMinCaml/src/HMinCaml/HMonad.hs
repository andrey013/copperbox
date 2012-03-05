{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HMinCaml.HMonad
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC (RankNTypes)
--
-- Monad for syntax processing - state, two environments, 
-- backtracking via two-continuations and fatal errors.
-- 
-- Warning - associativity of `mplus` is not what we want!
--
--------------------------------------------------------------------------------


module HMinCaml.HMonad 
  (
    HMonad
  , runHMonad

  -- * Right asscociative mplus
  , (<<|>) 

  -- * Errors
  , fatal
  , failure
  , (<?>)

  -- * State operations
  , get
  , gets
  , put
  , puts
  , puts_

  -- * Environment (reader) operations
  , ask1
  , ask2
  , asks1
  , asks2
  , local1 
  , local2

  ) where


import Control.Applicative
import Control.Monad

-- | Result supports recoverable error and fatal error.
--
-- Recoverable error backtracks the state.
-- 
-- State is strict.
--
data Result st ans = Fatal String | Fail String | Okay ans !st
  deriving (Eq,Ord,Show)


type SK r1 r2 st a ans = a -> FK st ans -> r1 -> r2 -> st -> Result st ans
type FK st ans   = Result st ans


newtype HMonad r1 r2 st a = HMonad { 
    getHMonad :: forall ans. 
            SK r1 r2 st a ans -> FK st ans -> r1 -> r2 -> st -> Result st ans }


runHMonad :: r1 -> r2 -> st -> HMonad r1 r2 st a -> Either String (a,st)
runHMonad r1 r2 s0 p = post $ getHMonad p sk_zero fk_zero r1 r2 s0
  where
    sk_zero = \ans _ _ _ st -> Okay ans st
    fk_zero = Fail "_initial failure_"

    post (Fatal s)   = Left s
    post (Fail s)    = Left s
    post (Okay a st) = Right (a,st)


instance Functor (HMonad r1 r2 st) where
  fmap f ma = HMonad $ \sk -> getHMonad ma $ \a -> sk (f a)

instance Applicative (HMonad r1 r2 st) where
  pure a    = HMonad $ \sk -> sk a
  af <*> av = HMonad $ \sk -> getHMonad af $ \f -> getHMonad av $ \a -> sk (f a)

instance Monad (HMonad r1 r2 st) where
  return  = pure
  m >>= k = HMonad $ \sk -> getHMonad m $ \a -> getHMonad (k a) sk


instance MonadPlus (HMonad r1 r2 st) where
  mzero = HMonad $ \_ fk _ _ _ -> fk
  mplus = alt


infixr 3 <<|>
(<<|>) :: MonadPlus m => m a -> m a -> m a
(<<|>) = mplus

-- mplus of MonadPlus, (<|>) of Applicative.
--
-- Now with fatal exceptions
--
alt :: HMonad r1 r2 st a -> HMonad r1 r2 st a -> HMonad r1 r2 st a
alt p1 p2 = HMonad $ \sk fk r1 r2 st -> 
    case fk of
      Fatal s -> Fatal s
      hk      -> getHMonad p1 sk (getHMonad p2 sk hk r1 r2 st) r1 r2 st


fatal :: String -> HMonad r1 r2 st a
fatal err_msg = HMonad $ \_ _ _ _ _ -> Fatal err_msg


-- | Equivalent to 'mzero' with an error message.
--
failure :: String -> HMonad r1 r2 st a
failure err_msg = HMonad $ \_ _ _ _ _ -> Fail err_msg

infixr 0 <?>

(<?>) :: HMonad r1 r2 st a -> String -> HMonad r1 r2 st a
p <?> err_msg = HMonad $ \sk fk ss -> getHMonad p sk (swapMsg fk) ss
  where
    swapMsg (Fail _)    = Fail err_msg 
    swapMsg other       = other


get :: HMonad r1 r2 st st
get = HMonad $ \sk fk r1 r2 st -> sk st fk r1 r2 st

gets :: (st -> a) -> HMonad r1 r2 st a
gets fn = HMonad $ \sk fk r1 r2 st -> sk (fn st) fk r1 r2 st

put :: st -> HMonad r1 r2 st ()
put s1 = HMonad $ \sk fk r1 r2 _ -> sk () fk r1 r2 s1

puts :: (st -> (a,st)) -> HMonad r1 r2 st a
puts fn = HMonad $ \sk fk r1 r2 st -> 
            let (a,s2) = fn st in sk a fk r1 r2 s2


puts_ :: (st -> st) -> HMonad r1 r2 st ()
puts_ fn = HMonad $ \sk fk r1 r2 st -> sk () fk r1 r2 (fn st)

ask1 :: HMonad r1 r2 st r1
ask1 = HMonad $ \sk fk r1 r2 st -> sk r1 fk r1 r2 st

ask2 :: HMonad r1 r2 st r2
ask2 = HMonad $ \sk fk r1 r2 st -> sk r2 fk r1 r2 st


asks1 :: (r1 -> a) -> HMonad r1 r2 st a
asks1 fn = HMonad $ \sk fk r1 r2 st -> sk (fn r1) fk r1 r2 st

asks2 :: (r2 -> a) -> HMonad r1 r2 st a
asks2 fn = HMonad $ \sk fk r1 r2 st -> sk (fn r2) fk r1 r2 st

local1 :: (r1 -> r1) -> HMonad r1 r2 st a -> HMonad r1 r2 st a
local1 f ma = HMonad $ \sk fk r1 r2 st -> 
                getHMonad ma sk fk (f r1) r2 st

local2 :: (r2 -> r2) -> HMonad r1 r2 st a -> HMonad r1 r2 st a
local2 f ma = HMonad $ \sk fk r1 r2 st -> 
                getHMonad ma sk fk r1 (f r2) st