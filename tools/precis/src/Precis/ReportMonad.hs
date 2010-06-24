{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Precis.ReportMonad
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Logging monad for collecting report fragments
--
--------------------------------------------------------------------------------


module Precis.ReportMonad
  ( 
    -- * Report-monad
    ReportM
  , Log
  , CMP(..)
  , ChangeStats(..)

  , ModuleParseFunction
  , ReportLevel(..)

  , runReportM
  , execReportM

  , askParseFun
  , liftIO

  , tellHtml
  , tellParseFail

  , incrRemovedModules
  , incrRemovedExports 
  , incrChangedExports 
  , incrRemovedDatatypes
  , incrChangedDatatypes
  , incrRemovedTypeSigs
  , incrChangedTypeSigs
  , incrRemovedInstances
  , incrChangedInstances


  ) where


import Precis.Cabal.Datatypes
import Precis.HsSrc.Datatypes
import Precis.Utils.Common

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts
import Text.XHtml hiding ( name )               -- package: xhtml

import Control.Monad

-- Note - the monad has Writer operations but State implementation.
-- We don't particulary want append, snoc would be better, cons plus 
-- final reverse will do.

-- 

type Log = ([Html],ChangeStats)

-- Stats collects changes that should bump a /major version 
-- number/ as well as files that can't be parsed. 

data CMP a = NEW a | OLD a
  deriving (Eq,Show)

instance Functor CMP where
  fmap f (NEW a) = NEW (f a)
  fmap f (OLD a) = OLD (f a)

data ChangeStats = ChangeStats 
      { unparseable_modules     :: [CMP StrName]
      , removed_modules         :: Int

      -- exports from a module
      , removed_exports         :: Int
      , changed_exports         :: Int

      -- datatypes
      , removed_datatypes       :: Int
      , changed_datatypes       :: Int

      -- type signatures of functions / constants
      , removed_typesigs        :: Int
      , changed_typesigs        :: Int

      -- class instances
      , removed_instances       :: Int
      , changed_instances       :: Int
      }
  deriving (Show)

type ModuleParseFunction = SourceFile -> IO (Either ModuleParseError Module)

data ReportLevel = JUST_MSG | MSG_AND_HTML
  deriving (Eq,Show)

type Env = (ModuleParseFunction, ReportLevel)

-- Reader (for report level)  x State
--
newtype ReportM a = ReportM { getReportM :: Env -> Log -> IO (a,Log) }

instance Functor ReportM where
  fmap f (ReportM rf) = ReportM $ \e w ->  
                          rf e w `bindIO` \(a,w') -> returnIO (f a,w')

returnIO :: a -> IO a
returnIO = return

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO = (>>=)


instance Monad ReportM where
  return a  = ReportM $ \_ w -> returnIO (a,w)
  ma >>= mf = ReportM $ \e w -> (getReportM ma e w) `bindIO` \(a,w') ->
                                (getReportM . mf) a e w'



log_zero :: Log
log_zero = ([],stats_zero)
  where
    stats_zero = ChangeStats [] 0  0 0   0 0   0 0   0 0

runReportM :: ModuleParseFunction -> ReportLevel -> ReportM a -> IO (a,Log)
runReportM pf lvl mf =  (getReportM mf) (pf,lvl) log_zero `bindIO` post
  where
    post (a,(hs,stats)) = returnIO (a,(reverse hs,stats))

execReportM :: ModuleParseFunction -> ReportLevel -> ReportM a -> IO Log
execReportM pf lvl mf = liftM snd (runReportM pf lvl mf)

askParseFun :: ReportM ModuleParseFunction
askParseFun = ReportM $ \(pf,_) w -> returnIO (pf,w)

liftIO :: IO a -> ReportM a
liftIO mf = ReportM $ \_ w -> mf `bindIO` \a -> returnIO (a,w)


tellHtml :: Html -> ReportM ()
tellHtml h = ReportM $ \(_,lvl) (hs,stats) -> case lvl of 
               JUST_MSG     -> returnIO ((),(hs,stats))                     
               MSG_AND_HTML -> returnIO ((),(h:hs,stats))

updateStats :: (ChangeStats -> ChangeStats) -> ReportM ()
updateStats fn = ReportM $ \_ (hs,stats) -> returnIO ((),(hs, fn stats))


tellParseFail :: CMP StrName -> ReportM ()
tellParseFail name = updateStats $ 
    pstar (\xs s -> s { unparseable_modules = name:xs})  unparseable_modules

incrRemovedModules :: ReportM ()
incrRemovedModules = updateStats $
    pstar (\i s -> s { removed_modules = i+1}) removed_modules


incrRemovedExports :: ReportM ()
incrRemovedExports = updateStats $
    pstar (\i s -> s { removed_exports = i+1}) removed_exports

incrChangedExports :: ReportM ()
incrChangedExports = updateStats $
    pstar (\i s -> s { changed_exports = i+1}) changed_exports


incrRemovedDatatypes :: ReportM ()
incrRemovedDatatypes = updateStats $
    pstar (\i s -> s { removed_datatypes = i+1}) removed_datatypes

incrChangedDatatypes :: ReportM ()
incrChangedDatatypes = updateStats $
    pstar (\i s -> s { changed_datatypes = i+1}) changed_datatypes


incrRemovedTypeSigs :: ReportM ()
incrRemovedTypeSigs = updateStats $
    pstar (\i s -> s { removed_typesigs = i+1}) removed_typesigs 

incrChangedTypeSigs :: ReportM ()
incrChangedTypeSigs = updateStats $
    pstar (\i s -> s { changed_typesigs = i+1}) changed_typesigs

incrRemovedInstances :: ReportM ()
incrRemovedInstances = updateStats $
    pstar (\i s -> s { removed_instances = i+1}) removed_instances 

incrChangedInstances :: ReportM ()
incrChangedInstances = updateStats $
    pstar (\i s -> s { changed_instances = i+1}) changed_instances
