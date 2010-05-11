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
  , ModuleParseFunction
  , ReportLevel(..)

  , runReportM
  , execReportM

  , tellHtml
  , tellMsg
  , liftIO
  , askParseFun

  ) where

import Precis.Datatypes

import Language.Haskell.Exts ( Module )         -- package: haskell-src-exts
import Text.XHtml               -- package: xhtml

import Control.Monad

-- Note - the monad has Writer operations but State implementation.
-- We don't particulary want append, snoc would be better, cons plus 
-- final reverse will do.

-- 

type Log = ([String],[Html])

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



runReportM :: ModuleParseFunction -> ReportLevel -> ReportM a -> IO (a,Log)
runReportM pf lvl mf =  (getReportM mf) (pf,lvl) ([],[]) `bindIO` post
  where
    post (a,(xs,ys)) = returnIO (a,(reverse xs, reverse ys))

execReportM :: ModuleParseFunction -> ReportLevel -> ReportM a -> IO Log
execReportM pf lvl mf = liftM snd (runReportM pf lvl mf)

askParseFun :: ReportM ModuleParseFunction
askParseFun = ReportM $ \(pf,_) w -> returnIO (pf,w)

tellHtml :: Html -> ReportM ()
tellHtml h = ReportM $ \(_,lvl) (ss,hs) -> case lvl of 
               JUST_MSG     -> returnIO ((),(ss,hs))                     
               MSG_AND_HTML -> returnIO ((),(ss,h:hs))

tellMsg :: String -> ReportM ()
tellMsg s = ReportM $ \_ (ss,hs) -> returnIO ((),(s:ss,hs))

liftIO :: IO a -> ReportM a
liftIO mf = ReportM $ \_ w -> mf `bindIO` \a -> returnIO (a,w)
