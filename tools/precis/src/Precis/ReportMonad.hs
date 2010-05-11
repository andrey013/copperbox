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
  , ReportLevel(..)

  , runReportM
  , execReportM

  , tellHtml
  , tellMsg
  , liftIO

  ) where

import Text.XHtml               -- package: xhtml

import Control.Monad

-- Note - the monad has Writer operations but State implementation.
-- We don't particulary want append, snoc would be better, cons plus 
-- final reverse will do.

-- 

type Log = ([String],[Html])

data ReportLevel = JUST_MSG | MSG_AND_HTML
  deriving (Eq,Show)

-- Reader (for report level)  x State
--
newtype ReportM a = ReportM { getReportM :: ReportLevel -> Log -> IO (a,Log) }

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



runReportM :: ReportLevel -> ReportM a -> IO (a,Log)
runReportM lvl mf =  (getReportM mf) lvl ([],[]) `bindIO` post
  where
    post (a,(xs,ys)) = returnIO (a,(reverse xs, reverse ys))

execReportM :: ReportLevel -> ReportM a -> IO Log
execReportM lvl mf = liftM snd (runReportM lvl mf)

tellHtml :: Html -> ReportM ()
tellHtml h = ReportM $ \e (ss,hs) -> case e of 
               JUST_MSG     -> returnIO ((),(ss,hs))                     
               MSG_AND_HTML -> returnIO ((),(ss,h:hs))

tellMsg :: String -> ReportM ()
tellMsg s = ReportM $ \_ (ss,hs) -> returnIO ((),(s:ss,hs))

liftIO :: IO a -> ReportM a
liftIO mf = ReportM $ \_ w -> mf `bindIO` \a -> returnIO (a,w)
