
module Compiler.CompilerMonad 
  ( CState        -- just the type not its constructor 
  , CompilerM
  , runCompilerM
  , further
  , furtherFail
  , ResultF
  , ResultFS
  , report
  , reportEllipsis
  , reportSep
  , postmortem
  , query
  , update
  , addTarget
  , askView
  )where

import Compiler.Env
import Compiler.Language
import Compiler.Views.ViewBase


import Control.Monad
import Control.Monad.Error
import Control.Monad.State

-- Compiler Monad has:
-- State (the environment is built in the initial phases and used in the later ones)
-- Errors
-- Logging (can be part of the state of course)



data CState = CState
  { env_st :: CompilerEnv
  , log_st :: [String]
  }

cstate0 = CState {env_st=default_env, log_st=[]}
  
type CompilerM a = ErrorT String (StateT CState IO) a  


type ResultF a = (Either String a)
-- result with failure and state
type ResultFS a = (Either String a, CState) 



runCompilerM :: CompilerM a -> IO (ResultFS a)
runCompilerM f = liftIO $ do 
  runStateT (runErrorT f) cstate0

-- further with a no-failure step
further :: (a -> CompilerM b) -> ResultF a -> CompilerM (ResultF b)
further f (Left err) = return (Left err)
further f (Right a) = do 
  ans <- f a
  return (Right ans)

-- further where the next step can fail
furtherFail :: (a -> CompilerM (ResultF b)) -> ResultF a -> CompilerM (ResultF b)
furtherFail f (Left err) = return (Left err)
furtherFail f (Right a) =  f a

  
    
    
report :: String -> CompilerM ()
report str = do
  xs <- gets log_st
  modify (\s -> s {log_st=(str:xs)} )

reportEllipsis :: String -> CompilerM ()
reportEllipsis str = report (str ++ "...")

reportSep :: CompilerM ()  
reportSep = report "--------------------------------------------------"


postmortem :: CState -> IO ()
postmortem (CState {env_st=e, log_st=ss}) 
  = case (silent_mode e) of
      False -> do { mapM_ putStrLn (reverse ss) 
                  ; putStrLn $ "postmortem ++ " ++ show e }
      True -> return ()

-- query aka reader monad's ask
query :: (CompilerEnv -> a) -> CompilerM a
query f = do
  env <- gets env_st
  return (f env)
  
update :: (CompilerEnv -> CompilerEnv) -> CompilerM ()
update f = do 
  env <- gets env_st
  modify (\s -> s{ env_st= (f env)}) 

 
addTarget :: Lang -> CompilerM ()
addTarget t = do
  ts <- query targets
  case (t `elem` ts) of 
    True -> return ()
    False -> update (\s -> s {targets=(t:ts)})
    
askView l k1 k2 = do
  vt <- query view_table
  return $ textEntry l k1 k2 vt    

  