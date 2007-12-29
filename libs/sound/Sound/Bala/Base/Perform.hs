
module Sound.Bala.Base.Perform where

output :: [evt] -> env -> Perform evt env out -> IO ()
output xs env (Perform {render=r,perform=p}) 
  = let o = r xs env in p o env

data Perform evt env out = Perform {
  render :: [evt] -> env -> out,
  perform :: out  -> env -> IO ()
  }  
  
{-
class Render evt env out where 
  render :: [evt] -> env -> out
  
class Perform out where 
  perform :: out -> IO ()
-}