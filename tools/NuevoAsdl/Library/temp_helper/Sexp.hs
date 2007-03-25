
module SexprPkl 
  (write_sexpr
  , read_sexpr
  ) where


import AsdlBase
import Sexpr


val1 = Cons Nil Nil



  

write_sexpr1 :: Sexpr -> CM ()
write_sexpr1 (Int i) = do
  write_tag 1
  pickle i
             
write_sexpr1 (String string1) = do
  write_tag 2
  pickle string1
             
write_sexpr1 (Symbol identifier1) = do
  write_tag 3
  pickle identifier1
             
write_sexpr1 (Cons sexpr1 sexpr2) = do
  write_tag 4
  write_sexpr1 sexpr1
  write_sexpr1 sexpr2
             
write_sexpr1 (Nil) = do
  write_tag 5

            
write_tagged_sexpr1 x = do
  write_tag 4
  write_sexpr1 x

     
read_sexpr1 = do 
  i <- read_tag
  read_sexpr1' i
  
  where
    read_sexpr1' 1 = do
      int1 <- unpickle
      return (Int int1)

    read_sexpr1' 2 = do 
      string1 <- unpickle
      return (String string1)
                    
    read_sexpr1' 3 = do 
      identifier1 <- unpickle
      return (Symbol identifier1)
                    
    read_sexpr1' 4 = do 
      sexpr1 <- read_sexpr1
      sexpr2 <- read_sexpr1
      return (Cons sexpr1 sexpr2)
                    
    read_sexpr1' 5 = do 
      return (Nil)
                
    read_sexpr1' i = error $ "read_sexpr1' " ++ show i
              
demo = readPickle "sexp.asdlpickle" read_sexpr1

demo2 = do 
  a <- readPickle "sexp.asdlpickle" read_sexpr1
  writePickle "sexp2.asdlpickle" write_sexpr1 a
  b <- readPickle "sexp2.asdlpickle" read_sexpr1
  return b
  


  
write_sexpr = write_sexpr1

instance Pickle Sexpr where
  pickle = write_sexpr1
  
  unpickle = read_sexpr1  
    

  
