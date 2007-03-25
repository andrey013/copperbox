

open Asdl_base



type sexpr = Int of int
          | String of string
          | Symbol of identifier
          | Cons of sexpr * sexpr
          | Nil




let rec write_sexpr x s =
  match x with
      Int (x1) -> ptag 1 s ; pinteger x1 s
    | String (x1) -> ptag 2 s ; pstring x1 s
    | Symbol (x1) -> ptag 3 s ; pidentifier x1 s
    | Cons (s1,s2) -> ptag 4 s ; write_sexpr s1 s ; write_sexpr s2 s
    | Nil -> ptag 5 s


let rec read_sexpr s = 
  let i = uinteger s
  in match i with
      1 -> let x1 = uinteger s 
           in Int (x1)
    | 2 -> let x1 = ustring s 
           in String (x1)
    | 3 -> let x1 = uidentifier s 
           in Symbol (x1)                        
    | 4 -> let x1 = read_sexpr s
           and x2 = read_sexpr s 
           in Cons (x1,x2)
    | 5 -> Nil
    | x -> raise (Bad_constructor_index x) 


let sexp1 = Cons (Int 1, (Cons (Int 9, Symbol "hello")))
  
let demo = 
  let outch = open_out_bin "sexp.asdlpickle"
  in write_sexpr sexp1 outch ;
     close_out outch
     
      
   
(* 
-- #use "sexp.ml" ;;

-- for each change of asdl_base.ml the following will be necessary:

-- with a cygwin shell, compile asdl_base
$ ocamlc -c asdl_base.mli

$ ocamlc -c asdl_base.ml


-- #cd "D:/coding/haskell/GHC_Workspace/Asdl/src/Library" ;;
-- #load "asdl_base.cmo" ;;
-- #use "sexp.ml" ;;

-- if you get: "The files X and Y disagree over interface"
-- you should restart the ocaml top level


*)

