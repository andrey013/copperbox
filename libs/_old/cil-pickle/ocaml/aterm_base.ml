(* aterm_base.ml *)

(* Copyright (c) 2007 Stephen Peter Tetley *)

(* __MIT License__ *)
(* Permission is hereby granted, free of charge, to any person obtaining a    *)
(* copy of this software and associated documentation files (the "Software"), *)
(* to deal in the Software without restriction, including without limitation  *) 
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *) 
(* and/or sell copies of the Software, and to permit persons to whom the      *)
(* Software is furnished to do so, subject to the following conditions:       *)

(* The above copyright notice and this permission notice shall be included in *)
(* all copies or substantial portions of the Software.                        *)


(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *) 
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *)
(* DEALINGS IN THE SOFTWARE.                                                  *)


open Char
open Int64

type aterm = 
    ATermAppl of string * aterm list
  | ATermList of aterm list
  | ATermInt of int
  

let dquote (s : string) = "\"" ^ s ^ "\""

let aterm_empty = ATermAppl ("NRR", [])


let to_aterm_string (s : string) = ATermAppl (dquote s, [])  (* escaping? *)

(* maybe escaped isn't right *)
let to_aterm_char (c : char) =ATermAppl ((dquote (escaped c)), [])


let to_aterm_bool (b : bool) = 
  match b with
    true -> ATermAppl ("True", [])
  | false -> ATermAppl ("False", [])  
  
let to_aterm_list (fn : 'a -> aterm) (xs : 'a list) = ATermList (List.map fn xs) 

let to_aterm_opt (fn : 'a -> aterm) (ao : 'a option) = 
  match ao with
  	Some (x) -> ATermAppl ("Just", [fn x])
  | None -> ATermAppl ("Nothing", [])

let to_aterm_tuple2 (fn1 : 'a -> aterm) (fn2 : 'b -> aterm) (a,b : 'a * 'b) =
  ATermAppl ("Tuple2", [fn1 a; fn2 b])
	  
let to_aterm_tuple3 (fn1 : 'a -> aterm) (fn2 : 'b -> aterm) (fn3 : 'c -> aterm) (a,b,c : 'a * 'b * 'c) =
  ATermAppl ("Tuple3", [fn1 a; fn2 b; fn3 c])
  
   
let to_aterm_int (i : int) = ATermInt i 

let to_aterm_int64 (i : int64) = ATermAppl ("Int64", [to_aterm_string (to_string i)])

let to_aterm_float (f : float) = ATermAppl ("Float", [to_aterm_string (string_of_float f)])

let to_aterm_ref (fn : 'a -> aterm) (r : 'a ref) =
  let aval = !r in fn aval
