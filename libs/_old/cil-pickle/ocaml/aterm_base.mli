(* aterm_base.mli *)


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



type aterm = 
    ATermAppl of string * aterm list
  | ATermList of aterm list
  | ATermInt of int
  

val dquote : string -> string

val aterm_empty : aterm

val to_aterm_string : string -> aterm

val	to_aterm_char : char -> aterm

val to_aterm_bool : bool -> aterm

val to_aterm_list : ('a -> aterm) -> 'a list -> aterm

val to_aterm_opt : ('a -> aterm) -> 'a option -> aterm

val to_aterm_tuple2 : ('a -> aterm) -> ('b -> aterm) -> 'a * 'b -> aterm

val to_aterm_tuple3 : ('a -> aterm) -> ('b -> aterm) -> ('c -> aterm) -> 'a * 'b * 'c -> aterm

val to_aterm_int : int -> aterm

val to_aterm_int64 : int64 -> aterm

val to_aterm_float : float -> aterm

val to_aterm_ref : ('a -> aterm) -> 'a ref -> aterm
  
  


