(* asdl_base.mli *)

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




open String


type identifier = string


exception Bad_constructor_index of int

val pinteger : int -> out_channel -> unit

val uinteger : in_channel -> int

val pstring : string -> out_channel -> unit 

val ustring : in_channel -> string

val pfloat : float -> out_channel -> unit

val ufloat : in_channel -> float


val ptag : int -> out_channel -> unit

val utag : in_channel -> int

val pidentifier : string -> out_channel -> unit

val uidentifier : in_channel -> string

val pmaybe : ('a -> out_channel -> unit) -> 'a option -> out_channel -> unit 

val umaybe : (in_channel -> 'a) -> in_channel -> 'a option 

val plist : ('a -> out_channel -> unit) -> 'a list -> out_channel -> unit

val ulist : (in_channel -> 'a) -> in_channel -> 'a list


