

(* asdl_base.mli *)

open String


type identifier = string


exception Bad_constructor_index of int

val pinteger : int -> out_channel -> unit

val uinteger : in_channel -> int

val pstring : string -> out_channel -> unit 

val ustring : in_channel -> string

val ptag : int -> out_channel -> unit

val utag : in_channel -> int

val pidentifier : string -> out_channel -> unit

val uidentifier : in_channel -> string

val pmaybe : ('a -> out_channel -> unit) -> 'a option -> out_channel -> unit 

val umaybe : (in_channel -> 'a) -> in_channel -> 'a option 

val plist : ('a -> out_channel -> unit) -> 'a list -> out_channel -> unit

val ulist : (in_channel -> 'a) -> in_channel -> 'a list


