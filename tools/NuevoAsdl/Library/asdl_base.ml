(* asdl_base.ml *)


(* to load into the top level # #use "asdl_base.ml" ;; *)



open Char
open String
module L = List

type identifier = string

type uint8 = int

let negate a = 0 - a

(* chop off the bits of and int that aren't in the uint8 range *)
let down a = a land 0xff

(* is bit 7 set? - determines whether an integer 'continues' *)
let kbit a = (down a) land 0x80 == 0x80

(* is bit 6 set? - determines the signedness *)
let sbit a = (down a) land 0x40 == 0x40

(* set the continues bit *)
let kbit_on a = a lor 0x80

(* set the signedness bit *)
let sbit_on a = a lor 0x40


(* take the rightmost 7 bits; bits 0-6 *)
let right7 a = (down a) land 0x7f

(* take the rightmost 6 bits; bits 0-5  *)
let right6 a =  (down a) land 0x3f

(* forward -- helper for reading integers *)
let forward a acc shift = acc + (a lsl shift)


(* int_write : int x out_channel -> () *)
let rec int_write (i : int) (chan : out_channel) = 
    match i with
  | n when ((abs n) <= 63) -> if (i<0) then let ch = chr (sbit_on (abs n)) 
                                            in output_char chan ch
                              else (output_char chan (chr n))			
  | _ -> let n = kbit_on ((down i) land 0x7f)  
         in (output_char chan (chr n)) ; 
            int_write (i lsr 7) chan

(* text_write : string x out_channel -> () *) 
let text_write s chan = output_string chan s


(* try_char : in_channel -> option int *)
let try_byte chan = 
  try (let b = input_byte chan in Some b) with
  End_of_file -> None
    


(* integer_read : buffer -> int *)
let integer_read chan = 
  let rec fn n s = match (try_byte chan) with 
      None -> raise End_of_file
    | Some b  -> if (kbit b) 
                 then (fn (forward (right7 b) n s) (s+7))
                 else if (sbit b)
                      then (negate (forward (right6 b) n s))
                      else (forward (right6 b) n s)
  in fn 0 0 

let text_read i chan = 
  let buf = create i
  and pos = pos_in chan
  in match (input chan buf pos (pos+1)) with
      0 -> raise End_of_file
    | _ -> buf
    


                               
(* -- Now the picklers -- *)

exception Bad_constructor_index of int 


let pinteger = int_write

let uinteger = integer_read

let pstring s chan = 
  let i = length s 
  in pinteger i chan ;
     text_write s chan

let ustring chan = 
  let i = uinteger chan
  in text_read i chan
       
let ptag = pinteger
let utag = uinteger

let pidentifier = pstring
let uidentifier = ustring

let pmaybe f o chan = 
  match o with
    None -> pinteger 0 chan
  | Some a -> pinteger 1 chan ;
              f a chan
  
let umaybe f chan = let i = uinteger chan
  in match i with
        0 -> None 
      | 1 -> let a = f chan
             in Some (a) 
      | i -> raise (Bad_constructor_index i)

let plist f xs chan = 
  pinteger (L.length xs) chan ;
  L.iter (fun x -> f x chan) xs

let rec replicate i f =
  let rep_inner i xs = 
    match i with
        0 -> xs
      | n -> ((f) :: xs)
  in rep_inner i []  
                                      
let ulist f chan = let i = uinteger chan 
  in match i with
        0 -> [] 
      | x -> replicate x (f chan)
      


  
      
                  
                                                      
(* 

        



let maybeint = umaybe uinteger


let listint = ulist uinteger
*)


(* 

-- #use "asdl_base.ml" ;;

-- demo

let bs = create 80

int_write 1000 bs ;;

output_buffer stdout bs ;;

*)                  

(* 


        
*)        
                      
                                                 