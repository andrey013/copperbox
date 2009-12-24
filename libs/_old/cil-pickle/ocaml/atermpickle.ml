(* atermpickle.ml *)

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


open Cil
open Pretty
open Int64
open List
open Aterm_base


exception Global_exn of string


  
let rec to_aterm_cil_file (f : file) = 
  let a1 = to_aterm_string f.fileName
  and a2 = ATermList (List.map to_aterm_global f.globals) 
  and a3 = to_aterm_opt to_aterm_fundec f.globinit
  and a4 = to_aterm_bool f.globinitcalled in
  ATermAppl ("Tuple4", [a1;a2;a3;a4])    					 			



and to_aterm_global (g : global) = 
  match g with
    GType (x1,x2) -> let a1 = to_aterm_typeinfo x1
    				 and a2 = to_aterm_location x2 in
    				 ATermAppl ("GType", [a1;a2])
    				 
  | GCompTag (x1,x2) -> let a1 = to_aterm_compinfo x1
    				 	and a2 = to_aterm_location x2 in 
    				 	ATermAppl ("GCompTag", [a1;a2])
  | GCompTagDecl (x1,x2) -> let a1 = to_aterm_compinfo x1
    				 		and a2 = to_aterm_location x2 in    				 		
    				 		ATermAppl ("GCompTagDecl", [a1;a2])  
  | GEnumTag (x1,x2) -> let a1 = to_aterm_enuminfo x1
    				 	and a2 = to_aterm_location x2 in
    				 	ATermAppl ("GEnumTag", [a1;a2])
  | GEnumTagDecl (x1,x2) -> let a1 = to_aterm_enuminfo x1
    				 		and a2 = to_aterm_location x2 in
    				 		ATermAppl ("GEnumTagDecl", [a1;a2]) 
  | GVarDecl (x1,x2) -> let a1 = to_aterm_varinfo x1
    				 	and a2 = to_aterm_location x2 in
    				 	ATermAppl ("GVarDecl", [a1;a2]) 
  | GVar (x1,x2,x3) -> let a1 = to_aterm_varinfo x1
    				   and a2 = to_aterm_initinfo x2
    				   and a3 = to_aterm_location x3 in
    				   ATermAppl ("GVar", [a1;a2;a3]) 
  | GFun (x1,x2) -> let a1 = to_aterm_fundec x1
    				and a2 = to_aterm_location x2 in
    				ATermAppl ("GFun", [a1;a2]) 
  | GAsm (x1,x2) -> let a1 = to_aterm_string x1
    				and a2 = to_aterm_location x2 in
    				ATermAppl ("GAsm", [a1;a2])  
  | GPragma (x1,x2) -> let a1 = to_aterm_attribute x1
    				   and a2 = to_aterm_location x2 in
    				   ATermAppl ("GPragma", [a1;a2])    				 		  				 	    				 	
  | GText (x1) -> let a1 = to_aterm_string x1 in
    			  ATermAppl ("GText", [a1])  

    			      			  
and to_aterm_typ (t : typ) = 
  match t with
    TVoid (x1) -> let a1 = to_aterm_attributes x1 in
    			  ATermAppl ("TVoid", [a1])
  | TInt (x1,x2) -> let a1 = to_aterm_ikind x1 
  					and a2 = to_aterm_attributes x2 in
    			  	ATermAppl ("TInt", [a1;a2])    			  
  | TFloat (x1,x2) -> let a1 = to_aterm_fkind x1 
  					  and a2 = to_aterm_attributes x2 in
    			  	  ATermAppl ("TFloat", [a1;a2])
  | TPtr (x1,x2) -> let a1 = to_aterm_typ x1
  					and a2 = to_aterm_attributes x2 in
    			  	ATermAppl ("TPtr", [a1;a2])
  | TArray (x1,x2,x3) -> let a1 = to_aterm_typ x1
  						 and a2 = to_aterm_opt to_aterm_exp x2
  						 and a3 = to_aterm_attributes x3 in
    			  	 	 ATermAppl ("TArray", [a1;a2;a3])
  | TFun (x1,None,x3,x4) -> let a1 = to_aterm_typ x1
  						  and a2 = to_aterm_list (to_aterm_tuple3 to_aterm_string to_aterm_typ to_aterm_attributes) []
  						  and a3 = to_aterm_bool x3
  						  and a4 = to_aterm_attributes x4 in
  						  ATermAppl ("TFun", [a1;a2;a3;a4])
  | TFun (x1,Some x2,x3,x4) -> let a1 = to_aterm_typ x1
  						  and a2 = to_aterm_list (to_aterm_tuple3 to_aterm_string to_aterm_typ to_aterm_attributes) x2
  						  and a3 = to_aterm_bool x3
  						  and a4 = to_aterm_attributes x4 in
  						  ATermAppl ("TFun", [a1;a2;a3;a4])
  						    						  
  | TNamed (x1,x2) -> let a1 = to_aterm_typeinfo x1 
  					  and a2 = to_aterm_attributes x2 in
  					  ATermAppl ("TNamed", [a1;a2])
  | TComp (x1,x2) -> let a1 = to_aterm_compinfo x1 
  					 and a2 = to_aterm_attributes x2 in
  					 ATermAppl ("TComp", [a1;a2])
  | TEnum (x1,x2) -> let a1 = to_aterm_enuminfo x1
  					 and a2 = to_aterm_attributes x2 in
  					 ATermAppl ("TEnum", [a1;a2])
  | TBuiltin_va_list (x1) -> let a1 = to_aterm_attributes x1 in
  							 ATermAppl ("TBuiltin_va_list", [a1])
  			      					     					 
  
and to_aterm_ikind (i : ikind) =
  match i with
    IChar -> ATermAppl ("IChar", [])
  | ISChar -> ATermAppl ("ISChar", [])
  | IUChar -> ATermAppl ("IUChar", [])
  | IInt -> ATermAppl ("IInt", [])
  | IUInt -> ATermAppl ("IChar", [])
  | IShort -> ATermAppl ("IUInt", []) 
  | IUShort -> ATermAppl ("IChar", [])
  | ILong -> ATermAppl ("IUShort", []) 
  | IULong -> ATermAppl ("IChar", [])
  | ILongLong -> ATermAppl ("IULong", [])
  | IULongLong -> ATermAppl ("IULongLong", [])
  
and to_aterm_fkind (f : fkind) = 
  match f with
    FFloat -> ATermAppl ("FFloat", [])
  | FDouble -> ATermAppl ("FDouble", [])
  | FLongDouble -> ATermAppl ("FLongDouble", [])

and to_aterm_attribute (a : attribute) =
  match a with
    Attr (x1,x2) -> let a1 = to_aterm_string x1 
    				and a2 = to_aterm_list to_aterm_attrparam x2 in
    			  	ATermAppl ("Tuple2", [a1;a2])      
    			  	
    			  	
and to_aterm_attributes (xs : attributes) = to_aterm_list to_aterm_attribute xs 

and to_aterm_attrparam (a : attrparam) =
  match a with
  	AInt (x1) -> let a1 = to_aterm_int x1 in
    			       ATermAppl ("AInt", [a1]) 
  | AStr (x1) -> let a1 = to_aterm_string x1 in
    			       ATermAppl ("AStr", [a1])
  | ACons (x1,x2) -> let a1 = to_aterm_string x1 
  					 and a2 = to_aterm_list to_aterm_attrparam x2 in
  					 ATermAppl ("ACons", [a1;a2])
  | ASizeOf (x1) -> let a1 = to_aterm_typ x1 in
    			  	ATermAppl ("ASizeOf", [a1])
  | ASizeOfE (x1) -> let a1 = to_aterm_attrparam x1 in
    			 	 ATermAppl ("ASizeOfE", [a1])
  | ASizeOfS (x1) -> let a1 = to_aterm_typsig x1 in
    			 	 ATermAppl ("ASizeOfS", [a1])
  | AAlignOf (x1) -> let a1 = to_aterm_typ x1 in
    			 	 ATermAppl ("AAlignOf", [a1])
  | AAlignOfE (x1) -> let a1 = to_aterm_attrparam x1 in
    			 	  ATermAppl ("AAlignOfE", [a1])
  | AAlignOfS (x1) -> let a1 = to_aterm_typsig x1 in
    			 	  ATermAppl ("AAlignOfS", [a1])
  | AUnOp (x1,x2) -> let a1 = to_aterm_unop x1 
  					 and a2 = to_aterm_attrparam x2 in
  					 ATermAppl ("AUnOp", [a1;a2])
  | ABinOp (x1,x2,x3) -> let a1 = to_aterm_binop x1 
  					 	 and a2 = to_aterm_attrparam x2
  					 	 and a3 = to_aterm_attrparam x3 in
  					 	 ATermAppl ("ABinOp", [a1;a2;a3])
  | ADot (x1,x2) -> let a1 = to_aterm_attrparam x1 
  					and a2 = to_aterm_string x2 in
  					ATermAppl ("ADot", [a1;a2])

  					
and to_aterm_compinfo (c : compinfo) = 
  let a1 = to_aterm_bool c.cstruct
  and a2 = to_aterm_string c.cname
  and a3 = to_aterm_int c.ckey
  and a4 = to_aterm_list to_aterm_fieldinfo c.cfields
  and a5 = to_aterm_attributes c.cattr
  and a6 = to_aterm_bool c.cdefined
  and a7 = to_aterm_bool c.creferenced in
  ATermAppl ("Compinfo", [a1;a2;a3;a4;a5;a6;a7])

	
and to_aterm_fieldinfo (f : fieldinfo) = 
  let a2 = to_aterm_string f.fname    (* drop first field - it is a 'parent pointer' *)
  and a3 = to_aterm_typ f.ftype
  and a4 = to_aterm_opt to_aterm_int f.fbitfield
  and a5 = to_aterm_attributes f.fattr
  and a6 = to_aterm_location f.floc in
  ATermAppl ("Fieldinfo", [a2;a3;a4;a5;a6])
    					
and to_aterm_enuminfo (e : enuminfo) =
  let a1 = to_aterm_string e.ename
  and a2 = (to_aterm_list (to_aterm_tuple3 to_aterm_string to_aterm_exp to_aterm_location)) e.eitems (* eitems: (string * exp * location) list *)
  and a3 = to_aterm_attributes e.eattr
  and a4 = to_aterm_bool e.ereferenced in
  ATermAppl ("Enuminfo", [a1;a2;a3;a4])

  
  
and to_aterm_typeinfo (t : typeinfo) =
  let a1 = to_aterm_string t.tname
  and a2 = to_aterm_typ t.ttype
  and a3 = to_aterm_bool t.treferenced in
  ATermAppl ("TypeInfo", [a1;a2;a3])
    
and to_aterm_varinfo (v : varinfo) = 
  let a1 = to_aterm_string v.vname
  and a2 = to_aterm_typ v.vtype
  and a3 = to_aterm_attributes v.vattr
  and a4 = to_aterm_storage v.vstorage
  and a5 = to_aterm_bool v.vglob
  and a6 = to_aterm_bool v.vinline
  and a7 = to_aterm_location v.vdecl
  and a8 = to_aterm_int v.vid
  and a9 = to_aterm_bool v.vaddrof
  and a10 = to_aterm_bool v.vreferenced in
  ATermAppl ("VarInfo", [a1;a2;a3;a4;a5;a6;a7;a8;a9;a10])

  					
and to_aterm_storage (s : storage) =
  match s with									  
    NoStorage -> ATermAppl ("NoStorage", []) 
  | Static -> ATermAppl ("Static", [])
  | Register -> ATermAppl ("Register", [])
  | Extern -> ATermAppl ("Extern", []) 
  
and to_aterm_exp (e : exp) = 
  match e with
    Const (x1) -> let a1 = to_aterm_constant x1 in
    			  ATermAppl ("Const", [a1])
  | Lval (x1) -> let a1 = to_aterm_lval x1 in
    			 ATermAppl ("Lval", [a1])
  | SizeOf (x1) -> let a1 = to_aterm_typ x1 in
    			   ATermAppl ("SizeOf", [a1])
  | SizeOfE (x1) -> let a1 = to_aterm_exp x1 in
    			    ATermAppl ("SizeOfE", [a1])
  | SizeOfStr (x1) -> let a1 = to_aterm_string x1 in
    			      ATermAppl ("SizeOfStr", [a1])
  | AlignOf (x1) -> let a1 = to_aterm_typ x1 in
    			    ATermAppl ("AlignOf", [a1])
  | AlignOfE (x1) -> let a1 = to_aterm_exp x1 in
    			     ATermAppl ("AlignOfE", [a1])                       
  | UnOp (x1,x2,x3) -> let a1 = to_aterm_unop x1 
  					   and a2 = to_aterm_exp x2 
  					   and a3 = to_aterm_typ x3 in
  					   ATermAppl ("UnOp", [a1;a2;a3])
  | BinOp (x1,x2,x3,x4) -> let a1 = to_aterm_binop x1 
  					 	   and a2 = to_aterm_exp x2
  					 	   and a3 = to_aterm_exp x3
  					 	   and a4 = to_aterm_typ x4 in
  					 	   ATermAppl ("BinOp", [a1;a2;a3;a4])
  | CastE (x1,x2) -> let a1 = to_aterm_typ x1 
  					 and a2 = to_aterm_exp x2 in
  					 ATermAppl ("CastE", [a1;a2])
  | AddrOf (x1) -> let a1 = to_aterm_lval x1 in
    			   ATermAppl ("AddrOf", [a1])
  | StartOf (x1) -> let a1 = to_aterm_lval x1 in
    			    ATermAppl ("StartOf", [a1])
    
and to_aterm_constant (c : constant) = 
  match c with        			  	
    CInt64 (x1,x2,x3) -> let a1 = to_aterm_int64 x1 
  					     and a2 = to_aterm_ikind x2 
  					     and a3 = to_aterm_opt to_aterm_string x3 in
  					     ATermAppl ("CInt64", [a1;a2;a3])
  | CStr  (x1) -> let a1 = to_aterm_string x1 in
    			  ATermAppl ("CStr", [a1])
  | CWStr (x1) -> let a1 = to_aterm_list to_aterm_int64 x1 in
    			  ATermAppl ("CWStr", [a1])
  | CChr (x1) -> let a1 = to_aterm_char x1 in
    			 ATermAppl ("CChr", [a1])
  | CReal (x1,x2,x3) -> let a1 = to_aterm_float x1
  					    and a2 = to_aterm_fkind x2 
  					    and a3 = to_aterm_opt to_aterm_string x3 in
  					    ATermAppl ("CReal", [a1;a2;a3])
  | CEnum (x1,x2,x3) -> let a1 = to_aterm_exp x1
                        and a2 = to_aterm_string x2
                        and a3 = to_aterm_enuminfo x3 in
                        ATermAppl ("CEnum", [a1;a2;a3])
  
  
and to_aterm_unop (u : unop) =
  match u with
    Neg -> ATermAppl ("Neg", []) 
  | BNot -> ATermAppl ("BNot", [])
  | LNot -> ATermAppl ("LNot", [])
  
and to_aterm_binop (b : binop) =
  match b with
    PlusA -> ATermAppl ("PlusA", [])
  | PlusPI -> ATermAppl ("PlusPI", [])
  | IndexPI -> ATermAppl ("IndexPI", [])
  | MinusA -> ATermAppl ("MinusA", [])
  | MinusPI -> ATermAppl ("MinusPI", [])
  | MinusPP -> ATermAppl ("MinusPP", [])
  | Mult -> ATermAppl ("Mult", [])
  | Div -> ATermAppl ("Div", [])
  | Mod -> ATermAppl ("Mod", [])
  | Shiftlt -> ATermAppl ("Shiftlt", [])
  | Shiftrt -> ATermAppl ("Shiftrt", [])
  | Lt -> ATermAppl ("Lt", [])
  | Gt -> ATermAppl ("Gt", [])
  | Le -> ATermAppl ("Gt", [])
  | Ge -> ATermAppl ("Ge", [])
  | Eq -> ATermAppl ("Eq", [])
  | Ne -> ATermAppl ("Ne", [])
  | BAnd -> ATermAppl ("BAnd", [])
  | BXor -> ATermAppl ("BXor", [])
  | BOr -> ATermAppl ("BOr", [])
  | LAnd -> ATermAppl ("LAnd", [])
  | LOr -> ATermAppl ("LOr", [])

and to_aterm_lval (l : lval) =
  match l with
    (x1,x2) -> to_aterm_tuple2 to_aterm_lhost to_aterm_offset (x1,x2)

and to_aterm_lhost (l :lhost) = 
  match l with
    Var (x1) -> let a1 = to_aterm_varinfo x1 in
    			 ATermAppl ("Var", [a1])
  | Mem (x1) -> let a1 = to_aterm_exp x1 in
    			 ATermAppl ("Mem", [a1])    			     			     

and to_aterm_offset (o : offset) =
  match o with 
    NoOffset -> ATermAppl ("NoOffset", [])
  | Field (x1,x2) -> let a1 = to_aterm_fieldinfo x1
  			   		 and a2 = to_aterm_offset x2 in
  			   		 ATermAppl ("Field", [a1;a2])

  | Index (x1,x2) -> let a1 = to_aterm_exp x1
  			   		 and a2 = to_aterm_offset x2 in
  			   		 ATermAppl ("Index", [a1;a2])
    			     			 
and to_aterm_init (i : init) =
  match i with 
  | SingleInit (x1) -> let a1 = to_aterm_exp x1 in
    			 	   ATermAppl ("SingleInit", [a1]) 
  | CompoundInit (x1,x2) -> let a1 = to_aterm_typ x1
  			   		 		and a2 = to_aterm_list (to_aterm_tuple2 to_aterm_offset to_aterm_init) x2 in
  			   		 		ATermAppl ("CompoundInit", [a1;a2])
  			   		 
and to_aterm_initinfo (i : initinfo) = 
  let a1 = to_aterm_opt to_aterm_init i.init in
  ATermAppl ("Initinfo", [a1])
  			   		     			 
    			 
and to_aterm_fundec (f : fundec) =
  let a1 = to_aterm_varinfo f.svar 
  and a2 = to_aterm_list to_aterm_varinfo f.sformals
  and a3 = to_aterm_list to_aterm_varinfo f.slocals
  and a4 = to_aterm_int f.smaxid
  and a5 = to_aterm_block f.sbody
  and a6 = to_aterm_opt to_aterm_int f.smaxstmtid
  and a7 = to_aterm_list to_aterm_stmt f.sallstmts in
  ATermAppl ("FunDec", [a1;a2;a3;a4;a5;a6;a7])
			 
and to_aterm_block (b : block) =
  let a1 = to_aterm_attributes b.battrs
  and a2 = to_aterm_list to_aterm_stmt b.bstmts in
  ATermAppl ("Tuple2", [a1;a2])
  
and to_aterm_stmt (s : stmt) =
  let a1 = to_aterm_list to_aterm_label s.labels
  and a2 = to_aterm_stmtkind s.skind
  and a3 = to_aterm_int s.sid 
(*
  and a4 = ATermList [] -- don't print successors or predecessors [at least for now] 
  and a5 = ATermList [] 

*)
  in
  ATermAppl ("Stmt", [a1;a2;a3])
  
              
and to_aterm_label (l : label) =
  match l with
    Label (x1,x2,x3) -> let a1 = to_aterm_string x1
  			   		 	and a2 = to_aterm_location x2
  			   		 	and a3 = to_aterm_bool x3 in
  			   		 	ATermAppl ("Label", [a1;a2;a3])
  | Case (x1,x2) -> let a1 = to_aterm_exp x1
  			   		and a2 = to_aterm_location x2 in
  			   		ATermAppl ("Case", [a1;a2])
  | Default (x1) -> let a1 = to_aterm_location x1 in
  			   		ATermAppl ("Default", [a1])  
  
and to_aterm_stmtkind (s : stmtkind) =
  match s with 
    Instr (x1) -> let a1 = to_aterm_list to_aterm_instr x1 in
    			  ATermAppl ("Instr", [a1])
  | Return (x1,x2) -> let a1 = to_aterm_opt to_aterm_exp x1 
  					  and a2 = to_aterm_location x2 in
    			  	  ATermAppl ("Return", [a1;a2])
  | Goto (x1,x2) -> let a1 = to_aterm_ref to_aterm_stmt x1
  					and a2 = to_aterm_location x2 in
    			  	ATermAppl ("Goto", [a1;a2])
  | Break (x1) -> let a1 = to_aterm_location x1 in
    			  ATermAppl ("Break", [a1])
  | Continue (x1) -> let a1 = to_aterm_location x1 in
    			     ATermAppl ("Continue", [a1])
  | If (x1,x2,x3,x4) -> let a1 = to_aterm_exp x1 
  					    and a2 = to_aterm_block x2 
  					    and a3 = to_aterm_block x3
  					    and a4 = to_aterm_location x4 in
    			  	  	ATermAppl ("If", [a1;a2;a3;a4])
  | Switch (x1,x2,x3,x4) -> let a1 = to_aterm_exp x1
  					    	and a2 = to_aterm_block x2 
  					    	and a3 = to_aterm_list to_aterm_stmt x3
  					    	and a4 = to_aterm_location x4 in
    			  	  		ATermAppl ("Switch", [a1;a2;a3;a4])

  | Loop (x1,x2,x3,x4) -> let a1 = to_aterm_block x1
  					      and a2 = to_aterm_location x2 
  					      and a3 = to_aterm_opt to_aterm_stmt x3 
  					      and a4 = to_aterm_opt to_aterm_stmt x4 in
    			  	      ATermAppl ("Loop", [a1;a2;a3;a4])

  | Block (x1) -> let a1 = to_aterm_block x1 in
    			  ATermAppl ("Block", [a1])
  | TryFinally (x1,x2,x3) -> let a1 = to_aterm_block x1
  					      	 and a2 = to_aterm_block x2 
  					      	 and a3 = to_aterm_location x3 in
    			  	      	 ATermAppl ("TryFinally", [a1;a2;a3])    
  | TryExcept (x1,x2,x3,x4) -> let a1 = to_aterm_block x1
  					     	   and a2 = to_aterm_tuple2 (to_aterm_list to_aterm_instr) to_aterm_exp x2  
  					     	   and a3 = to_aterm_block x3 
  					     	   and a4 = to_aterm_location x4 in
  					     	   ATermAppl ("TryExcept", [a1;a2;a3;a4])
  
    
and to_aterm_instr (i : instr) =
  match i with
    Set (x1,x2,x3) -> let a1 = to_aterm_lval x1 
  					  and a2 = to_aterm_exp x2
  					  and a3 = to_aterm_location x3 in
  					  ATermAppl ("Set", [a1;a2;a3])
  | Call (x1,x2,x3,x4) -> let a1 = to_aterm_opt to_aterm_lval x1
  					      and a2 = to_aterm_exp x2
  					      and a3 = to_aterm_list to_aterm_exp x3
  					      and a4 = to_aterm_location x4 in
  					      ATermAppl ("Call", [a1;a2;a3;a4]) 
  | Asm (x1,x2,x3,x4,x5,x6) -> let a1 = to_aterm_attributes x1
  					      	   and a2 = to_aterm_list to_aterm_string x2
  					      	   and a3 = to_aterm_list (to_aterm_tuple2 to_aterm_string to_aterm_lval) x3  
  					      	   and a4 = to_aterm_list (to_aterm_tuple2 to_aterm_string to_aterm_exp) x4
  					      	   and a5 = to_aterm_list to_aterm_string x5 
  					      	   and a6 = to_aterm_location x6 in
  					      	   ATermAppl ("Call", [a1;a2;a3;a4;a5;a6]) 

            

and to_aterm_location (l : location) = 
  let a1 = to_aterm_int l.line
  and a2 = to_aterm_string l.file
  and a3 = to_aterm_int l.byte in
  ATermAppl ("Location", [a1;a2;a3])
  

and to_aterm_typsig (t : typsig) =
  match t with 
    TSArray (x1,x2,x3) -> let a1 = to_aterm_typsig x1 
  					  	  and a2 = to_aterm_opt to_aterm_int64 x2 
  					  	  and a3 = to_aterm_list to_aterm_attribute x3 in
  					  	  ATermAppl ("TSArray", [a1;a2;a3])
  | TSPtr (x1,x2) -> let a1 = to_aterm_typsig x1 
  					 and a2 = to_aterm_list to_aterm_attribute x2 in
  					 ATermAppl ("TSPtr", [a1;a2])
  | TSComp (x1,x2,x3) -> let a1 = to_aterm_bool x1 
  					  	 and a2 = to_aterm_string x2 
  					  	 and a3 = to_aterm_list to_aterm_attribute x3 in
  					  	 ATermAppl ("TSComp", [a1;a2;a3])
  | TSFun (x1,x2,x3,x4) -> let a1 = to_aterm_typsig x1
  					  	   and a2 = to_aterm_list to_aterm_typsig x2
  					  	   and a3 = to_aterm_bool x3
  					  	   and a4 = to_aterm_list to_aterm_attribute x4 in
  					  ATermAppl ("TSFun", [a1;a2;a3;a4])
  | TSEnum (x1,x2) -> let a1 = to_aterm_string x1 
  					  and a2 = to_aterm_list to_aterm_attribute x2 in
  					  ATermAppl ("TSEnum", [a1;a2])
  | TSBase (x1) -> let a1 = to_aterm_typ x1 in
  					  ATermAppl ("TSBase", [a1])
  
  
        			 
    			 
                                               
let rec aterm_to_doc (a : aterm) = 
  match a with
    ATermAppl (name,xs) -> text name ++ conv_list xs 
  | ATermList xs -> chr '[' ++ (docList aterm_to_doc () xs) ++ chr ']'
  | ATermInt i -> num i
  
and conv_list (xs : aterm list)  = 
  match xs with
    [] -> nil
  | ls -> chr '(' ++ (docList aterm_to_doc () ls) ++ chr ')'

(*
  List.fold_left (fun acc elt -> if acc <> Nil then acc ++ chr ',' ++ (aterm_to_aterm_doc elt)
  								 else (aterm_to_aterm_doc elt) 
  								 ) nil xs	 
*)  

  
let print_aterm (a : aterm) =  print_string (sprint 70 (aterm_to_doc a))


let output_aterm_file (f : file) =
  let outch = open_out_bin (f.fileName ^ ".taf")
  and doc = aterm_to_doc (to_aterm_cil_file f)
  in fprint outch 70 doc ;
     close_out outch



let feature : featureDescr = 
  { fd_name = "atermpickle";
    fd_enabled = ref false; (*   Cilutil.printAterm; *)
    fd_description = "prints the Cil ast in Aterm format";
    fd_extraopt = [];
    (* fd_doit = (function (f: file) -> print_aterm (to_aterm_cil_file f) ); *)
    fd_doit = (function (f: file) -> output_aterm_file f);
    fd_post_check = true;
  } 

