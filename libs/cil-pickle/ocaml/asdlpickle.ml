(* asdlpickle.ml *)

open Int64
open Char
open Cil
open Asdl_base



let write_bool (x : bool) s =
  match x with
      true -> pinteger 1 s
    | false -> pinteger 0 s

let write_int64 i s = 
  ptag 1 s
  ; pstring (to_string i) s

let write_char c s =
  ptag 1 s
  ; pinteger (code c) s
  
let write_float d s = 
  ptag 1 s
  ; pfloat d s


(*  Hand corrected (NOT GENERATED) ........... *)

let rec write_cil_file x s =
  pidentifier x.fileName s
  ; write_globals x.globals s
  ; write_opt_fun_dec x.globinit s
  ; write_bool x.globinitcalled s

and write_comment x s =
  match x with
    (x1,x2) -> write_location x1 s; pstring x2 s

and write_globals x s = 
  (plist write_global) x s

and write_global x s = 
  match x with
      GType (x1,x2) -> ptag 1 s
                       ; write_typeinfo x1 s
                       ; write_location x2 s
    | GCompTag (x1,x2) -> ptag 2 s
                          ; write_compinfo x1 s
                          ; write_location x2 s
    | GCompTagDecl (x1,x2) -> ptag 3 s
                              ; write_compinfo x1 s
                              ; write_location x2 s
    | GEnumTag (x1,x2) -> ptag 4 s
                          ; write_enuminfo x1 s
                          ; write_location x2 s
    | GEnumTagDecl (x1,x2) -> ptag 5 s
                              ; write_enuminfo x1 s
                              ; write_location x2 s
    | GVarDecl (x1,x2) -> ptag 6 s; write_varinfo x1 s; write_location x2 s
    | GVar (x1,x2,x3) -> ptag 7 s
                         ; write_varinfo x1 s
                         ; write_initinfo x2 s
                         ; write_location x3 s
    | GFun (x1,x2) -> ptag 8 s; write_fun_dec x1 s; write_location x2 s
    | GAsm (x1,x2) -> ptag 9 s; pstring x1 s; write_location x2 s
    | GPragma (x1,x2) -> ptag 10 s
                         ; write_cil_attribute x1 s
                         ; write_location x2 s
    | GText (x1) -> ptag 11 s; pstring x1 s

and write_cil_type x s = 
  match x with
      TVoid (x1) -> ptag 1 s; write_cil_attributes x1 s
    | TInt (x1,x2) -> ptag 2 s
                      ; write_int_kind x1 s
                      ; write_cil_attributes x2 s
    | TFloat (x1,x2) -> ptag 3 s
                        ; write_float_kind x1 s
                        ; write_cil_attributes x2 s
    | TPtr (x1,x2) -> ptag 4 s
                      ; write_cil_type x1 s
                      ; write_cil_attributes x2 s
    | TArray (x1,x2,x3) -> ptag 5 s
                           ; write_cil_type x1 s
                           ; write_opt_exp x2 s
                           ; write_cil_attributes x3 s
    | TFun (x1,x2,x3,x4) -> ptag 6 s
                            ; write_cil_type x1 s
                            ; write_formal_args x2 s
                            ; write_bool x3 s
                            ; write_cil_attributes x4 s
    | TNamed (x1,x2) -> ptag 7 s
                        ; write_typeinfo x1 s
                        ; write_cil_attributes x2 s
    | TComp (x1,x2) -> ptag 8 s
                       ; write_compinfo x1 s
                       ; write_cil_attributes x2 s
    | TEnum (x1,x2) -> ptag 9 s
                       ; write_enuminfo x1 s
                       ; write_cil_attributes x2 s
    | TBuiltin_va_list (x1) -> ptag 10 s; write_cil_attributes x1 s

(* hand coded *)
and write_formal_args x s =
  match x with 
      None -> (plist write_formal_arg) [] s
    | Some (xs) -> (plist write_formal_arg) xs s

and write_formal_arg x s =
  match x with
    (x1,x2,x3) -> pstring x1 s; write_cil_type x2 s; write_cil_attributes x3 s

and write_int_kind x s =
  match x with
      IChar  -> ptag 1 s
    | ISChar  -> ptag 2 s
    | IUChar  -> ptag 3 s
    | IInt  -> ptag 4 s
    | IUInt  -> ptag 5 s
    | IShort  -> ptag 6 s
    | IUShort  -> ptag 7 s
    | ILong  -> ptag 8 s
    | IULong  -> ptag 9 s
    | ILongLong  -> ptag 10 s
    | IULongLong  -> ptag 11 s

and write_float_kind x s = 
  match x with
      FFloat  -> ptag 1 s
    | FDouble  -> ptag 2 s
    | FLongDouble  -> ptag 3 s

and write_cil_attributes x s = 
  (plist write_cil_attribute) x s

and write_cil_attribute x s =
  match x with
    Attr (x1,x2) -> pstring x1 s; write_attr_params x2 s

and write_attr_params x s = 
  (plist write_attr_param) x s

and write_attr_param x s = 
  match x with
      AInt (x1) -> ptag 1 s; pinteger x1 s
    | AStr (x1) -> ptag 2 s; pstring x1 s
    | ACons (x1,x2) -> ptag 3 s; pstring x1 s; write_attr_params x2 s
    | ASizeOf (x1) -> ptag 4 s; write_cil_type x1 s
    | ASizeOfE (x1) -> ptag 5 s; write_attr_param x1 s
    | ASizeOfS (x1) -> ptag 6 s; write_type_sig x1 s
    | AAlignOf (x1) -> ptag 7 s; write_cil_type x1 s
    | AAlignOfE (x1) -> ptag 8 s; write_attr_param x1 s
    | AAlignOfS (x1) -> ptag 9 s; write_type_sig x1 s
    | AUnOp (x1,x2) -> ptag 10 s; write_unOp x1 s; write_attr_param x2 s
    | ABinOp (x1,x2,x3) -> ptag 11 s
                           ; write_binOp x1 s
                           ; write_attr_param x2 s
                           ; write_attr_param x3 s
    | ADot (x1,x2) -> ptag 12 s; write_attr_param x1 s; pstring x2 s

and write_compinfo x s = 
  ptag 1 s
  ; write_bool x.cstruct s
  ; pstring x.cname s
  ; pinteger x.ckey s
  ; write_fieldinfos x.cfields s
  ; write_cil_attributes x.cattr s
  ; write_bool x.cdefined s
  ; write_bool x.creferenced s

and write_fieldinfos x s = 
  (plist write_fieldinfo) x s

and write_fieldinfo x s = 
  ptag 1 s
  (* missing fcomp -- stop looping *)
  ; pstring x.fname s
  ; write_cil_type x.ftype s
  ; (pmaybe pinteger) x.fbitfield s
  ; write_cil_attributes x.fattr s
  ; write_location x.floc s

and write_enuminfo x s = 
  ptag 1 s
  ; pstring x.ename s
  ; write_items x.eitems s
  ; write_cil_attributes x.eattr s
  ; write_bool x.ereferenced s

and write_typeinfo x s = 
  ptag 1 s
  ; pstring x.tname s
  ; write_cil_type x.ttype s
  ; write_bool x.treferenced s

and write_varinfos x s = 
  (plist write_varinfo) x s

and write_opt_varinfo x s = 
  (pmaybe write_varinfo) x s

and write_varinfo x s = 
  ptag 1 s
  ; pstring x.vname s
  ; write_cil_type x.vtype s
  ; write_cil_attributes x.vattr s
  ; write_storage x.vstorage s
  ; write_bool x.vglob s
  ; write_bool x.vinline s
  ; write_location x.vdecl s
  ; pinteger x.vid s
  ; write_bool x.vaddrof s
  ; write_bool x.vreferenced s

and write_items x s = 
  (plist write_item) x s

and write_item x s =
  match x with
    (x1,x2,x3) -> pstring x1 s; write_exp x2 s; write_location x3 s

and write_storage x s = 
  match x with
      NoStorage  -> ptag 1 s
    | Static  -> ptag 2 s
    | Register  -> ptag 3 s
    | Extern  -> ptag 4 s

and write_opt_exp x s = 
  (pmaybe write_exp) x s

and write_exps x s = 
  (plist write_exp) x s

and write_exp x s = 
  match x with
      Const (x1) -> ptag 1 s; write_constant x1 s
    | Lval (x1) -> ptag 2 s; write_lval x1 s
    | SizeOf (x1) -> ptag 3 s; write_cil_type x1 s
    | SizeOfE (x1) -> ptag 4 s; write_exp x1 s
    | SizeOfStr (x1) -> ptag 5 s; pstring x1 s
    | AlignOf (x1) -> ptag 6 s; write_cil_type x1 s
    | AlignOfE (x1) -> ptag 7 s; write_exp x1 s
    | UnOp (x1,x2,x3) -> ptag 8 s
                         ; write_unOp x1 s
                         ; write_exp x2 s
                         ; write_cil_type x3 s
    | BinOp (x1,x2,x3,x4) -> ptag 9 s
                             ; write_binOp x1 s
                             ; write_exp x2 s
                             ; write_exp x3 s
                             ; write_cil_type x4 s
    | CastE (x1,x2) -> ptag 10 s; write_cil_type x1 s; write_exp x2 s
    | AddrOf (x1) -> ptag 11 s; write_lval x1 s
    | StartOf (x1) -> ptag 12 s; write_lval x1 s

and write_constant x s = 
  match x with
      CInt64 (x1,x2,x3) -> ptag 1 s
                           ; write_int64 x1 s
                           ; write_int_kind x2 s
                           ; (pmaybe pstring) x3 s
    | CStr (x1) -> ptag 2 s; pstring x1 s
    | CWStr (x1) -> ptag 3 s; (plist write_int64) x1 s
    | CChr (x1) -> ptag 4 s; write_char x1 s
    | CReal (x1,x2,x3) -> ptag 5 s
                          ; write_float x1 s
                          ; write_float_kind x2 s
                          ; (pmaybe pstring) x3 s
    | CEnum (x1,x2,x3) -> ptag 6 s
                          ; write_exp x1 s
                          ; pstring x2 s
                          ; write_enuminfo x3 s

and write_unOp x s = 
  match x with
      Neg  -> ptag 1 s| BNot  -> ptag 2 s| LNot  -> ptag 3 s

and write_binOp x s = 
  match x with
      PlusA  -> ptag 1 s
    | PlusPI  -> ptag 2 s
    | IndexPI  -> ptag 3 s
    | MinusA  -> ptag 4 s
    | MinusPI  -> ptag 5 s
    | MinusPP  -> ptag 6 s
    | Mult  -> ptag 7 s
    | Div  -> ptag 8 s
    | Mod  -> ptag 9 s
    | Shiftlt  -> ptag 10 s
    | Shiftrt  -> ptag 11 s
    | Lt  -> ptag 12 s
    | Gt  -> ptag 13 s
    | Le  -> ptag 14 s
    | Ge  -> ptag 15 s
    | Eq  -> ptag 16 s
    | Ne  -> ptag 17 s
    | BAnd  -> ptag 18 s
    | BXor  -> ptag 19 s
    | BOr  -> ptag 20 s
    | LAnd  -> ptag 21 s
    | LOr  -> ptag 22 s

and write_opt_lval x s = 
  (pmaybe write_lval) x s

and write_lval x s =
  match x with
    (x1,x2) -> write_lhost x1 s; write_offset x2 s

and write_lhost x s = 
  match x with
      Var (x1) -> ptag 1 s; write_varinfo x1 s
    | Mem (x1) -> ptag 2 s; write_exp x1 s

and write_offset x s = 
  match x with
      NoOffset  -> ptag 1 s
    | Field (x1,x2) -> ptag 2 s; write_fieldinfo x1 s; write_offset x2 s
    | Index (x1,x2) -> ptag 3 s; write_exp x1 s; write_offset x2 s

and write_opt_init x s = 
  (pmaybe write_init) x s

and write_init x s = 
  match x with
      SingleInit (x1) -> ptag 1 s; write_exp x1 s
    | CompoundInit (x1,x2) -> ptag 2 s
                              ; write_cil_type x1 s
                              ; write_elt_initializers x2 s
(* significant change *)
and write_initinfo x s =
  ptag 1 s
  ; write_opt_init x.init s

and write_elt_initializers x s = 
  (plist write_elt_initializer) x s

and write_elt_initializer x s =
  match x with
    (x1,x2) -> write_offset x1 s; write_init x2 s

and write_opt_fun_dec x s =
  (pmaybe write_fun_dec) x s

and write_fun_dec x s =
  ptag 1 s
  ; write_varinfo x.svar s
  ; write_varinfos x.sformals s
  ; write_varinfos x.slocals s
  ; pinteger x.smaxid s
  ; write_block x.sbody s
  ; (pmaybe pinteger) x.smaxstmtid s
  ; write_stmts x.sallstmts s

and write_block x s = 
  write_cil_attributes x.battrs s; write_stmts x.bstmts s

and write_stmts x s = 
  (plist write_stmt) x s

and write_opt_stmt x s = 
  (pmaybe write_stmt) x s

(* looping - succs & preds missing *)
and write_stmt x s =
  ptag 1 s
  ; write_labels x.labels s
  ; write_stmt_kind x.skind s
  ; pinteger x.sid s


and write_labels x s = 
  (plist write_label) x s

and write_label x s = 
  match x with
      Label (x1,x2,x3) -> ptag 1 s
                          ; pstring x1 s
                          ; write_location x2 s
                          ; write_bool x3 s
    | Case (x1,x2) -> ptag 2 s; write_exp x1 s; write_location x2 s
    | Default (x1) -> ptag 3 s; write_location x1 s

(* deref on Goto *)
and write_stmt_kind x s = 
  match x with
      Instr (x1) -> ptag 1 s; write_instrs x1 s
    | Return (x1,x2) -> ptag 2 s; write_opt_exp x1 s; write_location x2 s
    | Goto (x1,x2) -> ptag 3 s; write_stmt !x1 s; write_location x2 s
    | Break (x1) -> ptag 4 s; write_location x1 s
    | Continue (x1) -> ptag 5 s; write_location x1 s
    | If (x1,x2,x3,x4) -> ptag 6 s
                          ; write_exp x1 s
                          ; write_block x2 s
                          ; write_block x3 s
                          ; write_location x4 s
    | Switch (x1,x2,x3,x4) -> ptag 7 s
                              ; write_exp x1 s
                              ; write_block x2 s
                              ; write_stmts x3 s
                              ; write_location x4 s
    | Loop (x1,x2,x3,x4) -> ptag 8 s
                            ; write_block x1 s
                            ; write_location x2 s
                            ; write_opt_stmt x3 s
                            ; write_opt_stmt x4 s
    | Block (x1) -> ptag 9 s; write_block x1 s
    | TryFinally (x1,x2,x3) -> ptag 10 s
                               ; write_block x1 s
                               ; write_block x2 s
                               ; write_location x3 s
    | TryExcept (x1,x2,x3,x4) -> ptag 11 s
                                 ; write_block x1 s
                                 ; write_except_exp x2 s
                                 ; write_block x3 s
                                 ; write_location x4 s

and write_except_exp x s =
  match x with
    (x1,x2) -> write_instrs x1 s; write_exp x2 s

and write_instrs x s = 
  (plist write_instr) x s

and write_instr x s = 
  match x with
      Set (x1,x2,x3) -> ptag 1 s
                        ; write_lval x1 s
                        ; write_exp x2 s
                        ; write_location x3 s
    | Call (x1,x2,x3,x4) -> ptag 2 s
                            ; write_opt_lval x1 s
                            ; write_exp x2 s
                            ; write_exps x3 s
                            ; write_location x4 s
    | Asm (x1,x2,x3,x4,x5,x6) -> ptag 3 s
                                 ; write_cil_attributes x1 s
                                 ; (plist pstring) x2 s
                                 ; write_asm_outputs x3 s
                                 ; write_asm_inputs x4 s
                                 ; (plist pstring) x5 s
                                 ; write_location x6 s

and write_asm_outputs x s = 
  (plist write_asm_output) x s

and write_asm_output x s =
  match x with
    (x1,x2) -> pstring x1 s; write_lval x2 s

and write_asm_inputs x s =
  (plist write_asm_input) x s

and write_asm_input x s =
  match x with
    (x1,x2) -> pstring x1 s; write_exp x2 s

and write_location x s = 
  ptag 1 s
  ; pinteger x.line s
  ; pstring x.file s
  ; pinteger x.byte s

and write_type_sigs x s = 
  (plist write_type_sig) x s

and write_type_sig x s =
  match x with
      TSArray (x1,x2,x3) -> ptag 1 s
                            ; write_type_sig x1 s
                            ; (pmaybe write_int64) x2 s
                            ; write_cil_attributes x3 s
    | TSPtr (x1,x2) -> ptag 2 s
                       ; write_type_sig x1 s
                       ; write_cil_attributes x2 s
    | TSComp (x1,x2,x3) -> ptag 3 s
                           ; write_bool x1 s
                           ; pstring x2 s
                           ; write_cil_attributes x3 s
    | TSFun (x1,x2,x3,x4) -> ptag 4 s
                             ; write_type_sig x1 s
                             ; write_type_sigs x2 s
                             ; write_bool x3 s
                             ; write_cil_attributes x4 s
    | TSEnum (x1,x2) -> ptag 5 s; pstring x1 s; write_cil_attributes x2 s
    | TSBase (x1) -> ptag 6 s; write_cil_type x1 s



(* PICKLERS END ....... *)

let pickle_cil_file (f : file) =
  let outch = open_out_bin (f.fileName ^ ".asdlpkl")
  in write_cil_file f outch ;
     close_out outch

let feature : featureDescr =
  { fd_name = "asdlpickle";
    fd_enabled = ref false; (*   Cilutil.printAterm; *)
    fd_description = "writes an Asdl pickle of the Cil ast";
    fd_extraopt = [];
    fd_doit = (function (f: file) -> pickle_cil_file f );
    fd_post_check = true;
  }