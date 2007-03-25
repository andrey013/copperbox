(* Sun Jan 28 15:04:22 GMT Standard Time 2007 *)
type asdl_spec = 
    AsdlSpec of decls 
and decls = {
  x1           : decl list} 
and decl = 
    Module of string * definitions 
and definitions = {
  x1           : definition list} 
and definition = 
    Def of string * asdl_type 
and asdl_type = 
    Sum of constrs
  | Prod of fields 
and constrs = {
  x1           : constr list} 
and constr = 
    Constr of string * fields 
and fields = {
  x1           : field list} 
and field = 
    Field of opt_qualifier * asdl_prim * cardinality * opt_identifier 
and opt_qualifier = {
  x1           : string option} 
and opt_identifier = {
  x1           : string option} 
and asdl_prim = 
    TyUnit
  | TyString
  | TyInt
  | TyIdentifier
  | TyRef of string
  | TyExternalPrim of string 
and cardinality = 
    One
  | Zom
  | Opt 