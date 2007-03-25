(* Sun Jan 28 15:04:22 GMT Standard Time 2007 *)
type asdl_spec = {
  x1           : alt_three list} 
and alt_three = 
    Alt1 of module_defn
  | Alt2 of prim_module
  | Alt3 of view_defn 
and module_defns = {
  x1           : module_defn list} 
and module_defn = 
    Module of string * import_stmts * definitions 
and prim_module = 
    PrimModule of string * string list 
and view_defn = 
    View of string * view_decls 
and import_stmts = {
  x1           : import_stmt list} 
and import_stmt = 
    ImportStmt of string 
and view_decls = {
  x1           : view_decl list} 
and view_decl = 
    View_Plain of view_entity * view_pair
  | View_Many_to_One of view_entity list * view_pair
  | View_One_to_Many of view_entity * view_pair list
  | View_Many_to_Many of view_entity list * view_pair list 
and view_entity = 
    TypeView of string * string
  | ConstrView of string * string
  | ModuleView of string 
and view_pair = {
  prop         : property;
  value        : text_value} 
and definitions = {
  x1           : definition list} 
and definition = 
    Def of string * asdl_type 
and asdl_type = 
    Sum of constrs * opt_attribs
  | Prod of fields 
and constrs = {
  x1           : constr list} 
and constr = 
    Constr of string * fields 
and opt_attribs = {
  x1           : attribs option} 
and attribs = {
  x1           : field list} 
and fields = {
  x1           : field list} 
and field = 
    Field of opt_qualifier * asdl_prim * cardinality * opt_identifier 
and opt_identifier = {
  x1           : string option} 
and opt_qualifier = {
  x1           : string option} 
and asdl_prim = 
    TyUnit
  | TyString
  | TyInt
  | TyIdentifier
  | TyRef of string 
and cardinality = 
    One
  | Zom
  | Opt 