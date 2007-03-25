(* Sun Jan 28 15:04:22 GMT Standard Time 2007 *)
type program = 
    Program of defs 
and defs = {
  x1           : def list} 
and def = 
    MLet of let_bindings
  | MLetrec of let_bindings 
and value_path = {
  module_path  : opt_identifier;
  value_name   : string} 
and value_name = 
    IdVal of string
  | OpVal of string 
and type_definition = 
    TypeDefinition of typedefs 
and typedefs = {
  x1           : typedef list} 
and typedef = {
  ty_params    : ty_params;
  tycon_name   : string;
  ty_info      : ty_info} 
and ty_params = {
  x1           : string list} 
and ty_info = {
  opt_ty_equation : opt_ty_equation;
  opt_ty_rep   : opt_ty_rep} 
and opt_ty_equation = {
  x1           : type_expr option} 
and opt_ty_rep = {
  x1           : ty_rep option} 
and ty_rep = 
    ConstrRep of constr_decls
  | RecordRep of field_decls 
and constr_decls = {
  x1           : constr_decl list} 
and constr_decl = {
  constr_name  : string;
  opt_type_expr : opt_type_expr} 
and field_decls = {
  x1           : field_decl list} 
and field_decl = {
  field_name   : string;
  ty_expr      : type_expr} 
and constr = {
  module_path  : opt_identifier;
  constr_name  : string} 
and field = {
  module_path  : opt_identifier;
  field_name   : string} 
and opt_identifier = {
  name         : string option} 
and type_expr = 
    TyExprVar of string
  | TyExprAnon
  | TyExprConstr of string
  | TyExprConstrApp of type_expr * type_expr
  | TyExprTuple of type_exprs 
and opt_type_expr = {
  x1           : type_expr option} 
and type_exprs = {
  x1           : type_expr list} 
and exprs = {
  x1           : expr list} 
and opt_expr = {
  x1           : expr option} 
and expr = 
    ValuePathExpr of value_path
  | ConstantExpr of constant
  | ParenExpr of expr
  | TupleExpr of exprs
  | ConstrApp of constr * expr
  | ListExpr of exprs
  | App of expr * arguments
  | FieldAccess of expr * field
  | IfExpr of expr * expr * opt_expr
  | Seq of expr * expr
  | Match of expr * pattern_matching
  | Let of let_bindings * expr
  | Letrec of let_bindings * expr 
and arguments = {
  x1           : argument list} 
and argument = 
    ArgExpr of expr 
and pattern_matching = {
  x1           : pattern_clause list} 
and pattern_clause = {
  pat          : pattern;
  opt_guard    : opt_expr;
  expr         : expr} 
and let_bindings = {
  x1           : let_binding list} 
and let_binding = 
    PatternBinding of pattern * expr
  | ValueBinding of string * parameters * opt_type_expr * expr 
and parameters = {
  x1           : parameter list} 
and parameter = 
    ParamPat of pattern 
and patterns = {
  x1           : pattern list} 
and pattern = 
    ValuePat of string
  | AnonPat
  | ConstantPat of constant
  | ParenPat of pattern * opt_type_expr
  | ConstrPat of constr * pattern
  | TuplePat of patterns 
and constant = 
    IntegerLiteral of int
  | FloatLiteral of float
  | StringLiteral of string
  | Val_True
  | Val_False
  | Unit
  | Nil 