(* Sun Jan 28 15:04:22 GMT Standard Time 2007 *)
type ag = {
  x1           : elem list} 
and elem = 
    Data of conid * data_alts
  | Type of conid * type_body 
and type_body = 
    TB_List of typ
  | TB_Maybe of typ
  | TB_Tuple of fields 
and typ = 
    NamedType of conid
  | CodeBlock of string 
and data_alts = {
  x1           : data_alt list} 
and data_alt = 
    DataAlt of conid * fields 
and fields = {
  x1           : field list} 
and field = 
    LabelledField of varid * typ
  | ConstrField of conid 