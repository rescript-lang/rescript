open Parsetree

val jsx_mapper :
  config:Jsx_common.jsx_config ->
  (Ast_mapper.mapper -> expression -> expression)
  * (Ast_mapper.mapper -> module_binding -> module_binding)
  * (signature_item -> signature_item list)
  * (structure_item -> structure_item list)
