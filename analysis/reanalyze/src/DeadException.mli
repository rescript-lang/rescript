open DeadCommon

val find_exception_from_decls : Declarations.t -> DcePath.t -> Location.t option

val add :
  config:DceConfig.t ->
  decls:Declarations.builder ->
  file:FileContext.t ->
  path:DcePath.t ->
  loc:Location.t ->
  str_loc:Location.t ->
  module_loc:Location.t ->
  Name.t ->
  Name.t

val mark_as_used :
  config:DceConfig.t ->
  refs:References.builder ->
  file_deps:FileDeps.builder ->
  cross_file:CrossFileItems.builder ->
  binding:Location.t ->
  loc_from:Location.t ->
  loc_to:Location.t ->
  Path.t ->
  unit
