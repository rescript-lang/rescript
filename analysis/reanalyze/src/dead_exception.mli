open Dead_common

val find_exception_from_decls :
  Declarations.t -> Dce_path.t -> Location.t option

val add :
  config:Dce_config.t ->
  decls:Declarations.builder ->
  file:File_context.t ->
  path:Dce_path.t ->
  loc:Location.t ->
  str_loc:Location.t ->
  module_loc:Location.t ->
  Name.t ->
  Name.t

val mark_as_used :
  config:Dce_config.t ->
  refs:References.builder ->
  file_deps:File_deps.builder ->
  cross_file:Cross_file_items.builder ->
  binding:Location.t ->
  loc_from:Location.t ->
  loc_to:Location.t ->
  Path.t ->
  unit
