open DeadCommon

val find_exception_from_decls : Declarations.t -> DcePath.t -> Location.t option

val add :
  config:DceConfig.t ->
  decls:Declarations.builder ->
  file:FileContext.t ->
  path:DcePath.t ->
  loc:Location.t ->
  strLoc:Location.t ->
  moduleLoc:Location.t ->
  Name.t ->
  Name.t

val markAsUsed :
  config:DceConfig.t ->
  refs:References.builder ->
  file_deps:FileDeps.builder ->
  cross_file:CrossFileItems.builder ->
  binding:Location.t ->
  locFrom:Location.t ->
  locTo:Location.t ->
  Path.t ->
  unit
