open GenTypeCommon

type export_type = {
  loc: Location.t;
  name_as: string option;
  opaque: bool option;
  type_: type_;
  type_vars: string list;
  resolved_type_name: ResolvedName.t;
  doc_string: DocString.t;
}

type import_value = {
  as_path: string;
  import_annotation: Annotation.import;
  type_: type_;
  value_name: string;
}

type export_value = {
  doc_string: DocString.t;
  module_access_path: Runtime.module_access_path;
  original_name: string;
  resolved_name: ResolvedName.t;
  type_: type_;
}

type export_from_type_declaration = {
  export_type: export_type;
  annotation: Annotation.t;
}

type import_type = {
  type_name: string;
  as_type_name: string option;
  import_path: ImportPath.t;
}

type export_type_item = {
  type_vars: string list;
  type_: type_;
  annotation: Annotation.t;
}

type export_type_map = export_type_item StringMap.t

type type_declaration = {
  export_from_type_declaration: export_from_type_declaration;
  import_types: import_type list;
}

type t = ExportValue of export_value | ImportValue of import_value

type translation = {
  import_types: import_type list;
  code_items: t list;
  type_declarations: type_declaration list;
}
