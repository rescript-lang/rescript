open Types

type error =
  | Invalid_type
  | Requires_type_annotation of string
  | Not_record of Longident.t
  | Field_not_optional of string list * Longident.t
  | Field_missing of string list * Longident.t
  | Extra_field of string * Longident.t
  | Field_runtime_name_mismatch of {
      field: string;
      rest_type: Longident.t;
      source_runtime_name: string;
      rest_runtime_name: string;
    }

exception Error of Location.t * Env.t * error

val type_record_pat_rest :
  env:Env.t ->
  pattern_force:(unit -> unit) list ref ->
  loc:Location.t ->
  record_ty:type_expr ->
  lbl_pat_list:
    (Longident.t Location.loc * label_description * Typedtree.pattern * bool)
    list ->
  rest:Parsetree.record_pat_rest ->
  enter_variable:(Location.t -> string Location.loc -> type_expr -> Ident.t) ->
  unify_pat_types:(Location.t -> Env.t -> type_expr -> type_expr -> unit) ->
  check_not_private:(Location.t -> type_expr -> type_declaration -> unit) ->
  Typedtree.record_pat_rest

val report_error : Format.formatter -> error -> unit
