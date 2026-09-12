(** Cross-file items collected during AST processing.
    
    These are references that span file boundaries and need to be resolved
    after all files are processed. *)

(** {2 Item types} *)

type exception_ref = {exception_path: Dce_path.t; loc_from: Location.t}

type optional_arg_call = {
  pos_from: Lexing.position;
  pos_to: Lexing.position;
  arg_names: string list;
  arg_names_maybe: string list;
}

type function_ref = {pos_from: Lexing.position; pos_to: Lexing.position}

type coercion = {
  source_type_paths: Dce_path.t list;
  target_type_paths: Dce_path.t list;
}

type optional_arg_value_escape = {
  pos_from: Lexing.position;
  pos_to: Lexing.position;
}

(** {2 Types} *)

type t = {
  exception_refs: exception_ref list;
  optional_arg_calls: optional_arg_call list;
  function_refs: function_ref list;
  optional_arg_value_escapes: optional_arg_value_escape list;
  coercions: coercion list;
}

type builder = {
  mutable exception_refs: exception_ref list;
  mutable optional_arg_calls: optional_arg_call list;
  mutable function_refs: function_ref list;
  mutable optional_arg_value_escapes: optional_arg_value_escape list;
  mutable coercions: coercion list;
}

(** {2 Builder API} *)

let create_builder () : builder =
  {
    exception_refs = [];
    optional_arg_calls = [];
    function_refs = [];
    optional_arg_value_escapes = [];
    coercions = [];
  }

let add_exception_ref (b : builder) ~exception_path ~loc_from =
  b.exception_refs <- {exception_path; loc_from} :: b.exception_refs

let add_optional_arg_call (b : builder) ~pos_from ~pos_to ~arg_names
    ~arg_names_maybe =
  b.optional_arg_calls <-
    {pos_from; pos_to; arg_names; arg_names_maybe} :: b.optional_arg_calls

let add_function_reference (b : builder) ~pos_from ~pos_to =
  b.function_refs <- {pos_from; pos_to} :: b.function_refs

let add_optional_arg_value_escape (b : builder) ~pos_from ~pos_to =
  b.optional_arg_value_escapes <-
    {pos_from; pos_to} :: b.optional_arg_value_escapes

let add_coercion (b : builder) ~source_type_paths ~target_type_paths =
  b.coercions <- {source_type_paths; target_type_paths} :: b.coercions

(** {2 Merge API} *)

let merge_all (builders : builder list) : t =
  let exception_refs =
    builders |> List.concat_map (fun b -> b.exception_refs)
  in
  let optional_arg_calls =
    builders |> List.concat_map (fun b -> b.optional_arg_calls)
  in
  let function_refs = builders |> List.concat_map (fun b -> b.function_refs) in
  let optional_arg_value_escapes =
    builders |> List.concat_map (fun b -> b.optional_arg_value_escapes)
  in
  let coercions = builders |> List.concat_map (fun b -> b.coercions) in
  {
    exception_refs;
    optional_arg_calls;
    function_refs;
    optional_arg_value_escapes;
    coercions;
  }

(** {2 Builder extraction for reactive merge} *)

let builder_to_t (builder : builder) : t =
  {
    exception_refs = builder.exception_refs;
    optional_arg_calls = builder.optional_arg_calls;
    function_refs = builder.function_refs;
    optional_arg_value_escapes = builder.optional_arg_value_escapes;
    coercions = builder.coercions;
  }
