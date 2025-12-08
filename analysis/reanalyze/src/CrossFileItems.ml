(** Cross-file items collected during AST processing.
    
    These are references that span file boundaries and need to be resolved
    after all files are processed. *)

open Common

(** {2 Item types} *)

type exception_ref = {exception_path: Path.t; loc_from: Location.t}

type optional_arg_call = {
  pos_to: Lexing.position;
  arg_names: string list;
  arg_names_maybe: string list;
}

type function_ref = {pos_from: Lexing.position; pos_to: Lexing.position}

(** {2 Types} *)

type t = {
  exception_refs: exception_ref list;
  optional_arg_calls: optional_arg_call list;
  function_refs: function_ref list;
}

type builder = {
  mutable exception_refs: exception_ref list;
  mutable optional_arg_calls: optional_arg_call list;
  mutable function_refs: function_ref list;
}

(** {2 Builder API} *)

let create_builder () : builder =
  {exception_refs = []; optional_arg_calls = []; function_refs = []}

let add_exception_ref (b : builder) ~exception_path ~loc_from =
  b.exception_refs <- {exception_path; loc_from} :: b.exception_refs

let add_optional_arg_call (b : builder) ~pos_to ~arg_names ~arg_names_maybe =
  b.optional_arg_calls <-
    {pos_to; arg_names; arg_names_maybe} :: b.optional_arg_calls

let add_function_reference (b : builder) ~pos_from ~pos_to =
  b.function_refs <- {pos_from; pos_to} :: b.function_refs

(** {2 Merge API} *)

let merge_all (builders : builder list) : t =
  let exception_refs =
    builders |> List.concat_map (fun b -> b.exception_refs)
  in
  let optional_arg_calls =
    builders |> List.concat_map (fun b -> b.optional_arg_calls)
  in
  let function_refs = builders |> List.concat_map (fun b -> b.function_refs) in
  {exception_refs; optional_arg_calls; function_refs}

(** {2 Processing API} *)

let process_exception_refs (t : t) ~refs ~find_exception ~config =
  t.exception_refs
  |> List.iter (fun {exception_path; loc_from} ->
         match find_exception exception_path with
         | None -> ()
         | Some loc_to ->
           DeadCommon.addValueReference ~config ~refs ~binding:Location.none
             ~addFileReference:true ~locFrom:loc_from ~locTo:loc_to)

let process_optional_args (t : t) ~decls =
  (* Process optional arg calls *)
  t.optional_arg_calls
  |> List.iter (fun {pos_to; arg_names; arg_names_maybe} ->
         match Declarations.find_opt decls pos_to with
         | Some {declKind = Value r} ->
           r.optionalArgs
           |> OptionalArgs.call ~argNames:arg_names
                ~argNamesMaybe:arg_names_maybe
         | _ -> ());
  (* Process function references *)
  t.function_refs
  |> List.iter (fun {pos_from; pos_to} ->
         match
           ( Declarations.find_opt decls pos_from,
             Declarations.find_opt decls pos_to )
         with
         | Some {declKind = Value rFrom}, Some {declKind = Value rTo}
           when not (OptionalArgs.isEmpty rTo.optionalArgs) ->
           OptionalArgs.combine rFrom.optionalArgs rTo.optionalArgs
         | _ -> ())
