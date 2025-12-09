(** Cross-file items collected during AST processing.
    
    These are references that span file boundaries and need to be resolved
    after all files are processed. *)

(** {2 Item types} *)

type exception_ref = {exception_path: DcePath.t; loc_from: Location.t}

type optional_arg_call = {
  pos_from: Lexing.position;
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

let add_optional_arg_call (b : builder) ~pos_from ~pos_to ~arg_names
    ~arg_names_maybe =
  b.optional_arg_calls <-
    {pos_from; pos_to; arg_names; arg_names_maybe} :: b.optional_arg_calls

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

let process_exception_refs (t : t) ~refs ~file_deps ~find_exception ~config =
  t.exception_refs
  |> List.iter (fun {exception_path; loc_from} ->
         match find_exception exception_path with
         | None -> ()
         | Some loc_to ->
           DeadCommon.addValueReference ~config ~refs ~file_deps
             ~binding:Location.none ~addFileReference:true ~locFrom:loc_from
             ~locTo:loc_to)

(** Compute optional args state from calls and function references.
    Returns a map from position to final OptionalArgs.t state.
    Pure function - does not mutate declarations. *)
let compute_optional_args_state (t : t) ~decls ~is_live : OptionalArgsState.t =
  let state = OptionalArgsState.create () in
  (* Initialize state from declarations *)
  let get_state pos =
    match OptionalArgsState.find_opt state pos with
    | Some s -> s
    | None -> (
      match Declarations.find_opt decls pos with
      | Some {declKind = Value {optionalArgs}} -> optionalArgs
      | _ -> OptionalArgs.empty)
  in
  let set_state pos s = OptionalArgsState.set state pos s in
  (* Process optional arg calls *)
  t.optional_arg_calls
  |> List.iter (fun {pos_from; pos_to; arg_names; arg_names_maybe} ->
         if is_live pos_from then
           let current = get_state pos_to in
           let updated =
             OptionalArgs.apply_call ~argNames:arg_names
               ~argNamesMaybe:arg_names_maybe current
           in
           set_state pos_to updated);
  (* Process function references *)
  t.function_refs
  |> List.iter (fun {pos_from; pos_to} ->
         if is_live pos_from then
           let state_from = get_state pos_from in
           let state_to = get_state pos_to in
           if not (OptionalArgs.isEmpty state_to) then (
             let updated_from, updated_to =
               OptionalArgs.combine_pair state_from state_to
             in
             set_state pos_from updated_from;
             set_state pos_to updated_to));
  state
