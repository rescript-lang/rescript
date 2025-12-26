(** Abstraction over cross-file items storage.

    Allows iteration over optional arg calls and function refs from either:
    - [Frozen]: Collected [CrossFileItems.t] 
    - [Reactive]: Direct iteration over reactive collection (no intermediate allocation) *)

type t =
  | Frozen of CrossFileItems.t
  | Reactive of (string, CrossFileItems.t) Reactive.t

let of_frozen cfi = Frozen cfi

let of_reactive reactive = Reactive reactive

let iter_optional_arg_calls t f =
  match t with
  | Frozen cfi -> List.iter f cfi.CrossFileItems.optional_arg_calls
  | Reactive r ->
    Reactive.iter
      (fun _path items -> List.iter f items.CrossFileItems.optional_arg_calls)
      r

let iter_function_refs t f =
  match t with
  | Frozen cfi -> List.iter f cfi.CrossFileItems.function_refs
  | Reactive r ->
    Reactive.iter
      (fun _path items -> List.iter f items.CrossFileItems.function_refs)
      r

(** Compute optional args state from calls and function references.
    Returns a map from position to final OptionalArgs.t state.
    Pure function - does not mutate declarations. *)
let compute_optional_args_state (store : t) ~find_decl ~is_live :
    OptionalArgsState.t =
  let state = OptionalArgsState.create () in
  (* Initialize state from declarations *)
  let get_state pos =
    match OptionalArgsState.find_opt state pos with
    | Some s -> s
    | None -> (
      match find_decl pos with
      | Some {Decl.declKind = Value {optionalArgs}} -> optionalArgs
      | _ -> OptionalArgs.empty)
  in
  let set_state pos s = OptionalArgsState.set state pos s in
  (* Process optional arg calls *)
  iter_optional_arg_calls store
    (fun {CrossFileItems.pos_from; pos_to; arg_names; arg_names_maybe} ->
      if is_live pos_from then
        let current = get_state pos_to in
        let updated =
          OptionalArgs.apply_call ~argNames:arg_names
            ~argNamesMaybe:arg_names_maybe current
        in
        set_state pos_to updated);
  (* Process function references *)
  iter_function_refs store (fun {CrossFileItems.pos_from; pos_to} ->
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
