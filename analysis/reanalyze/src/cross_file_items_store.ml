(** Abstraction over cross-file items storage.

    Allows iteration over optional arg calls and function refs from either:
    - [Frozen]: Collected [CrossFileItems.t] 
    - [Reactive]: Direct iteration over reactive collection (no intermediate allocation) *)

type t =
  | Frozen of Cross_file_items.t
  | Reactive of (string, Cross_file_items.t) Reactive.t

let of_frozen cfi = Frozen cfi

let of_reactive reactive = Reactive reactive

let iter_optional_arg_calls t f =
  match t with
  | Frozen cfi -> List.iter f cfi.Cross_file_items.optional_arg_calls
  | Reactive r ->
    Reactive.iter
      (fun _path items -> List.iter f items.Cross_file_items.optional_arg_calls)
      r

let iter_function_refs t f =
  match t with
  | Frozen cfi -> List.iter f cfi.Cross_file_items.function_refs
  | Reactive r ->
    Reactive.iter
      (fun _path items -> List.iter f items.Cross_file_items.function_refs)
      r

(** Compute optional args state from calls and function references.
    Returns a map from position to final OptionalArgs.t state.
    Pure function - does not mutate declarations. *)
let compute_optional_args_state (store : t) ~find_decl ~is_live :
    Optional_args_state.t =
  let state = Optional_args_state.create () in
  (* Initialize state from declarations *)
  let get_state pos =
    match Optional_args_state.find_opt state pos with
    | Some s -> s
    | None -> (
      match find_decl pos with
      | Some {Decl.decl_kind = Value {optional_args}} -> optional_args
      | _ -> Optional_args.empty)
  in
  let set_state pos s = Optional_args_state.set state pos s in
  (* Process optional arg calls *)
  iter_optional_arg_calls store
    (fun {Cross_file_items.pos_from; pos_to; arg_names; arg_names_maybe} ->
      if is_live pos_from then
        let current = get_state pos_to in
        let updated =
          Optional_args.apply_call ~arg_names ~arg_names_maybe current
        in
        set_state pos_to updated);
  (* Process function references *)
  iter_function_refs store (fun {Cross_file_items.pos_from; pos_to} ->
      if is_live pos_from then
        let state_from = get_state pos_from in
        let state_to = get_state pos_to in
        if not (Optional_args.is_empty state_to) then (
          let updated_from, updated_to =
            Optional_args.combine_pair state_from state_to
          in
          set_state pos_from updated_from;
          set_state pos_to updated_to));
  state

let compute_live_direct_optional_arg_calls (store : t) ~find_decl ~is_live :
    Pos_set.t =
  let direct_calls = ref Pos_set.empty in
  iter_optional_arg_calls store (fun {Cross_file_items.pos_from; pos_to; _} ->
      if is_live pos_from then
        match find_decl pos_to with
        | Some
            {
              Decl.decl_kind =
                Value
                  {
                    optional_args_report =
                      Decl.Kind.ReportOptionalArgsIfDirectCall;
                  };
            } ->
          direct_calls := Pos_set.add pos_to !direct_calls
        | _ -> ());
  !direct_calls
