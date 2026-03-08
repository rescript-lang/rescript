(** Abstraction over declaration storage.

    Allows the solver to work with either:
    - [Frozen]: Traditional [Declarations.t] (copied from reactive)
    - [Reactive]: Direct [Reactive.t] (no copy, zero-cost on warm runs)

    This eliminates the O(N) freeze step when using reactive mode. *)

type t =
  | Frozen of Declarations.t
  | Reactive of (Lexing.position, Decl.t) Reactive.t

let of_frozen decls = Frozen decls

let of_reactive reactive = Reactive reactive

let find_opt t pos =
  match t with
  | Frozen decls -> Declarations.find_opt decls pos
  | Reactive reactive ->
    let mb = Reactive.get reactive (Stable.unsafe_of_value pos) in
    if Maybe.is_some mb then
      Some (Stable.unsafe_to_nonlinear_value (Maybe.unsafe_get mb))
    else None

let fold f t init =
  match t with
  | Frozen decls -> Declarations.fold f decls init
  | Reactive reactive ->
    let acc = ref init in
    Reactive.iter
      (fun pos decl ->
        acc :=
          f
            (Stable.unsafe_to_nonlinear_value pos)
            (Stable.unsafe_to_nonlinear_value decl)
            !acc)
      reactive;
    !acc

let iter f t =
  match t with
  | Frozen decls -> Declarations.iter f decls
  | Reactive reactive ->
    Reactive.iter
      (fun pos decl ->
        f
          (Stable.unsafe_to_nonlinear_value pos)
          (Stable.unsafe_to_nonlinear_value decl))
      reactive
