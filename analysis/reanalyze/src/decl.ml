(** Declaration types for dead code analysis. *)

module Kind = struct
  type t =
    | Exception
    | RecordLabel
    | VariantCase
    | Value of {
        is_toplevel: bool;
        mutable reports_optional_args: bool;
        mutable optional_args: Optional_args.t;
        side_effects: bool;
      }

  let is_type dk =
    match dk with
    | RecordLabel | VariantCase -> true
    | Exception | Value _ -> false

  let to_string dk =
    match dk with
    | Exception -> "Exception"
    | RecordLabel -> "RecordLabel"
    | VariantCase -> "VariantCase"
    | Value _ -> "Value"
end

type pos_adjustment = FirstVariant | OtherVariant | Nothing

type t = {
  decl_kind: Kind.t;
  module_loc: Location.t;
  pos_adjustment: pos_adjustment;
  path: Dce_path.t;
      (** For type re-exports (e.g. [type y = x = {...}]), record/variant label
      declarations belonging to the re-exporting type can carry the manifest
      type path so [DeadType.process_type_label_dependencies] can link fields
      without needing the typed tree. *)
  manifest_type_path: Dce_path.t option;
  pos: Lexing.position;
  pos_end: Lexing.position;
  pos_start: Lexing.position;
  mutable resolved_dead: bool option;
  mutable report: bool;
}

let is_value decl =
  match decl.decl_kind with
  | Value _ (* | Exception *) -> true
  | _ -> false

(** Check if a declaration is live (or unknown). Returns false only if resolved as dead. *)
let is_live decl =
  match decl.resolved_dead with
  | Some true -> false
  | Some false | None -> true

let compare_using_dependencies ~ordered_files
    {
      decl_kind = kind1;
      path = _path1;
      pos =
        {pos_fname = fname1; pos_lnum = lnum1; pos_bol = bol1; pos_cnum = cnum1};
    }
    {
      decl_kind = kind2;
      path = _path2;
      pos =
        {pos_fname = fname2; pos_lnum = lnum2; pos_bol = bol2; pos_cnum = cnum2};
    } =
  let find_position fn = Hashtbl.find ordered_files fn [@@raises Not_found] in
  (* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top. *)
  let position1, position2 =
    try (fname1 |> find_position, fname2 |> find_position)
    with Not_found -> (0, 0)
  in
  compare
    (position1, lnum2, bol2, cnum2, kind1)
    (position2, lnum1, bol1, cnum1, kind2)

let compare_for_reporting
    {
      decl_kind = kind1;
      pos =
        {pos_fname = fname1; pos_lnum = lnum1; pos_bol = bol1; pos_cnum = cnum1};
    }
    {
      decl_kind = kind2;
      pos =
        {pos_fname = fname2; pos_lnum = lnum2; pos_bol = bol2; pos_cnum = cnum2};
    } =
  compare (fname1, lnum1, bol1, cnum1, kind1) (fname2, lnum2, bol2, cnum2, kind2)
