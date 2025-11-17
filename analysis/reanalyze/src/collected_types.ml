open Common

type value_decl = {
  name: Name.t;
  path: Path.t;
  loc: Location.t;
  module_loc: Location.t;
  optional_args: OptionalArgs.t;
  side_effects: bool;
  is_toplevel: bool;
  pos_start: Lexing.position option;
  pos_end: Lexing.position option;
}

type general_decl = {
  name: Name.t;
  path: Path.t;
  loc: Location.t;
  module_loc: Location.t;
  decl_kind: DeclKind.t;
  pos_adjustment: posAdjustment;
  pos_start: Lexing.position option;
  pos_end: Lexing.position option;
}

type decl =
  | Value_decl of value_decl
  | General_decl of general_decl

type value_reference = {
  loc_from: Location.t;
  loc_to: Location.t;
  add_file_reference: bool;
  target_path: Path.t option;
}

type type_reference = {pos_from: Lexing.position; pos_to: Lexing.position}

type t = {
  decls: Common.decl list;
  value_references: value_reference list;
  type_references: type_reference list;
}

let empty = {decls = []; value_references = []; type_references = []}

let key_of_decl = function
  | Value_decl {loc; _} -> loc.loc_start
  | General_decl {loc; _} -> loc.loc_start

let normalize_pos default override =
  match override with
  | Some pos -> pos
  | None -> default

let should_record ~current_src ~current_module loc =
  (not loc.Location.loc_ghost)
  && (String.equal current_src loc.Location.loc_start.pos_fname
     || String.equal current_module "*include*")

let to_common_decl ~current_src ~current_module = function
  | Value_decl
      {
        name;
        path;
        loc;
        module_loc;
        optional_args;
        side_effects;
        is_toplevel;
        pos_start;
        pos_end;
      } ->
    if should_record ~current_src ~current_module loc then
      let pos = loc.loc_start in
      Some
        {
          declKind = Value {isToplevel = is_toplevel; optionalArgs = optional_args; sideEffects = side_effects};
          moduleLoc = module_loc;
          posAdjustment = Nothing;
          path = name :: path;
          pos;
          posEnd = normalize_pos loc.loc_end pos_end;
          posStart = normalize_pos pos pos_start;
          resolvedDead = None;
          report = true;
        }
    else None
  | General_decl
      {
        name;
        path;
        loc;
        module_loc;
        decl_kind;
        pos_adjustment;
        pos_start;
        pos_end;
      } ->
    if should_record ~current_src ~current_module loc then
      let pos = loc.loc_start in
      Some
        {
          declKind = decl_kind;
          moduleLoc = module_loc;
          posAdjustment = pos_adjustment;
          path = name :: path;
          pos;
          posEnd = normalize_pos loc.loc_end pos_end;
          posStart = normalize_pos pos pos_start;
          resolvedDead = None;
          report = true;
        }
    else None

