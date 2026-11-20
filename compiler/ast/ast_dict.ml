(* Dict literals, [Pexp_dict]: [dict{"a": 1, ...d}]. These helpers group
   their entries into rows and spreads, for the parser, the printer and the
   type checker. Ast_dict0 encodes dict literals on the v0 PPX wire. *)

open Parsetree

type ('row, 'spread) part = Rows of 'row list | Spread of 'spread

(* Group consecutive rows *)
let group (entries : [`Row of 'row | `Spread of 'spread] list) :
    ('row, 'spread) part list =
  let flush rows acc =
    match rows with
    | [] -> acc
    | _ -> Rows (List.rev rows) :: acc
  in
  let rec loop rows acc = function
    | [] -> List.rev (flush rows acc)
    | `Row row :: rest -> loop (row :: rows) acc rest
    | `Spread spread :: rest -> loop [] (Spread spread :: flush rows acc) rest
  in
  loop [] [] entries

(* The rows of a dict without spreads, or the target and the sources it's
   built from *)
let target_and_sources (parts : ('row, 'spread) part list) =
  match parts with
  | [] -> `Rows []
  | [Rows rows] -> `Rows rows
  | Rows target :: sources -> `Spread (target, sources)
  | sources -> `Spread ([], sources)

let entries (entries : dict_entry list) =
  List.map
    (function
      | Pdict_entry (key, value) -> `Row (key, value)
      | Pdict_spread spread -> `Spread spread)
    entries

let parts dict_entries = group (entries dict_entries)

let mk_loc loc_start loc_end = {Location.loc_start; loc_end; loc_ghost = false}

(* From the first key to the last value *)
let rows_loc ~loc (rows : (string Location.loc * expression) list) =
  match (rows, List.rev rows) with
  | (first_key, _) :: _, (_, last_value) :: _ ->
    mk_loc first_key.loc.loc_start last_value.pexp_loc.loc_end
  | _ -> loc

(* [[("a", 1), ...]], the argument of [Primitive_dict.make] *)
let rows_array ~loc (rows : (string Location.loc * expression) list) =
  Ast_helper.Exp.array ~loc
    (List.map
       (fun ((key : string Location.loc), value) ->
         Ast_helper.Exp.tuple
           ~loc:(mk_loc key.loc.loc_start value.pexp_loc.loc_end)
           [
             Ast_helper.Exp.constant ~loc:key.loc
               (Ast_helper.Const.string key.txt);
             value;
           ])
       rows)
