(* Array and list literals with spreads, [Pexp_spread]: [[a, ...b]] and
   [list{...a, b}]. They used to be parsed into calls of the runtime's
   [Primitive_array.spread] and [Primitive_list.spread], which remain their
   encoding on the v0 PPX wire and the calls they compile to. The [spread]
   carries the [res.spread] marker, and its argument is an array of parts:
   - for arrays, the spreads, and array literals of the items between them;
   - for lists, the items before each spread consed onto it
     ([a :: b :: spread]), and a list literal of the items after the last one.
   The array literals and [::] and [[]] constructors built from the items have
   ghost locations, which tells them apart from array and list literals that
   are spread. *)

open Parsetree

let marker = "res.spread"

let module_name = function
  | Spread_array -> Primitive_modules.array
  | Spread_list -> Primitive_modules.list

let kind_of_module_name name =
  if name = Primitive_modules.array then Some Spread_array
  else if name = Primitive_modules.list then Some Spread_list
  else None

(* The items before each spread with that spread, and the items after the last
   spread *)
let groups (elements : spread_element list) :
    (expression list * expression option) list =
  let flush items acc =
    match items with
    | [] -> acc
    | _ -> (List.rev items, None) :: acc
  in
  let rec loop items acc = function
    | [] -> List.rev (flush items acc)
    | Pspread_item item :: rest -> loop (item :: items) acc rest
    | Pspread spread :: rest ->
      loop [] ((List.rev items, Some spread) :: acc) rest
  in
  loop [] [] elements

let ghost_span (first : expression) (last : expression) =
  {
    Location.loc_start = first.pexp_loc.loc_start;
    loc_end = last.pexp_loc.loc_end;
    loc_ghost = true;
  }

(* The parts in the argument of [Primitive_array.spread] or
   [Primitive_list.spread], each an array or list *)
let parts kind elements =
  let construct ~loc name args =
    Ast_helper.Exp.construct ~loc
      (Location.mkloc (Longident.Lident name) loc)
      (Location.mkloc args loc)
  in
  List.concat_map
    (fun (items, spread) ->
      match (kind, items, List.rev items) with
      | _, [], _ | _, _, [] -> Option.to_list spread
      | Spread_array, first :: _, last :: _ ->
        Ast_helper.Exp.array ~loc:(ghost_span first last) items
        :: Option.to_list spread
      | Spread_list, _, last :: _ ->
        let tail =
          match spread with
          | Some spread -> spread
          | None ->
            construct
              ~loc:
                {
                  last.pexp_loc with
                  loc_start = last.pexp_loc.loc_end;
                  loc_ghost = true;
                }
              "[]" []
        in
        [
          List.fold_right
            (fun item tail ->
              construct ~loc:(ghost_span item tail) "::" [item; tail])
            items tail;
        ])
    (groups elements)

(* The call encoding the literal [kind] of [elements] at [loc] *)
let to_call ~loc ~attrs kind elements =
  Ast_helper.Exp.apply ~loc ~attrs
    (Ast_helper.Exp.ident ~loc
       ~attrs:[(Location.mknoloc marker, PStr [])]
       (Location.mkloc
          (Longident.Ldot (Longident.Lident (module_name kind), "spread"))
          loc))
    [(Asttypes.Nolabel, Ast_helper.Exp.array ~loc (parts kind elements))]
