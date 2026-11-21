(* Dict literals, [Pexp_dict]: [dict{"a": 1, ...d}]. They used to be parsed
   into calls of the runtime's [Primitive_dict], which remain their encoding on
   the v0 PPX wire:
   - without spreads, [Primitive_dict.make([("a", 1)])], whose [make] carries
     the [res.dict] marker;
   - with spreads, [Primitive_dict.spread(target, [part, ...])], whose
     [spread] carries the [res.dictSpread] marker. The target is
     [Primitive_dict.make(rows)] with the rows before the first spread, and
     the parts are the spreads and [Primitive_dict.make(rows)] for the rows
     between them, like [Object.assign]. These [make] calls carry the
     [res.dictRows] marker.
   The markers tell the encoding apart from calls written in the source, and
   the rows of a dict literal from a dict literal spread into it. *)

open Parsetree

let primitive_dict ~loc ~attrs name =
  Ast_helper.Exp.ident ~loc ~attrs
    (Location.mkloc
       (Longident.Ldot (Longident.Lident Primitive_modules.dict, name))
       loc)

let literal_marker = "res.dict"
let spread_marker = "res.dictSpread"
let rows_marker = "res.dictRows"

let make ~loc ~marker rows =
  Ast_helper.Exp.apply ~loc
    (primitive_dict ~loc ~attrs:[(Location.mknoloc marker, PStr [])] "make")
    [(Asttypes.Nolabel, Ast_dict.rows_array ~loc rows)]

(* The calls encoding the dict literal [dict{entries}] at [loc] *)
let to_calls ~loc ~attrs dict_entries =
  let e =
    match Ast_dict.target_and_sources (Ast_dict.parts dict_entries) with
    | `Rows rows -> make ~loc ~marker:literal_marker rows
    | `Spread (target, sources) ->
      Ast_helper.Exp.apply ~loc
        (primitive_dict ~loc
           ~attrs:[(Location.mknoloc spread_marker, PStr [])]
           "spread")
        [
          ( Nolabel,
            make ~loc:(Ast_dict.rows_loc ~loc target) ~marker:rows_marker target
          );
          ( Nolabel,
            Ast_helper.Exp.array ~loc
              (List.map
                 (function
                   | Ast_dict.Rows rows ->
                     make
                       ~loc:(Ast_dict.rows_loc ~loc rows)
                       ~marker:rows_marker rows
                   | Spread spread -> spread)
                 sources) );
        ]
  in
  {e with pexp_attributes = attrs}
