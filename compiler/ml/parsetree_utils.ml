open Parsetree
open Asttypes
open Longident

(* Optional arguments with defaults are lowered in two steps:

     let f = (~test: option<int>=42) => body

   first binds the raw optional argument, then rebinds the resolved value:

     let *opt* = test in
     let test = switch *opt* {
     | Some(x) => x
     | None => 42
     }

   The second binding is no longer option<int>; it is plain int. This helper
   rewrites the original pattern so that:

     (~test: option<int>=42)

   becomes a resolved-value pattern equivalent to:

     (~test: int=42)

   for the synthesized rebinding step only. *)
let rec adapt_pattern_for_resolved_default pattern =
  match pattern.ppat_desc with
  | Ppat_constraint
      ( inner_pattern,
        ({ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [inner_type])} as
         inner_constraint) ) ->
    (* After resolving Some/None, the value has type t, not option<t>. *)
    {
      pattern with
      ppat_desc =
        Ppat_constraint
          ( adapt_pattern_for_resolved_default inner_pattern,
            {inner_constraint with ptyp_desc = inner_type.ptyp_desc} );
    }
  | Ppat_constraint
      (inner_pattern, ({ptyp_desc = Ptyp_package _} as inner_constraint)) ->
    (* Preserve first-class module constraints. We still recurse so an outer
       option<...> annotation can be removed if present. *)
    {
      pattern with
      ppat_desc =
        Ppat_constraint
          (adapt_pattern_for_resolved_default inner_pattern, inner_constraint);
    }
  | Ppat_constraint (inner_pattern, inner_constraint) ->
    {
      pattern with
      ppat_desc =
        Ppat_constraint
          (adapt_pattern_for_resolved_default inner_pattern, inner_constraint);
    }
  | _ -> pattern
