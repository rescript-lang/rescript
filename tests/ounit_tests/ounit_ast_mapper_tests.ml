let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let parse source =
  let parsed =
    Res_driver.parse_implementation_from_source ~display_filename:"Mapper.res"
      ~source
  in
  OUnit.assert_bool "source parses" (not parsed.invalid);
  parsed.parsetree

let parse_interface source =
  let parsed =
    Res_driver.parse_interface_from_source ~display_filename:"Mapper.resi"
      ~source
  in
  OUnit.assert_bool "source parses" (not parsed.invalid);
  parsed.parsetree

let mapper = Ast_mapper.default_mapper

(* Nodes whose construction takes attributes apart, such as [@as] on fields
   and constructors and [@react.component] on bindings, among the rest *)
let implementation =
  {|@@warning("-27")
type t = | @as("a") A | B(int) | C({@as("x") x: int, mutable y?: string})
type r<'a> = {@as("f") f: 'a, g: (int, ~l: string, ~o: float=?) => unit}
type e = ..
type e += E(int)
exception Failed(string)
@val external f: (@as(1) _, int) => int = "f"
module type S = {
  type t
  let x: t
}
module M: S with type t = int = {
  type t = int
  let x = 1
}
module F = (X: S) => {
  include X
}
@react.component
let make = (~a, ~b=?, ()) => {
  let {x, ?y} = {x: 1, y: None}
  let d = dict{"k": a}
  switch (a, b) {
  | (1, Some(c)) if c > 0 => <div className="c"> {React.int(c)} </div>
  | (_, _) as p => <> {React.null} </>
  }
}
let g = async x => await x
let h = x => x->Js.log
let s = `a${b}c`
let rec loop = n => n > 0 ? loop(n - 1) : ()
for i in 0 to 10 {
  while false {
    ()
  }
}
let o = {"a": 1}["a"]
let _ = %raw(`1`)
|}

let interface =
  {|type t = A | B(int)
@react.component
let make: (~a: int) => React.element
module type S = {
  type t
}
module M: S
|}

let test_unchanged_tree_is_returned_as_it_is _ =
  let structure = parse implementation in
  OUnit.assert_bool "structure" (mapper.structure mapper structure == structure);
  let signature = parse_interface interface in
  OUnit.assert_bool "signature" (mapper.signature mapper signature == signature)

let test_rewrite_rebuilds_only_its_path _ =
  let structure = parse "let a = (1, 2)\nlet b = (3, 4)\n" in
  let two = Ast_helper.Exp.constant (Pconst_integer ("20", None)) in
  let rewrite =
    {
      mapper with
      expr =
        (fun self e ->
          match e.pexp_desc with
          | Pexp_constant (Pconst_integer ("2", None)) -> two
          | _ -> mapper.expr self e);
    }
  in
  match (structure, rewrite.structure rewrite structure) with
  | [a; b], [a'; b'] -> (
    OUnit.assert_bool "the rewritten item is rebuilt" (a' != a);
    OUnit.assert_bool "the other item is shared" (b' == b);
    match a'.pstr_desc with
    | Pstr_value (_, [{pvb_expr = {pexp_desc = Pexp_tuple [one; two']}}]) -> (
      OUnit.assert_bool "the rewritten leaf is in place" (two' == two);
      match a.pstr_desc with
      | Pstr_value (_, [{pvb_expr = {pexp_desc = Pexp_tuple [one0; _]}}]) ->
        OUnit.assert_bool "its unchanged sibling is shared" (one == one0)
      | _ -> OUnit.assert_failure "a tuple")
    | _ -> OUnit.assert_failure "a tuple")
  | _ -> OUnit.assert_failure "a structure of two items"

(* Mappers can have effects, so the default mapper maps the parts of a node in
   the order it always has: an application's arguments before its function, a
   sequence's second expression before its first. *)
let test_parts_are_mapped_in_their_order _ =
  let visited = ref [] in
  let record =
    {
      mapper with
      expr =
        (fun self e ->
          (match e.pexp_desc with
          | Pexp_ident {txt = Lident name} -> visited := name :: !visited
          | _ -> ());
          mapper.expr self e);
    }
  in
  ignore (record.structure record (parse "let _ = {f(x, y); g(z)}\n"));
  OUnit.assert_equal ~printer:(String.concat " ") ["z"; "g"; "x"; "y"; "f"]
    (List.rev !visited)

let suites =
  __FILE__
  >::: [
         "unchanged_tree_is_returned_as_it_is"
         >:: test_unchanged_tree_is_returned_as_it_is;
         "rewrite_rebuilds_only_its_path"
         >:: test_rewrite_rebuilds_only_its_path;
         "parts_are_mapped_in_their_order"
         >:: test_parts_are_mapped_in_their_order;
       ]
