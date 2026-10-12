let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

module E = Js_exp_make

let statement desc : J.statement =
  {statement_desc = desc; comment = None; source_loc = None}

let case body : J.case_clause =
  {switch_body = body; should_break = true; comment = None; source_loc = None}

(* [switch (g) { case 1: let g = 1; f(g, <global g>) }] *)
let switch_on_global () =
  let local = Ident.create "g" in
  let read_global =
    statement
      (Exp
         (E.call ~info:Js_call_info.na_full_call (E.js_global "f")
            [E.var local; E.js_global "g"]))
  in
  let body =
    [
      Js_stmt_make.define_variable ~kind:Lambda.Strict local (E.small_int 1);
      read_global;
    ]
  in
  let program : J.program =
    {
      block = [statement (Int_switch (E.js_global "g", [(1, case body)], None))];
      exports = [];
      export_set = Set_ident.empty;
    }
  in
  Js_pass_external_shadow.program program

let is_global_read (e : J.expression) =
  match e.expression_desc with
  | Var (Id id) -> Ext_ident.is_js id && id.name = "g"
  | _ -> false

let is_global_this_read (e : J.expression) =
  match e.expression_desc with
  | Static_index ({expression_desc = Var (Id gt)}, "g", _) ->
    Ext_ident.is_js gt && gt.name = "globalThis"
  | _ -> false

let suites =
  __FILE__
  >::: [
         ( "switch discriminant is read outside the scope of the cases"
         >:: fun _ ->
           match switch_on_global () with
           | {block = [{statement_desc = Int_switch (discr, [(_, c)], None)}]}
             -> (
             OUnit.assert_bool "discriminant reads the global directly"
               (is_global_read discr);
             match c.switch_body with
             | [_; {statement_desc = Exp {expression_desc = Call (_, args, _)}}]
               ->
               OUnit.assert_bool "read in the case goes through globalThis"
                 (List.exists is_global_this_read args)
             | _ -> OUnit.assert_failure "unexpected case body")
           | _ -> OUnit.assert_failure "unexpected program shape" );
       ]
