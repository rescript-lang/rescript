let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

(* The raw Lambda bound to [name] in [source]. *)
let binding_lambda ~name source =
  let parsed =
    Res_driver.parse_implementation_from_source
      ~display_filename:"PartialApplication.res" ~source
  in
  OUnit.assert_bool "source parses" (not parsed.invalid);
  let typed, _, _ =
    Typemod.type_structure (Env.initial_safe_string ()) parsed.parsetree
  in
  let implementation =
    Translmod.transl_implementation "PartialApplication"
      (typed, Typedtree.Tcoerce_none)
  in
  let found = ref None in
  let rec find (lam : Lambda.t) =
    (match lam with
    | Llet (_, id, arg, _) when Ident.name id = name -> found := Some arg
    | _ -> ());
    Lambda_traverse.iter find lam
  in
  find implementation.lambda;
  match !found with
  | Some lam -> lam
  | None -> OUnit.assert_failure ("no binding for " ^ name)

let unexpected lam =
  OUnit.assert_failure ("unexpected Lambda: " ^ Printlambda.lambda_to_string lam)

let is_call_of name (lam : Lambda.t) =
  match lam with
  | Lapply {ap_func = Lvar f} -> Ident.name f = name
  | _ -> false

let suites =
  __FILE__
  >::: [
         ( "callee and arguments are bound outside the function, callee first"
         >:: fun _ ->
           let lam =
             binding_lambda ~name:"h"
               "let add = (a, _b, _c) => a\n\
                let getAdd = () => add\n\
                let next = () => 1\n\
                let h = getAdd()(next(), ...)\n"
           in
           match lam with
           | Llet
               ( Strict,
                 func,
                 callee,
                 Llet
                   ( Strict,
                     arg,
                     argument,
                     Lfunction
                       {
                         params = [extra1; extra2];
                         body =
                           Lapply
                             {
                               ap_func = Lvar f;
                               ap_args = [Lvar a; Lvar e1; Lvar e2];
                             };
                       } ) )
             when is_call_of "getAdd" callee && is_call_of "next" argument ->
             OUnit.assert_bool "the function calls the bound callee"
               (Ident.same f func);
             OUnit.assert_bool "the function passes the bound argument"
               (Ident.same a arg);
             OUnit.assert_bool "the remaining parameters are passed on"
               (Ident.same e1 extra1 && Ident.same e2 extra2)
           | lam -> unexpected lam );
         ( "variables and constants stay in the function" >:: fun _ ->
           let lam =
             binding_lambda ~name:"h"
               "let add = (a, _b, _c) => a\nlet x = 1\nlet h = add(x, 2, ...)\n"
           in
           match lam with
           | Lfunction
               {
                 body =
                   Lapply
                     {
                       ap_func = Lvar f;
                       ap_args = [Lvar x; Lconst (Const_int 2l); Lvar _];
                     };
               }
             when Ident.name f = "add" && Ident.name x = "x" ->
             ()
           | lam -> unexpected lam );
         ( "an allocating constant is bound outside the function" >:: fun _ ->
           let lam =
             binding_lambda ~name:"h"
               "let first = (x, _y) => x\nlet h = first((1, 2), ...)\n"
           in
           match lam with
           | Llet
               ( Strict,
                 arg,
                 Lconst (Const_block _),
                 Lfunction
                   {
                     body = Lapply {ap_func = Lvar f; ap_args = [Lvar a; Lvar _]};
                   } )
             when Ident.name f = "first" && Ident.same a arg ->
             ()
           | lam -> unexpected lam );
       ]
