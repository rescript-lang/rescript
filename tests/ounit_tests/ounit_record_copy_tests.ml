let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

(* A record spread that copies the fields of its base one by one, as raw
   Lambda: a mutable field that comes after an override with side effects is
   read into a [let] before the record is built, so that the override can't
   change the copy. *)

(* The raw Lambda bound to [name] in [source]. *)
let binding_lambda ~name source =
  let parsed =
    Res_driver.parse_implementation_from_source
      ~display_filename:"RecordCopy.res" ~source
  in
  OUnit.assert_bool "source parses" (not parsed.invalid);
  let typed, signature, env =
    Typemod.type_structure (Env.initial_safe_string ()) parsed.parsetree
  in
  let implementation =
    Translmod.transl_implementation "RecordCopy" env signature
      (typed, Typedtree.Tcoerce_none)
  in
  (* The traversal visits a [let] before its body, so the first match is the
     outermost one; stop there. *)
  let found = ref None in
  let rec find (lam : Lambda.t) =
    match (!found, lam) with
    | Some _, _ -> ()
    | None, Llet (_, id, arg, _) when Ident.name id = name -> found := Some arg
    | None, _ -> Lambda_traverse.iter find lam
  in
  find implementation.lambda;
  match !found with
  | Some lam -> lam
  | None -> OUnit.assert_failure ("no binding for " ^ name)

(* [Read (var, pos)] reads field [pos] of the variable [var]; [Var var] is a
   variable bound outside the function body's [let]s, such as a parameter. *)
type value =
  | Read of string * int
  | Var of string
  | Bound of string
  | Call
  | Other

let value_to_string = function
  | Read (var, pos) -> "read " ^ var ^ "." ^ string_of_int pos
  | Var var -> "var " ^ var
  | Bound name -> "bound " ^ name
  | Call -> "call"
  | Other -> "other"

(* The [let]s a function's body binds, outermost first, with the value each
   one binds, and how the record it then builds gets the value of each field,
   in position order: read from a variable in place, taken from one of those
   [let]s, or computed by an override. *)
let copy_shape (lam : Lambda.t) =
  let unexpected lam =
    OUnit.assert_failure
      ("unexpected Lambda: " ^ Printlambda.lambda_to_string lam)
  in
  (* [lets] holds the [let]s around [arg], innermost first. *)
  let value lets (arg : Lambda.t) =
    match arg with
    | Lprim {primitive = Pfield (pos, _); args = [Lvar id]} ->
      Read (Ident.name id, pos)
    | Lvar id when List.exists (fun (bound, _) -> Ident.same bound id) lets ->
      Bound (Ident.name id)
    | Lvar id -> Var (Ident.name id)
    | Lapply _ -> Call
    | _ -> Other
  in
  let rec go lets (lam : Lambda.t) =
    match lam with
    | Llet (Strict, id, arg, body) ->
      go ((id, (Ident.name id, value lets arg)) :: lets) body
    | Lprim {primitive = Pmakeblock _; args} ->
      (List.rev_map (fun (_, bound) -> bound) lets, List.map (value lets) args)
    | lam -> unexpected lam
  in
  match lam with
  | Lfunction {body} -> go [] body
  | lam -> unexpected lam

let many =
  "type many = {\n\
  \  mutable first: int,\n\
  \  second: int,\n\
  \  mutable third: int,\n\
  \  fourth: int,\n\
  \  mutable fifth: int,\n\
   }\n"

let assert_shape ~lets ~values (lam : Lambda.t) =
  let actual_lets, actual_values = copy_shape lam in
  let values_to_string values =
    String.concat ", " (List.map value_to_string values)
  in
  OUnit.assert_equal
    ~printer:(fun lets ->
      String.concat ", "
        (List.map
           (fun (name, value) -> name ^ " = " ^ value_to_string value)
           lets))
    lets actual_lets;
  OUnit.assert_equal ~printer:values_to_string values actual_values

let suites =
  __FILE__
  >::: [
         ( "mutable fields after two calls are bound in position order"
         >:: fun _ ->
           (* [first] comes before every override and is read in place;
              [third] and [fifth] follow a call and are bound before both *)
           binding_lambda ~name:"copy"
             (many
            ^ "let copy = (base: many, f, g) => {...base, second: f(), fourth: \
               g()}\n")
           |> assert_shape
                ~lets:
                  [
                    ("init", Var "base");
                    ("third", Read ("init", 2));
                    ("fifth", Read ("init", 4));
                  ]
                ~values:
                  [Read ("init", 0); Call; Bound "third"; Call; Bound "fifth"]
         );
         ( "a mutable field before the first call is read in place" >:: fun _ ->
           (* [second] is a constant, so only [fifth], after the call that
              writes [fourth], is bound *)
           binding_lambda ~name:"copy"
             (many
            ^ "let copy = (base: many, g) => {...base, second: 2, fourth: g()}\n"
             )
           |> assert_shape
                ~lets:[("init", Var "base"); ("fifth", Read ("init", 4))]
                ~values:
                  [
                    Read ("init", 0);
                    Other;
                    Read ("init", 2);
                    Call;
                    Bound "fifth";
                  ] );
         ( "overrides without side effects bind nothing" >:: fun _ ->
           binding_lambda ~name:"copy"
             (many
            ^ "let copy = (base: many) => {...base, second: base.first}\n")
           (* the override reads [base] itself; the copied fields read the
              [init] bound to it *)
           |> assert_shape
                ~lets:[("init", Var "base")]
                ~values:
                  [
                    Read ("init", 0);
                    Read ("base", 0);
                    Read ("init", 2);
                    Read ("init", 3);
                    Read ("init", 4);
                  ] );
         ( "immutable fields after a call are read in place" >:: fun _ ->
           binding_lambda ~name:"copy"
             "type i = {x: int, y: int}\n\
              let copy = (base: i, f) => {...base, x: f()}\n"
           |> assert_shape
                ~lets:[("init", Var "base")]
                ~values:[Call; Read ("init", 1)] );
       ]
