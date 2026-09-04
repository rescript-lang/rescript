open OUnit

let loc = Location.none
let x = Ident.create "x"
let y = Ident.create "y"
let debugger = Lambda.prim ~primitive:Pdebugger ~args:[] loc
let var = Lambda.var x

(* One node per constructor that has children. A leaf shares trivially, so it
   would pass either check below without exercising anything. *)
let nodes : (string * Lambda.t) list =
  [
    ("apply", Lambda.apply var [var] {ap_loc = loc; ap_inlined = Default_inline});
    ( "function",
      Lambda.function_ ~loc ~attr:Lambda.default_function_attribute ~params:[x]
        ~body:debugger );
    ("let", Lambda.let_ Strict y debugger var);
    ("letrec", Lambda.letrec [(y, debugger)] var);
    ("prim", Lambda.prim ~primitive:Pdebugger ~args:[var] loc);
    ( "switch",
      Lambda.switch var
        {
          sw_consts_full = false;
          sw_consts = [(Switch_int 0, debugger)];
          sw_blocks_full = false;
          sw_blocks = [];
          sw_failaction = Some debugger;
          sw_dispatch = Switch_direct;
        } );
    ("stringswitch", Lambda.stringswitch var [("a", debugger)] (Some debugger));
    ("staticraise", Lambda.staticraise 1 [var]);
    ( "staticcatch",
      Lambda.staticcatch (Lambda.staticraise 1 []) (1, []) debugger );
    ("trywith", Lambda.try_ debugger y var);
    ("ifthenelse", Lambda.if_ var debugger debugger);
    ("sequence", Lambda.seq debugger var);
    ("while", Lambda.while_ var debugger);
    ("for", Lambda.for_ y var var Upto debugger);
    ("for_of", Lambda.for_of y var debugger);
    ("for_await_of", Lambda.for_await_of y var debugger);
    ("assign", Lambda.assign x debugger);
  ]

(* Every optimization pass routes its "nothing to do here" case through
   [shallow_map_sharing], so an arm of it that stops sharing silently costs the
   property in all of them. That is invisible to generated output: breaking the
   [Lapply] and [Lswitch] arms leaves every fixture in the repository byte for
   byte identical. Add a node above when adding a Lambda constructor. *)
let suites =
  __FILE__
  >::: [
         ( "an unchanged child is not rebuilt" >:: fun _ ->
           List.iter
             (fun (name, node) ->
               assert_bool
                 (name ^ " should be handed back when nothing changed")
                 (Lambda_traverse.shallow_map_sharing (fun lam -> lam) node
                 == node))
             nodes );
         ( "a changed child is rebuilt" >:: fun _ ->
           (* Without this, a node whose children were never visited would pass
              the check above by doing nothing at all. *)
           List.iter
             (fun (name, node) ->
               assert_bool
                 (name ^ " should be rebuilt when a child changed")
                 (Lambda_traverse.shallow_map_sharing
                    (fun _ -> Lambda.const Lambda.const_unit)
                    node
                 != node))
             nodes );
       ]
