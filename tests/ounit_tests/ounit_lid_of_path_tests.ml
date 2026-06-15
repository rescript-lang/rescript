let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let ( =~ ) = OUnit.assert_equal

(* [Ctype.lid_of_path] must render applicative-functor application paths
   ([Path.Papply]) gracefully rather than aborting. Such paths are produced
   internally (applicative functors are on by default), but ReScript source
   cannot reference one, so the branch is unreachable end-to-end and is
   exercised here directly with a synthetic path. *)
let papply () =
  Path.Papply (Path.Pident (Ident.create "F"), Path.Pident (Ident.create "Arg"))

let suites =
  __FILE__
  >::: [
         (* F(Arg) renders to a single Lident, no crash *)
         ( __LOC__ >:: fun _ ->
           Ctype.lid_of_path (papply ()) =~ Longident.Lident "F(Arg)" );
         (* F(Arg).t renders to Ldot of that Lident *)
         ( __LOC__ >:: fun _ ->
           Ctype.lid_of_path (Path.Pdot (papply (), "t", 0))
           =~ Longident.Ldot (Longident.Lident "F(Arg)", "t") );
       ]
