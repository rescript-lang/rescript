open OUnit

(* Sharing is invisible to the generated output: a pass that rebuilds a term
   into an identical one produces the same JavaScript, so no snapshot can tell.
   This is the only place that notices. *)
let suites =
  __FILE__
  >::: [
         ( "shares an unchanged recursive group" >:: fun _ ->
           let recursive = Ident.create "recursive" in
           let lam =
             Lambda.letrec
               [(recursive, Lambda.var recursive)]
               (Lambda.var recursive)
           in
           assert_bool "the recursive group is physically unchanged"
             (Lam_pass_deep_flatten.deep_flatten lam == lam) );
       ]
