open OUnit

let loc = Location.none

let debugger = Lambda.prim ~primitive:Pdebugger ~args:[] loc

let suites =
  __FILE__
  >::: [
         ( "shares an unresolved raise with unchanged arguments" >:: fun _ ->
           let lam = Lambda.staticraise 1 [Lambda.const (Lambda.const_int 1)] in
           assert_bool "the raise is physically unchanged"
             (Lam_pass_exits.simplify_exits lam == lam) );
         (* A negative exit is never inlined into its raise, and a handler that
            is neither a variable nor a constant is not substituted, so this
            catch survives the pass and must survive it unrebuilt. *)
         ( "shares a retained catch" >:: fun _ ->
           let lam =
             Lambda.staticcatch
               (Lambda.staticraise (-1) [])
               (-1, [])
               (Lambda.seq debugger Lambda.lambda_unit)
           in
           assert_bool "the catch is physically unchanged"
             (Lam_pass_exits.simplify_exits lam == lam) );
         ( "removes a catch whose exit is unused" >:: fun _ ->
           let body = Lambda.const (Lambda.const_int 1) in
           let lam = Lambda.staticcatch body (1, []) debugger in
           assert_bool "the unused handler is removed"
             (Lam_pass_exits.simplify_exits lam == body) );
       ]
