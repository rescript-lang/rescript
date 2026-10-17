open OUnit

let loc = Location.none

let compile ~path ~shadowed ~collision =
  Request_local.with_fresh (fun () ->
      (Clflags.current ()).dont_write_files := true;
      let component = Ident.create "Component" in
      let replacement = Ident.create "Replacement" in
      let provider = Ident.create "Provider" in
      let colliding = Ident.create "Provider$make" in
      let exports = if collision then [provider; colliding] else [provider] in
      let component_value value =
        Lambda.function_ ~loc ~attr:Lambda.default_function_attribute
          ~params:[Ident.create "props"]
          ~body:(Lambda.const (Lambda.const_string value))
      in
      let module_value =
        Lambda.prim ~primitive:(Pmakeblock (Blk_module ["make"]))
          ~args:[Lambda.var (if shadowed then replacement else component)]
          loc
      in
      let body =
        Lambda.let_ Lambda.Strict component
          (component_value "component")
          (Lambda.let_ Strict replacement
             (component_value "replacement")
             (Lambda.let_ Strict provider module_value
                (Lambda.let_ Strict colliding Lambda.lambda_unit
                   (Lambda.prim
                      ~primitive:(Pmakeblock (Blk_module_export exports))
                      ~args:(List.map Lambda.var exports)
                      loc))))
      in
      let result =
        Lam_compile_main.compile "" exports
          [{Lambda.binding = component; path; loc; explicit = false}]
          body
      in
      List.map Ident.name result.program.exports)

let suites =
  __FILE__
  >::: [
         ( "export a component through the existing hoisting backend"
         >:: fun _ ->
           assert_equal
             ["Provider"; "Provider$make"]
             (compile ~path:["Provider"; "make"] ~shadowed:false
                ~collision:false) );
         ( "skip automatic hoists hidden by a signature" >:: fun _ ->
           assert_equal ["Provider"]
             (compile ~path:["Hidden"; "make"] ~shadowed:false ~collision:false)
         );
         ( "do not hoist a replacement binding at the same source path"
         >:: fun _ ->
           assert_equal ["Provider"]
             (compile ~path:["Provider"; "make"] ~shadowed:true ~collision:false)
         );
         ( "preserve existing code when an automatic export name collides"
         >:: fun _ ->
           assert_equal
             ["Provider"; "Provider$make"]
             (compile ~path:["Provider"; "make"] ~shadowed:false ~collision:true)
         );
       ]
