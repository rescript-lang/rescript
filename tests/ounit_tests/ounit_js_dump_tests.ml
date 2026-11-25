let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

module E = Js_exp_make

let var name = E.js_global name

(* Printed as an operand, so at a level where a low-precedence expression
   needs parentheses, and as a statement. *)
let assert_tagged ~printed tag value =
  let part = E.str ~delim:DBackQuotes in
  let call = E.tagged_template tag [part "x"; part ""] [value] in
  OUnit.assert_equal ~printer:Fun.id printed
    (String.trim (Js_dump.string_of_expression call));
  OUnit.assert_equal ~printer:Fun.id (printed ^ ".length")
    (String.trim (Js_dump.string_of_expression (E.dot call "length")))

let assert_string = OUnit.assert_equal ~printer:(Printf.sprintf "%S")

let assert_escaped ~template text =
  assert_string template (Js_dump_string.escaped_to_template text)

let assert_plain ~template text =
  assert_string template (Js_dump_string.string_to_template text)

let suites =
  __FILE__
  >::: [
         ( "conditional tag" >:: fun _ ->
           assert_tagged ~printed:"(\n  c ? a : b\n)`x${1}`"
             (E.econd (var "c") (var "a") (var "b"))
             (E.int 1l) );
         ( "function tag" >:: fun _ ->
           assert_tagged ~printed:"(() => 1)`x${1}`"
             (E.ocaml_fun ~return_unit:false ~async:false ~one_unit_arg:false []
                [Js_stmt_make.return_stmt (E.int 1l)])
             (E.int 1l) );
         ( "awaited tag" >:: fun _ ->
           assert_tagged ~printed:"(await p)`x${1}`"
             {expression_desc = Await (var "p"); comment = None}
             (E.int 1l) );
         ( "conditional value" >:: fun _ ->
           assert_tagged ~printed:"tag`x${c ? 1 : 2}`" (var "tag")
             (E.econd (var "c") (E.int 1l) (E.int 2l)) );
         ( "member tag" >:: fun _ ->
           assert_tagged ~printed:"a.b`x${1}`" (E.dot (var "a") "b") (E.int 1l)
         );
         ( "called tag" >:: fun _ ->
           assert_tagged ~printed:"make(1)`x${1}`"
             (E.call ~info:Js_call_info.builtin_runtime_call (var "make")
                [E.int 1l])
             (E.int 1l) );
         ( "sequence value" >:: fun _ ->
           assert_tagged ~printed:"tag`x${f(), 1}`" (var "tag")
             (E.seq
                (E.call ~info:Js_call_info.builtin_runtime_call (var "f") [])
                (E.int 1l)) );
         ( "template text without escapes" >:: fun _ ->
           assert_escaped ~template:"" "";
           assert_escaped ~template:"a{b}\"'" "a{b}\"'" );
         ( "backslash in template text" >:: fun _ ->
           assert_escaped ~template:{|a\nb|} {|a\nb|};
           assert_escaped ~template:{|a\\|} {|a\\|};
           (* Not valid escaped text, but must not read past the end *)
           assert_escaped ~template:{|\|} {|\|};
           assert_escaped ~template:{|a\|} {|a\|} );
         ( "backtick in template text" >:: fun _ ->
           assert_escaped ~template:{|\`|} {|`|};
           assert_escaped ~template:{|a\`b\`|} {|a`b`|};
           assert_escaped ~template:{|\`|} {|\`|};
           assert_escaped ~template:{|\\\`|} {|\\`|} );
         ( "dollar without a brace in template text" >:: fun _ ->
           assert_escaped ~template:"$" "$";
           assert_escaped ~template:"a$" "a$";
           assert_escaped ~template:"$a" "$a";
           assert_escaped ~template:"a$b{c" "a$b{c";
           assert_escaped ~template:"$a{" "$a{" );
         ( "dollar and brace in template text" >:: fun _ ->
           assert_escaped ~template:{|\${|} "${";
           assert_escaped ~template:{|a\${|} "a${";
           assert_escaped ~template:{|a\${b}|} "a${b}";
           assert_escaped ~template:{|\${\${|} "${${";
           assert_escaped ~template:{|$\${$|} "$${$";
           assert_escaped ~template:{|\${\`$|} "${`$" );
         ( "escaped dollar and brace in template text" >:: fun _ ->
           assert_escaped ~template:{|\${|} {|\${|};
           assert_escaped ~template:{|\\\${|} {|\\${|};
           assert_escaped ~template:{|$\{|} {|$\{|} );
         ( "plain string as template text" >:: fun _ ->
           assert_plain ~template:"" "";
           assert_plain ~template:{|\\|} {|\|};
           assert_plain ~template:{|a\\b\n|} "a\\b\n";
           assert_plain ~template:{|\\\`$|} {|\`$|};
           assert_plain ~template:{|\\\${|} {|\${|};
           assert_plain ~template:{|\"$a|} {|"$a|} );
       ]
