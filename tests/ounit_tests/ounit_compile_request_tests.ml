open OUnit

(* A valid value for each flag that takes one. *)
let sample_value = function
  | "-w" -> "+a-4"
  | "-warn-error" -> "-a+5"
  | "-bs-jsx" -> "4"
  | "-bs-source-map" -> "inline"
  | "-bs-source-map-sources-content" -> "true"
  | "-bs-package-output" -> "commonjs:lib/js:.js"
  | "-bs-gentype-module" -> "esmodule"
  | "-bs-gentype-module-resolution" -> "node16"
  | "-bs-gentype-shim" | "-bs-gentype-dep-path" -> "From=To"
  | "-color" -> "never"
  | "-enable-experimental" -> "LetUnwrap"
  | _ -> "value"

let flag_argv name =
  match Compile_request.takes_value name with
  | Some true -> [name; sample_value name]
  | Some false | None -> [name]

let read argv =
  match Compile_request.of_argv argv with
  | Ok parsed -> parsed
  | Error _ -> assert_failure ("could not read " ^ String.concat " " argv)

let is_command name = Compile_request.kind_of name = Some Command

(* Flags whose value the request does not keep. *)
let is_no_op name = name = "-unsafe" || name = "-bs-jsx-mode"

let suites =
  __FILE__
  >::: [
         ( "every flag survives to_argv and of_argv" >:: fun _ ->
           Compile_request.flag_names
           |> List.filter (fun name -> not (is_command name || is_no_op name))
           |> List.iter (fun name ->
               let {Compile_request.request} = read (flag_argv name) in
               assert_bool
                 (name ^ " changes the request")
                 (request <> Compile_request.empty);
               assert_bool
                 (name ^ " is read back from to_argv")
                 ((read (Compile_request.to_argv request)).request = request))
         );
         ( "all flags at once survive to_argv and of_argv" >:: fun _ ->
           let argv =
             Compile_request.flag_names
             |> List.filter (fun name -> not (is_command name))
             |> List.concat_map flag_argv
           in
           let {Compile_request.request} = read argv in
           assert_bool "the request is read back from to_argv"
             ((read (Compile_request.to_argv request)).request = request) );
         ( "repeated flags keep their order" >:: fun _ ->
           let argv =
             ["-open"; "B"; "-w"; "+a"; "-open"; "A"; "-warn-error"; "-a"]
           in
           let {Compile_request.request} = read argv in
           assert_equal ["B"; "A"] request.open_modules;
           assert_equal
             ["-open"; "B"; "-open"; "A"; "-w"; "+a"; "-warn-error"; "-a"]
             (Compile_request.to_argv request) );
         ( "commands and files are kept in order" >:: fun _ ->
           let {Compile_request.commands; arguments} =
             read ["a.res"; "-v"; "-bs-g"; "-format"; "b.res"; "c.res"]
           in
           assert_equal [Compile_request.Print_version; Format "b.res"] commands;
           assert_equal ["a.res"; "c.res"] arguments );
         ( "invalid values are errors" >:: fun _ ->
           let invalid argv =
             match Compile_request.of_argv argv with
             | Error (Invalid_flag_value _) -> true
             | Ok _ | Error _ -> false
           in
           List.iter
             (fun argv ->
               assert_bool
                 (String.concat " " argv ^ " is rejected")
                 (invalid argv))
             [
               ["-bs-jsx"; "3"];
               ["-bs-source-map"; "sometimes"];
               ["-w"; "+1.."];
               ["-color"; "red"];
               ["-bs-gentype-module"; "amd"];
               ["-enable-experimental"; "Unknown"];
               ["-bs-package-output"; "amd:lib"];
               ["-bs-package-name"; "a"; "-bs-package-name"; "b"];
             ] );
         ( "every flag has the kind its users rely on" >:: fun _ ->
           List.iter
             (fun (name, kind) ->
               assert_equal ~msg:name (Some kind) (Compile_request.kind_of name))
             [
               ("-bs-ns", Compile_request.Build_system);
               ("-bs-package-output", Build_system);
               ("-open", Public);
               ("-bs-jsx", Public);
               ("-w", Public);
               ("-bs-test-ast-conversion", Debug);
               ("-dtypedtree", Debug);
               ("-format", Command);
               ("-v", Command);
             ] );
         ( "a base request is extended" >:: fun _ ->
           let base = (read ["-open"; "A"; "-bs-g"]).request in
           match Compile_request.of_argv ~base ["-open"; "B"] with
           | Ok {request} ->
             assert_equal ["A"; "B"] request.open_modules;
             assert_bool "keeps -bs-g" request.codegen.debug
           | Error _ -> assert_failure "could not extend the request" );
       ]
