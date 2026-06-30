let run () =
  Eio_main.run (fun env ->
      let fs = Eio.Stdenv.fs env in
      let stdin = Eio.Stdenv.stdin env in
      let stdout = Eio.Stdenv.stdout env in
      Rescript_language_server.listen ~input:stdin ~output:stdout ~fs)

let () =
  let usage =
    {|ReScript Language Server

Usage: rescript-language-server [options]

Options:

--stdio               Use stdio
-v, --version         Print version
-h, --help            Print help|}
  in

  match Sys.argv |> Array.to_list with
  | [_; "--stdio"] -> run ()
  | [_; ("-v" | "--version")] ->
    Rescript_language_server.version |> print_endline
  | [_; ("-h" | "--help")] -> print_endline usage
  | _ ->
    prerr_endline usage;
    exit 1
