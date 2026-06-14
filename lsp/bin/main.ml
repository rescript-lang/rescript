let () =
  Eio_main.run (fun env ->
      let fs = Eio.Stdenv.fs env in
      let stdin = Eio.Stdenv.stdin env in
      let stdout = Eio.Stdenv.stdout env in
      Rescript_language_server.listen ~input:stdin ~output:stdout ~fs)
