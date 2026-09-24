type t = {dependencies: string list; source: string option}

let read path =
  let descriptor = Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  let channel = Unix.in_channel_of_descr descriptor in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      (* The first four bytes give the exact length of the newline-delimited
         dependency block. The following source path can itself be relative;
         scanning until an absolute path would read the marshalled AST. *)
      let dependency_length = input_binary_int channel in
      let dependencies =
        really_input_string channel dependency_length
        |> String.split_on_char '\n'
        |> List.filter (fun name -> name <> "")
      in
      let source = try Some (input_line channel) with End_of_file -> None in
      {dependencies; source})
