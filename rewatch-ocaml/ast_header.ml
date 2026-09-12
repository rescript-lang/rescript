type t = {dependencies: string list; source: string option}

let read path =
  let descriptor = Unix.openfile path [Unix.O_RDONLY] 0 in
  let channel = Unix.in_channel_of_descr descriptor in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      (try ignore (input_line channel) with End_of_file -> ());
      let rec loop dependencies =
        match input_line channel with
        | line ->
          let line = String.trim line in
          if line = "" then loop dependencies
          else if Filename.is_relative line then loop (line :: dependencies)
          else {dependencies = List.rev dependencies; source = Some line}
        | exception End_of_file ->
          {dependencies = List.rev dependencies; source = None}
      in
      loop [])
