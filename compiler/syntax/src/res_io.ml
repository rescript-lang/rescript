let read_file ~filename =
  let chan = open_in_bin filename in
  let content =
    try really_input_string chan (in_channel_length chan)
    with End_of_file -> ""
  in
  close_in_noerr chan;
  content

let read_stdin () =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_channel buf stdin 4096
     done
   with End_of_file -> ());
  Buffer.contents buf

let write_file ~filename ~contents:txt =
  let chan = open_out_bin filename in
  output_string chan txt;
  close_out chan
[@@raises Sys_error] [@@dead "+write_file"]
