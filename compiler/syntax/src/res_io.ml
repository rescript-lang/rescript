let read_file ~filename =
  let chan = open_in_bin filename in
  let content =
    try really_input_string chan (in_channel_length chan)
    with End_of_file -> ""
  in
  close_in_noerr chan;
  content

let write_file ~filename ~contents:txt =
  let chan = open_out_bin filename in
  output_string chan txt;
  close_out chan
[@@raises Sys_error]
