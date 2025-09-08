let with_file_as_pp filename f =
  Ext_pervasives.finally (open_out_bin filename) ~clean:close_out (fun chan ->
      let fmt = Format.formatter_of_out_channel chan in
      let v = f fmt in
      Format.pp_print_flush fmt ();
      v)

let failwithf ~loc fmt = Format.ksprintf (fun s -> failwith (loc ^ s)) fmt

let invalid_argf fmt = Format.ksprintf invalid_arg fmt
