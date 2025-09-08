let raises f =
  try
    ignore (f ());
    None
  with e -> Some e

let assert_raise_any ?msg (f : unit -> 'a) =
  let get_error_string () =
    let str =
      Format.sprintf "expected exception, but no exception was raised."
    in
    match msg with
    | None -> OUnit.assert_failure str
    | Some s -> OUnit.assert_failure (s ^ "\n" ^ str)
  in
  match raises f with
  | None -> OUnit.assert_failure (get_error_string ())
  | Some exn -> OUnit.assert_bool (Printexc.to_string exn) true

let time ?nums description f =
  match nums with
  | None ->
    let start = Unix.gettimeofday () in
    ignore @@ f ();
    let finish = Unix.gettimeofday () in
    Printf.printf "\n%s elapsed %f\n" description (finish -. start);
    flush stdout
  | Some nums ->
    let start = Unix.gettimeofday () in
    for _i = 0 to nums - 1 do
      ignore @@ f ()
    done;
    let finish = Unix.gettimeofday () in
    Printf.printf "\n%s elapsed %f\n" description (finish -. start);
    flush stdout
