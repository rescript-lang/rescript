(* Items are taken in chunks, so that domains rarely contend for the counter,
   and a few items don't pay for starting domains. *)
let chunk_size = 64

let map f items =
  let items = Array.of_list items in
  let count = Array.length items in
  let domain_count =
    min
      (Compiler_execution_mode.configured_count ())
      ((count + chunk_size - 1) / chunk_size)
  in
  if domain_count <= 1 then Array.to_list (Array.map f items)
  else
    let results = Array.make count None in
    let next = Atomic.make 0 in
    let rec work () =
      let start = Atomic.fetch_and_add next chunk_size in
      if start < count then (
        for index = start to min count (start + chunk_size) - 1 do
          results.(index) <-
            Some
              (match f items.(index) with
              | value -> Ok value
              | exception exn -> Error (exn, Printexc.get_raw_backtrace ()))
        done;
        work ())
    in
    let domains = List.init (domain_count - 1) (fun _ -> Domain.spawn work) in
    work ();
    List.iter Domain.join domains;
    Array.to_list results
    |> List.map (function
      | Some (Ok value) -> value
      | Some (Error (exn, backtrace)) ->
        Printexc.raise_with_backtrace exn backtrace
      | None -> assert false)
