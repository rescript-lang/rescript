(* For very small inputs, avoid domain overhead entirely. *)
let small_threshold = 10

(* Helper for parallel work distribution over a list. *)
let parallel_map ~domain_mgr ~items ~f =
  let len = List.length items in
  if len = 0 then []
  else
    let doms = Domain.recommended_domain_count () in
    (* If there's no parallel capacity or the list is small, run sequentially. *)
    if doms <= 1 || len < small_threshold then f items
    else
      let chunk_count = min doms len in
      let chunk_size = max 1 ((len + chunk_count - 1) / chunk_count) in
      let rec chunks_of size lst =
        if size <= 0 then []
        else
          match lst with
          | [] -> []
          | _ ->
            let rec take n acc rest =
              if n = 0 then (List.rev acc, rest)
              else
                match rest with
                | [] -> (List.rev acc, [])
                | x :: xs -> take (n - 1) (x :: acc) xs
            in
            let chunk, rest = take size [] lst in
            chunk :: chunks_of size rest
      in
      let chunks = chunks_of chunk_size items in
      Eio.Fiber.List.map
        (fun chunk -> Eio.Domain_manager.run domain_mgr (fun () -> f chunk))
        chunks
      |> List.concat
