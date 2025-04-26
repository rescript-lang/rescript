let loc_to_string (loc : Warnings.loc) : string =
  Format.sprintf "(%03d,%03d--%03d,%03d)" loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    loc.loc_end.pos_lnum
    (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)

let dump path =
  match Cmt.loadFullCmtFromPath ~path with
  | None -> failwith (Format.sprintf "Could not load cmt for %s" path)
  | Some full ->
    let open SharedTypes in
    let open SharedTypes.Stamps in
    let stamps = full.file.stamps |> getEntries in

    Printf.printf "Found %d stamps:\n\n" (List.length stamps);

    stamps
    |> List.sort (fun (_, a) (_, b) ->
           let aLoc = locOfKind a in
           let bLoc = locOfKind b in
           match compare aLoc.loc_start.pos_lnum bLoc.loc_start.pos_lnum with
           | 0 -> compare aLoc.loc_start.pos_cnum bLoc.loc_start.pos_cnum
           | c -> c)
    |> List.iter (fun (stamp, kind) ->
           match kind with
           | KType t ->
             Printf.printf "%d ktype        %s\n" stamp
               (loc_to_string t.extentLoc)
           | KValue t ->
             Printf.printf "%d kvalue       %s\n" stamp
               (loc_to_string t.extentLoc)
           | KModule t ->
             Printf.printf "%d kmodule      %s\n" stamp
               (loc_to_string t.extentLoc)
           | KConstructor t ->
             Printf.printf "%d kconstructor %s\n" stamp
               (loc_to_string t.extentLoc));

    (* Dump all locItems (typed nodes) *)
    let locItems =
      match full.extra with
      | {locItems} -> locItems
    in

    Printf.printf "\nFound %d locItems (typed nodes):\n\n"
      (List.length locItems);

    locItems
    |> List.sort (fun a b ->
           let aLoc = a.loc.Location.loc_start in
           let bLoc = b.loc.Location.loc_start in
           match compare aLoc.pos_lnum bLoc.pos_lnum with
           | 0 -> compare aLoc.pos_cnum bLoc.pos_cnum
           | c -> c)
    |> List.iter (fun {loc; locType} ->
           let locStr = loc_to_string loc in
           let kindStr = SharedTypes.locTypeToString locType in
           Printf.printf "%s %s\n" locStr kindStr)
