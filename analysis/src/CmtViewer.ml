let loc_to_string (loc : Warnings.loc) : string =
  Format.sprintf "(%d,%d--%d,%d)" loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    loc.loc_end.pos_lnum
    (loc.loc_end.pos_cnum - loc.loc_end.pos_bol)

let dump path =
  match Cmt.loadFullCmtFromPath ~path with
  | None -> failwith (Format.sprintf "Could not load cmt for %s" path)
  | Some full ->
    let open SharedTypes.Stamps in
    let stamps = full.file.stamps |> getEntries in

    Printf.printf "Found %d stamps:\n\n" (List.length stamps);

    stamps
    |> List.sort (fun (_, a) (_, b) ->
           let aLoc = SharedTypes.Stamps.locOfKind a in
           let bLoc = SharedTypes.Stamps.locOfKind b in
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
               (loc_to_string t.extentLoc))
