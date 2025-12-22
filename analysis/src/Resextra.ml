let extrasPathFromCmtPath cmtPath =
  if Filename.check_suffix cmtPath ".cmti" then
    Filename.chop_extension cmtPath ^ ".resiextra"
  else if Filename.check_suffix cmtPath ".cmt" then
    Filename.chop_extension cmtPath ^ ".resextra"
  else cmtPath ^ ".resextra"

let loadActionsFromPackage ~path ~package =
  let uri = Uri.fromPath path in
  let moduleName =
    BuildSystem.namespacedName package.SharedTypes.namespace
      (FindFiles.getName path)
  in
  match Hashtbl.find_opt package.SharedTypes.pathsForModule moduleName with
  | None -> None
  | Some paths ->
    let cmtPath = SharedTypes.getCmtPath ~uri paths in
    let extrasPath = extrasPathFromCmtPath cmtPath in

    let tryLoad path =
      if Sys.file_exists path then
        try
          let ic = open_in_bin path in
          let v = (input_value ic : Actions.action list) in
          close_in ic;
          Some v
        with _ -> None
      else None
    in
    tryLoad extrasPath
