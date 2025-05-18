let ( /+ ) = Filename.concat

let rec resolveNodeModulePath ~startPath name =
  let scope = Filename.dirname name in
  let name = Filename.basename name in
  let name =
    match scope.[0] with
    | '@' -> scope /+ name
    | _ -> name
  in
  let path = startPath /+ "node_modules" /+ name in
  if Files.exists path then Some path
  else if Filename.dirname startPath = startPath then None
  else resolveNodeModulePath ~startPath:(Filename.dirname startPath) name
