open DeadCommon

let declarations = Hashtbl.create 1

let add ~config ~decls ~file ~path ~loc ~(strLoc : Location.t)
    ~(moduleLoc : Location.t) name =
  let exceptionPath = name :: path in
  Hashtbl.add declarations exceptionPath loc;
  name
  |> addDeclaration_ ~config ~decls ~file ~posEnd:strLoc.loc_end
       ~posStart:strLoc.loc_start ~declKind:Exception ~moduleLoc ~path ~loc

let find_exception path = Hashtbl.find_opt declarations path

let markAsUsed ~config ~refs ~file_deps ~cross_file ~(binding : Location.t)
    ~(locFrom : Location.t) ~(locTo : Location.t) path_ =
  if locTo.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exceptionPath =
      path_ |> DcePath.fromPathT |> DcePath.moduleToImplementation
    in
    CrossFileItems.add_exception_ref cross_file ~exception_path:exceptionPath
      ~loc_from:locFrom
  else
    addValueReference ~config ~refs ~file_deps ~binding ~addFileReference:true
      ~locFrom ~locTo
