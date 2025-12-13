open DeadCommon

module PathMap = Map.Make (struct
  type t = DcePath.t

  let compare = Stdlib.compare
end)

let find_exception_from_decls (decls : Declarations.t) :
    DcePath.t -> Location.t option =
  let index =
    Declarations.fold
      (fun _pos (decl : Decl.t) acc ->
        match decl.Decl.declKind with
        | Exception ->
          (* Use raw decl positions: reference graph keys are raw positions. *)
          let loc : Location.t =
            {
              Location.loc_start = decl.pos;
              loc_end = decl.posEnd;
              loc_ghost = false;
            }
          in
          PathMap.add decl.path loc acc
        | _ -> acc)
      decls PathMap.empty
  in
  fun path -> PathMap.find_opt path index

let add ~config ~decls ~file ~path ~loc ~(strLoc : Location.t)
    ~(moduleLoc : Location.t) name =
  addDeclaration_ ~config ~decls ~file ~posEnd:strLoc.loc_end
    ~posStart:strLoc.loc_start ~declKind:Exception ~moduleLoc ~path ~loc name;
  name

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
