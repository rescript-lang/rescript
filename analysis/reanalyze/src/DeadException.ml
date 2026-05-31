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
        match decl.Decl.decl_kind with
        | Exception ->
          (* Use raw decl positions: reference graph keys are raw positions. *)
          let loc : Location.t =
            {
              Location.loc_start = decl.pos;
              loc_end = decl.pos_end;
              loc_ghost = false;
            }
          in
          PathMap.add decl.path loc acc
        | _ -> acc)
      decls PathMap.empty
  in
  fun path -> PathMap.find_opt path index

let add ~config ~decls ~file ~path ~loc ~(str_loc : Location.t)
    ~(module_loc : Location.t) name =
  addDeclaration_ ~config ~decls ~file ~pos_end:str_loc.loc_end
    ~pos_start:str_loc.loc_start ~decl_kind:Exception ~module_loc ~path ~loc
    name;
  name

let mark_as_used ~config ~refs ~file_deps ~cross_file ~(binding : Location.t)
    ~(loc_from : Location.t) ~(loc_to : Location.t) path_ =
  if loc_to.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exception_path =
      path_ |> DcePath.from_path_t |> DcePath.module_to_implementation
    in
    CrossFileItems.add_exception_ref cross_file ~exception_path ~loc_from
  else
    add_value_reference ~config ~refs ~file_deps ~binding
      ~add_file_reference:true ~loc_from ~loc_to
