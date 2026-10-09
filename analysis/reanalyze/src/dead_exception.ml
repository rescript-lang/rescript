open Dead_common

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
      path_ |> Dce_path.from_path_t |> Dce_path.module_to_implementation
    in
    Cross_file_items.add_exception_ref cross_file ~exception_path ~loc_from
  else
    add_value_reference ~config ~refs ~file_deps ~binding
      ~add_file_reference:true ~loc_from ~loc_to
