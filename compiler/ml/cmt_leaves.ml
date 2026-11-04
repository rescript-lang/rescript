(* The leaves of the typed tree for [Typedtree_export] and [Typedtree_import]:
   type nodes, the declarations holding them and environments, converted in
   one [Type_arena] session per [.cmt]. Environments in a [.cmt] are reduced
   to their summaries ([Env.keep_only_summary]), so only the summary, the
   local constraints and the flags are converted. *)

module B = Types_boxed
module TB = Typedtree_boxed

module Export = struct
  module A = Type_arena.Export

  type t = {
    types: A.t;
    summaries: (Env.summary, TB.summary) Type_arena.Memo.t;
    (* By summary: environment records all hash alike. *)
    envs: (Env.summary, (Env.t * TB.env) list ref) Type_arena.Memo.t;
  }

  let run f =
    A.run (fun types ->
        f
          {
            types;
            summaries = Type_arena.Memo.create ();
            envs = Type_arena.Memo.create ();
          })

  let type_expr c t = A.type_expr c.types t
  let value_description c v = A.value_description c.types v
  let type_declaration c d = A.type_declaration c.types d
  let extension_constructor c e = A.extension_constructor c.types e
  let module_type c m = A.module_type c.types m
  let signature c s = A.signature c.types s
  let constructor_description c d = A.constructor_description c.types d
  let label_description c l = A.label_description c.types l
  let row_desc c r = A.row_desc c.types r
  let row_desc_ref c r = ref (row_desc c !r)

  let module_declaration c md = A.module_declaration c.types md
  let modtype_declaration c mtd = A.modtype_declaration c.types mtd

  let rec summary c (s : Env.summary) : TB.summary =
    Type_arena.Memo.memo c.summaries
      (fun (s : Env.summary) : TB.summary ->
        match s with
        | Env_empty -> Env_empty
        | Env_value (s, id, v) ->
          let s = summary c s in
          Env_value (s, id, value_description c v)
        | Env_type (s, id, d) ->
          let s = summary c s in
          Env_type (s, id, type_declaration c d)
        | Env_extension (s, id, e) ->
          let s = summary c s in
          Env_extension (s, id, extension_constructor c e)
        | Env_module (s, id, md) ->
          let s = summary c s in
          Env_module (s, id, module_declaration c md)
        | Env_modtype (s, id, mtd) ->
          let s = summary c s in
          Env_modtype (s, id, modtype_declaration c mtd)
        | Env_open (s, p) -> Env_open (summary c s, p)
        | Env_functor_arg (s, id) -> Env_functor_arg (summary c s, id)
        | Env_constraints (s, cstrs) ->
          let s = summary c s in
          Env_constraints (s, Env.Path_map.map (type_declaration c) cstrs)
        | Env_copy_types (s, names) -> Env_copy_types (summary c s, names))
      s

  let env c (e : Env.t) : TB.env =
    let s, constraints, flags = Env.summary_parts e in
    let known = Type_arena.Memo.memo c.envs (fun _ -> ref []) s in
    match List.assq_opt e !known with
    | Some e' -> e'
    | None ->
      let s' = summary c s in
      let constraints' = Env.Path_map.map (type_declaration c) constraints in
      let e' =
        Obj.repr
          (Env.of_summary_parts
             (Obj.magic (s' : TB.summary) : Env.summary)
             (Obj.magic (constraints' : B.type_declaration Env.Path_map.t)
               : Types.type_declaration Env.Path_map.t)
             flags)
      in
      known := (e, e') :: !known;
      e'
end

module Import = struct
  module A = Type_arena.Import

  type t = {
    types: A.t;
    summaries: (TB.summary, Env.summary) Type_arena.Memo.t;
    envs: (TB.summary, (TB.env * Env.t) list ref) Type_arena.Memo.t;
  }

  let run f =
    A.run (fun types ->
        f
          {
            types;
            summaries = Type_arena.Memo.create ();
            envs = Type_arena.Memo.create ();
          })

  let type_expr c t = A.type_expr c.types t
  let value_description c v = A.value_description c.types v
  let type_declaration c d = A.type_declaration c.types d
  let extension_constructor c e = A.extension_constructor c.types e
  let module_type c m = A.module_type c.types m
  let signature c s = A.signature c.types s
  let constructor_description c d = A.constructor_description c.types d
  let label_description c l = A.label_description c.types l
  let row_desc c r = A.row_desc c.types r
  let row_desc_ref c r = ref (row_desc c !r)

  let module_declaration c md = A.module_declaration c.types md
  let modtype_declaration c mtd = A.modtype_declaration c.types mtd

  let rec summary c (s : TB.summary) : Env.summary =
    Type_arena.Memo.memo c.summaries
      (fun (s : TB.summary) : Env.summary ->
        match s with
        | Env_empty -> Env_empty
        | Env_value (s, id, v) ->
          let s = summary c s in
          Env_value (s, id, value_description c v)
        | Env_type (s, id, d) ->
          let s = summary c s in
          Env_type (s, id, type_declaration c d)
        | Env_extension (s, id, e) ->
          let s = summary c s in
          Env_extension (s, id, extension_constructor c e)
        | Env_module (s, id, md) ->
          let s = summary c s in
          Env_module (s, id, module_declaration c md)
        | Env_modtype (s, id, mtd) ->
          let s = summary c s in
          Env_modtype (s, id, modtype_declaration c mtd)
        | Env_open (s, p) -> Env_open (summary c s, p)
        | Env_functor_arg (s, id) -> Env_functor_arg (summary c s, id)
        | Env_constraints (s, cstrs) ->
          let s = summary c s in
          Env_constraints (s, Env.Path_map.map (type_declaration c) cstrs)
        | Env_copy_types (s, names) -> Env_copy_types (summary c s, names))
      s

  let env c (e : TB.env) : Env.t =
    let s, constraints, flags = Env.summary_parts (Obj.obj e : Env.t) in
    let s = (Obj.magic (s : Env.summary) : TB.summary) in
    let constraints =
      (Obj.magic (constraints : Types.type_declaration Env.Path_map.t)
        : B.type_declaration Env.Path_map.t)
    in
    let known = Type_arena.Memo.memo c.envs (fun _ -> ref []) s in
    match List.assq_opt e !known with
    | Some e' -> e'
    | None ->
      let e' =
        Env.of_summary_parts (summary c s)
          (Env.Path_map.map (type_declaration c) constraints)
          flags
      in
      known := (e, e') :: !known;
      e'
end
