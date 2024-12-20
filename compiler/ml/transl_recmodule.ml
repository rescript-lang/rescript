open Types
open Typedtree
open Lambda

type error = Circular_dependency of Ident.t

exception Error of Location.t * error
(* Reorder bindings to honor dependencies.  *)

(* Utilities for compiling "module rec" definitions *)

let undefined_location loc =
  let fname, line, char = Location.get_pos_info loc.Location.loc_start in
  let fname = Filename.basename fname in
  Lconst
    (Const_block
       ( Lambda.Blk_tuple,
         [
           Const_base (Const_string (fname, None));
           Const_base (Const_int line);
           Const_base (Const_int char);
         ] ))

let cstr_const = 3

let cstr_non_const = 2

let init_shape modl =
  let add_name x id =
    Const_block (Blk_tuple, [x; Const_base (Const_string (Ident.name id, None))])
  in
  let module_tag_info : Lambda.tag_info =
    Blk_constructor {name = "Module"; num_nonconst = 2; tag = 0; attrs = []}
  in
  let value_tag_info : Lambda.tag_info =
    Blk_constructor {name = "value"; num_nonconst = 2; tag = 1; attrs = []}
  in
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
    | Mty_ident _ -> raise Not_found
    | Mty_alias _ ->
      Const_block (value_tag_info, [Const_pointer (0, Pt_module_alias)])
    | Mty_signature sg ->
      Const_block
        (module_tag_info, [Const_block (Blk_tuple, init_shape_struct env sg)])
    | Mty_functor _ -> raise Not_found
  (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
    | [] -> []
    | Sig_value (id, {val_kind = Val_reg; val_type = ty}) :: rem ->
      let is_function t =
        match t.desc with
        | Tarrow _ -> true
        | _ -> false
      in
      let init_v =
        match Ctype.expand_head env ty with
        | t when is_function t ->
          Const_pointer
            ( 0,
              Pt_constructor
                {
                  name = "Function";
                  const = cstr_const;
                  non_const = cstr_non_const;
                  attrs = [];
                } )
        | {desc = Tconstr (p, _, _)} when Path.same p Predef.path_lazy_t ->
          Const_pointer
            ( 1,
              Pt_constructor
                {
                  name = "Lazy";
                  const = cstr_const;
                  non_const = cstr_non_const;
                  attrs = [];
                } )
        | _ -> raise Not_found
      in
      add_name init_v id :: init_shape_struct env rem
    | Sig_value (_, {val_kind = Val_prim _}) :: rem -> init_shape_struct env rem
    | Sig_type (id, tdecl, _) :: rem ->
      init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext _ :: _ -> raise Not_found
    | Sig_module (id, md, _) :: rem ->
      add_name (init_shape_mod env md.md_type) id
      :: init_shape_struct
           (Env.add_module_declaration ~check:false id md env)
           rem
    | Sig_modtype (id, minfo) :: rem ->
      init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class _ :: _ -> assert false
    | Sig_class_type _ :: rem -> init_shape_struct env rem
  in
  try
    Some
      ( undefined_location modl.mod_loc,
        Lconst (init_shape_mod modl.mod_env modl.mod_type) )
  with Not_found -> None

type binding_status = Undefined | Inprogress | Defined

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id, _, _, _) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_, loc, _, _) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_, _, init, _) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_, _, _, rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let rec emit_binding i =
    match status.(i) with
    | Defined -> ()
    | Inprogress -> raise (Error (loc.(i), Circular_dependency id.(i)))
    | Undefined ->
      if init.(i) = None then (
        status.(i) <- Inprogress;
        for j = 0 to num_bindings - 1 do
          if IdentSet.mem id.(j) fv.(i) then emit_binding j
        done);
      res := (id.(i), init.(i), rhs.(i)) :: !res;
      status.(i) <- Defined
  in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
    | Undefined -> emit_binding i
    | Inprogress -> assert false
    | Defined -> ()
  done;
  List.rev !res

type t = Lambda.lambda

(* Utilities for compiling "module rec" definitions *)

type loc = t

type shape = t

type binding = Ident.t * (loc * shape) option * t

let eval_rec_bindings_aux (bindings : binding list) (cont : t) : t =
  let rec bind_inits args acc =
    match args with
    | [] -> acc
    | (_id, None, _rhs) :: rem -> bind_inits rem acc
    | (id, Some (loc, shape), _rhs) :: rem ->
      Lambda.Llet
        ( Strict,
          Pgenval,
          id,
          Lprim (Pinit_mod, [loc; shape], Location.none),
          bind_inits rem acc )
  in
  let rec bind_strict args acc =
    match args with
    | [] -> acc
    | (id, None, rhs) :: rem ->
      Lambda.Llet (Strict, Pgenval, id, rhs, bind_strict rem acc)
    | (_id, Some _, _rhs) :: rem -> bind_strict rem acc
  in
  let rec patch_forwards args =
    match args with
    | [] -> cont
    | (_id, None, _rhs) :: rem -> patch_forwards rem
    | (id, Some (_loc, shape), rhs) :: rem ->
      Lsequence
        ( Lprim (Pupdate_mod, [shape; Lvar id; rhs], Location.none),
          patch_forwards rem )
  in
  bind_inits bindings (bind_strict bindings (patch_forwards bindings))

(* collect all function declarations
    if the module creation is just a set of function declarations and consts,
    it is good
*)
let rec is_function_or_const_block (lam : Lambda.lambda) acc =
  match lam with
  | Lprim (Pmakeblock _, args, _) ->
    Ext_list.for_all args (fun x ->
        match x with
        | Lvar id -> Set_ident.mem acc id
        | Lfunction _ | Lconst _ -> true
        | _ -> false)
  | Llet (_, _, id, Lfunction _, cont) ->
    is_function_or_const_block cont (Set_ident.add acc id)
  | Lletrec (bindings, cont) -> (
    let rec aux_bindings bindings acc =
      match bindings with
      | [] -> Some acc
      | (id, Lambda.Lfunction _) :: rest ->
        aux_bindings rest (Set_ident.add acc id)
      | (_, _) :: _ -> None
    in
    match aux_bindings bindings acc with
    | None -> false
    | Some acc -> is_function_or_const_block cont acc)
  | Llet (_, _, _, Lconst _, cont) -> is_function_or_const_block cont acc
  | Llet (_, _, id1, Lvar id2, cont) when Set_ident.mem acc id2 ->
    is_function_or_const_block cont (Set_ident.add acc id1)
  | _ -> false

let is_strict_or_all_functions (xs : binding list) =
  Ext_list.for_all xs (fun (_, opt, rhs) ->
      match opt with
      | None -> true
      | _ -> is_function_or_const_block rhs Set_ident.empty)

(* Without such optimizations:

   {[
     module rec X : sig
       val f : int -> int
     end = struct
       let f x = x + 1
     end
     and Y : sig
       val f : int -> int
     end = struct
       let f x  = x + 2
     end
   ]}
   would generate such rawlambda:

   {[
     (setglobal Debug_tmp!
        (let
          (X/1002 = (#init_mod [0: "debug_tmp.ml" 15 6] [0: [0: [0: 0a "f"]]])
             Y/1003 = (#init_mod [0: "debug_tmp.ml" 20 6] [0: [0: [0: 0a "f"]]]))
            (seq
               (#update_mod [0: [0: [0: 0a "f"]]] X/1002
                  (let (f/1010 = (function x/1011 (+ x/1011 1)))
                       (makeblock 0/[f] f/1010)))
               (#update_mod [0: [0: [0: 0a "f"]]] Y/1003
                  (let (f/1012 = (function x/1013 (+ x/1013 2)))
                       (makeblock 0/[f] f/1012)))
               (makeblock 0/module/exports X/1002 Y/1003))))

   ]}
*)
let eval_rec_bindings (bindings : binding list) (cont : t) : t =
  if is_strict_or_all_functions bindings then
    Lambda.Lletrec (Ext_list.map bindings (fun (id, _, rhs) -> (id, rhs)), cont)
  else eval_rec_bindings_aux bindings cont

let compile_recmodule compile_rhs bindings cont =
  eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun {mb_id = id; mb_expr = modl; mb_loc = loc; _} ->
            (id, modl.mod_loc, init_shape modl, compile_rhs id modl loc))
          bindings))
    cont

let report_error ppf = function
  | Circular_dependency id ->
    Format.fprintf ppf
      "@[Cannot safely evaluate the definition@ of the recursively-defined \
       module %a@]"
      Printtyp.ident id

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
