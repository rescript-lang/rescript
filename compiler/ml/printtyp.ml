(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types
open Btype
open Outcometree

type printing_context = {inlined_types: type_inlined_type list}

let print_res_poly_identifier : (string -> string) ref =
  ref (fun _ -> assert false)

(* Print a long identifier *)

let rec longident ppf = function
  | Lident s -> pp_print_string ppf s
  | Ldot (p, s) -> fprintf ppf "%a.%s" longident p s
  | Lapply (p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2

(* Print an identifier *)

let unique_names = ref Ident.empty

let ident_name id =
  try Ident.find_same id !unique_names with Not_found -> Ident.name id

let add_unique id =
  try ignore (Ident.find_same id !unique_names)
  with Not_found ->
    unique_names := Ident.add id (Ident.unique_toplevel_name id) !unique_names

let ident ppf id = pp_print_string ppf (ident_name id)

(* Print a path *)

let ident_pervasives = Ident.create_persistent "Pervasives"
let ident_stdlib = Ident.create_persistent "Stdlib"
let printing_env = ref Env.empty
let non_shadowed_pervasive_or_stdlib = function
  | Pdot (Pident id, s, _pos) as path -> (
    (Ident.same id ident_pervasives || Ident.same id ident_stdlib)
    &&
    try Path.same path (Env.lookup_type (Lident s) !printing_env)
    with Not_found -> true)
  | _ -> false

let rec tree_of_path = function
  | Pident id -> Oide_ident (ident_name id)
  | Pdot (_, s, _pos) as path when non_shadowed_pervasive_or_stdlib path ->
    Oide_ident s
  | Pdot (p, s, _pos) when String.starts_with (Path.name p) ~prefix:"Stdlib_" ->
    let path_name = Path.name p in
    let ident_without_stdlib_prefix =
      String.sub path_name 7 (String.length path_name - 7)
    in
    Oide_dot (Oide_ident ident_without_stdlib_prefix, s)
  | Pdot (p, s, _pos) -> Oide_dot (tree_of_path p, s)
  | Papply (p1, p2) -> Oide_apply (tree_of_path p1, tree_of_path p2)

let rec path ppf = function
  | Pident id -> ident ppf id
  | Pdot (_, s, _pos) as path when non_shadowed_pervasive_or_stdlib path ->
    pp_print_string ppf s
  | Pdot (p, s, _pos) ->
    path ppf p;
    pp_print_char ppf '.';
    pp_print_string ppf s
  | Papply (p1, p2) -> fprintf ppf "%a(%a)" path p1 path p2

let rec string_of_out_ident = function
  | Oide_ident s -> s
  | Oide_dot (id, s) -> String.concat "." [string_of_out_ident id; s]
  | Oide_apply (id1, id2) ->
    String.concat ""
      [string_of_out_ident id1; "("; string_of_out_ident id2; ")"]

let string_of_path p = string_of_out_ident (tree_of_path p)

(* Print a recursive annotation *)

let tree_of_rec = function
  | Trec_not -> Orec_not
  | Trec_first -> Orec_first
  | Trec_next -> Orec_next

(* Print a raw type expression, with sharing *)

let raw_list pr ppf = function
  | [] -> fprintf ppf "[]"
  | a :: l ->
    fprintf ppf "@[<1>[%a%t]@]" pr a (fun ppf ->
        List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let kind_vars = ref []
let kind_count = ref 0

let rec safe_kind_repr v = function
  | Fvar {contents = Some k} ->
    if List.memq k v then "Fvar loop" else safe_kind_repr (k :: v) k
  | Fvar r ->
    let vid =
      try List.assq r !kind_vars
      with Not_found ->
        let c =
          incr kind_count;
          !kind_count
        in
        kind_vars := (r, c) :: !kind_vars;
        c
    in
    Printf.sprintf "Fvar {None}@%d" vid
  | Fpresent -> "Fpresent"
  | Fabsent -> "Fabsent"

let rec safe_commu_repr v = function
  | Cok -> "Cok"
  | Cunknown -> "Cunknown"
  | Clink r ->
    if List.memq r v then "Clink loop" else safe_commu_repr (r :: v) !r

let rec safe_repr v = function
  | {desc = Tlink t} when not (List.memq t v) -> safe_repr (t :: v) t
  | t -> t

let rec list_of_memo = function
  | Mnil -> []
  | Mcons (_priv, p, _t1, _t2, rem) -> p :: list_of_memo rem
  | Mlink rem -> list_of_memo !rem

let print_name ppf = function
  | None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let string_of_label = function
  | Noloc.Nolabel -> ""
  | Labelled s -> s
  | Optional s -> "?" ^ s

let string_of_arity = function
  | None -> ""
  | Some arity -> string_of_int arity

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id
  else (
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;desc=@,%a}@]" ty.id ty.level raw_type_desc
      ty.desc)

and raw_type_list tl = raw_list raw_type tl

and raw_type_desc ppf = function
  | Tvar name -> fprintf ppf "Tvar %a" print_name name
  | Tarrow (l, t1, t2, c, a) ->
    fprintf ppf "@[<hov1>Tarrow(\"%s\",@,%a,@,%a,@,%s,@,%s)@]"
      (string_of_label l) raw_type t1 raw_type t2 (safe_commu_repr [] c)
      (string_of_arity a)
  | Ttuple tl -> fprintf ppf "@[<1>Ttuple@,%a@]" raw_type_list tl
  | Tconstr (p, tl, abbrev) ->
    fprintf ppf "@[<hov1>Tconstr(@,%a,@,%a,@,%a)@]" path p raw_type_list tl
      (raw_list path) (list_of_memo !abbrev)
  | Tobject (t, nm) ->
    fprintf ppf "@[<hov1>Tobject(@,%a,@,@[<1>ref%t@])@]" raw_type t (fun ppf ->
        match !nm with
        | None -> fprintf ppf " None"
        | Some (p, tl) ->
          fprintf ppf "(Some(@,%a,@,%a))" path p raw_type_list tl)
  | Tfield (f, k, t1, t2) ->
    fprintf ppf "@[<hov1>Tfield(@,%s,@,%s,@,%a,@;<0 -1>%a)@]" f
      (safe_kind_repr [] k) raw_type t1 raw_type t2
  | Tnil -> fprintf ppf "Tnil"
  | Tlink t -> fprintf ppf "@[<1>Tlink@,%a@]" raw_type t
  | Tsubst t -> fprintf ppf "@[<1>Tsubst@,%a@]" raw_type t
  | Tunivar name -> fprintf ppf "Tunivar %a" print_name name
  | Tpoly (t, tl) ->
    fprintf ppf "@[<hov1>Tpoly(@,%a,@,%a)@]" raw_type t raw_type_list tl
  | Tvariant row ->
    fprintf ppf
      "@[<hov1>{@[%s@,%a;@]@ @[%s@,%a;@]@ %s%B;@ %s%B;@ @[<1>%s%t@]}@]"
      "row_fields="
      (raw_list (fun ppf (l, f) -> fprintf ppf "@[%s,@ %a@]" l raw_field f))
      row.row_fields "row_more=" raw_type row.row_more "row_closed="
      row.row_closed "row_fixed=" row.row_fixed "row_name="
      (fun ppf ->
        match row.row_name with
        | None -> fprintf ppf "None"
        | Some (p, tl) -> fprintf ppf "Some(@,%a,@,%a)" path p raw_type_list tl)
  | Tpackage (p, _, tl) ->
    fprintf ppf "@[<hov1>Tpackage(@,%a@,%a)@]" path p raw_type_list tl

and raw_field ppf = function
  | Rpresent None -> fprintf ppf "Rpresent None"
  | Rpresent (Some t) -> fprintf ppf "@[<1>Rpresent(Some@,%a)@]" raw_type t
  | Reither (c, tl, m, e) ->
    fprintf ppf "@[<hov1>Reither(%B,@,%a,@,%B,@,@[<1>ref%t@])@]" c raw_type_list
      tl m (fun ppf ->
        match !e with
        | None -> fprintf ppf " None"
        | Some f -> fprintf ppf "@,@[<1>(%a)@]" raw_field f)
  | Rabsent -> fprintf ppf "Rabsent"

let raw_type_expr ppf t =
  visited := [];
  kind_vars := [];
  kind_count := 0;
  raw_type ppf t;
  visited := [];
  kind_vars := []

let () = Btype.print_raw := raw_type_expr

(* Normalize paths *)

type param_subst = Id | Nth of int | Map of int list

let is_nth = function
  | Nth _ -> true
  | _ -> false

let compose l1 = function
  | Id -> Map l1
  | Map l2 -> Map (List.map (List.nth l1) l2)
  | Nth n -> Nth (List.nth l1 n)

let apply_subst s1 tyl =
  if tyl = [] then []
    (* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity *)
  else
    match s1 with
    | Nth n1 -> [List.nth tyl n1]
    | Map l1 -> List.map (List.nth tyl) l1
    | Id -> tyl

type best_path = Paths of Path.t list | Best of Path.t

let printing_depth = ref 0
let printing_cont = ref ([] : Env.iter_cont list)
let printing_old = ref Env.empty
let printing_pers = ref Concr.empty
module PathMap = Map.Make (Path)
let printing_map = ref PathMap.empty

let same_type t t' = repr t == repr t'

let rec index l x =
  match l with
  | [] -> raise Not_found
  | a :: l -> if x == a then 0 else 1 + index l x

let rec uniq = function
  | [] -> true
  | a :: l -> (not (List.memq a l)) && uniq l

let rec normalize_type_path ?(cache = false) env p =
  try
    let params, ty, _ = Env.find_type_expansion p env in
    let params = List.map repr params in
    match repr ty with
    | {desc = Tconstr (p1, tyl, _)} ->
      let tyl = List.map repr tyl in
      if List.length params = List.length tyl && List.for_all2 ( == ) params tyl
      then normalize_type_path ~cache env p1
      else if cache || List.length params <= List.length tyl || not (uniq tyl)
      then (p, Id)
      else
        let l1 = List.map (index params) tyl in
        let p2, s2 = normalize_type_path ~cache env p1 in
        (p2, compose l1 s2)
    | ty -> (p, Nth (index params ty))
  with Not_found -> (Env.normalize_path None env p, Id)

let penalty s =
  if s <> "" && s.[0] = '_' then 10
  else
    try
      for i = 0 to String.length s - 2 do
        if s.[i] = '_' && s.[i + 1] = '_' then raise Exit
      done;
      1
    with Exit -> 10

let rec path_size = function
  | Pident id -> (penalty (Ident.name id), -Ident.binding_time id)
  | Pdot (p, _, _) ->
    let l, b = path_size p in
    (1 + l, b)
  | Papply (p1, p2) ->
    let l, b = path_size p1 in
    (l + fst (path_size p2), b)

let same_printing_env env =
  let used_pers = Env.used_persistent () in
  Env.same_types !printing_old env && Concr.equal !printing_pers used_pers

let set_printing_env env =
  printing_env := env;
  if !Clflags.real_paths || !printing_env == Env.empty || same_printing_env env
  then ()
  else (
    (* printf "Reset printing_map@."; *)
    printing_old := env;
    printing_pers := Env.used_persistent ();
    printing_map := PathMap.empty;
    printing_depth := 0;
    (* printf "Recompute printing_map.@."; *)
    let cont =
      Env.iter_types
        (fun p (p', _decl) ->
          let p1, s1 = normalize_type_path env p' ~cache:true in
          (* Format.eprintf "%a -> %a = %a@." path p path p' path p1 *)
          if s1 = Id then
            try
              let r = PathMap.find p1 !printing_map in
              match !r with
              | Paths l -> r := Paths (p :: l)
              | Best p' -> r := Paths [p; p']
              (* assert false *)
            with Not_found ->
              printing_map := PathMap.add p1 (ref (Paths [p])) !printing_map)
        env
    in
    printing_cont := [cont])

let wrap_printing_env env f =
  set_printing_env env;
  try_finally f (fun () -> set_printing_env Env.empty)

let wrap_printing_env env f = Env.without_cmis (wrap_printing_env env) f

let is_unambiguous path env =
  let l = Env.find_shadowed_types path env in
  List.exists (Path.same path) l
  ||
  (* concrete paths are ok *)
  match l with
  | [] -> true
  | p :: rem ->
    (* allow also coherent paths:  *)
    let normalize p = fst (normalize_type_path ~cache:true env p) in
    let p' = normalize p in
    List.for_all (fun p -> Path.same (normalize p) p') rem
    ||
    (* also allow repeatedly defining and opening (for toplevel) *)
    let id = lid_of_path p in
    List.for_all (fun p -> lid_of_path p = id) rem
    && Path.same p (Env.lookup_type id env)

let rec get_best_path r =
  match !r with
  | Best p' -> p'
  | Paths [] -> raise Not_found
  | Paths l ->
    r := Paths [];
    List.iter
      (fun p ->
        (* Format.eprintf "evaluating %a@." path p; *)
        match !r with
        | Best p' when path_size p >= path_size p' -> ()
        | _ -> if is_unambiguous p !printing_env then r := Best p)
        (* else Format.eprintf "%a ignored as ambiguous@." path p *)
      l;
    get_best_path r

let best_type_path p =
  if !Clflags.real_paths || !printing_env == Env.empty then (p, Id)
  else
    let p', s = normalize_type_path !printing_env p in
    let get_path () = get_best_path (PathMap.find p' !printing_map) in
    while
      !printing_cont <> []
      &&
      try fst (path_size (get_path ())) > !printing_depth
      with Not_found -> true
    do
      printing_cont := List.map snd (Env.run_iter_cont !printing_cont);
      incr printing_depth
    done;
    let p'' = try get_path () with Not_found -> p' in
    (* Format.eprintf "%a = %a -> %a@." path p path p' path p''; *)
    (p'', s)

(* Print a type expression *)

let names = ref ([] : (type_expr * string) list)
let name_counter = ref 0
let named_vars = ref ([] : string list)

let weak_counter = ref 1
let weak_var_map = ref TypeMap.empty
let named_weak_vars = ref StringSet.empty

let reset_names () =
  names := [];
  name_counter := 0;
  named_vars := []
let add_named_var ty =
  match ty.desc with
  | Tvar (Some name) | Tunivar (Some name) ->
    if List.mem name !named_vars then () else named_vars := name :: !named_vars
  | _ -> ()

let name_is_already_used name =
  List.mem name !named_vars
  || List.exists (fun (_, name') -> name = name') !names
  || StringSet.mem name !named_weak_vars

let rec new_name () =
  let name =
    if !name_counter < 26 then String.make 1 (Char.chr (97 + !name_counter))
    else
      String.make 1 (Char.chr (97 + (!name_counter mod 26)))
      ^ string_of_int (!name_counter / 26)
  in
  incr name_counter;
  if name_is_already_used name then new_name () else name

let rec new_weak_name ty () =
  let name = "weak" ^ string_of_int !weak_counter in
  incr weak_counter;
  if name_is_already_used name then new_weak_name ty ()
  else (
    named_weak_vars := StringSet.add name !named_weak_vars;
    weak_var_map := TypeMap.add ty name !weak_var_map;
    name)

let name_of_type name_generator t =
  (* We've already been through repr at this stage, so t is our representative
     of the union-find class. *)
  try List.assq t !names
  with Not_found -> (
    try TypeMap.find t !weak_var_map
    with Not_found ->
      let name =
        match t.desc with
        | Tvar (Some name) | Tunivar (Some name) ->
          (* Some part of the type we've already printed has assigned another
           * unification variable to that name. We want to keep the name, so try
           * adding a number until we find a name that's not taken. *)
          let current_name = ref name in
          let i = ref 0 in
          while List.exists (fun (_, name') -> !current_name = name') !names do
            current_name := name ^ string_of_int !i;
            i := !i + 1
          done;
          !current_name
        | _ ->
          (* No name available, create a new one *)
          name_generator ()
      in
      (* Exception for type declarations *)
      if name <> "_" then names := (t, name) :: !names;
      name)

let check_name_of_type t = ignore (name_of_type new_name t)

let remove_names tyl =
  let tyl = List.map repr tyl in
  names := Ext_list.filter !names (fun (ty, _) -> not (List.memq ty tyl))

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)
let delayed = ref ([] : type_expr list)

let add_delayed t = if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased ty = List.memq (proxy ty) !aliased
let add_alias ty =
  let px = proxy ty in
  if not (is_aliased px) then (
    aliased := px :: !aliased;
    add_named_var px)

let aliasable ty =
  match ty.desc with
  | Tvar _ | Tunivar _ | Tpoly _ -> false
  | Tconstr (p, _, _) -> not (is_nth (snd (best_type_path p)))
  | _ -> true

let namable_row row =
  row.row_name <> None
  && List.for_all
       (fun (_, f) ->
         match row_field_repr f with
         | Reither (c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
         | _ -> true)
       row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited && aliasable ty then add_alias px
  else
    let visited = px :: visited in
    match ty.desc with
    | Tvar _ -> add_named_var ty
    | Tarrow (_, ty1, ty2, _, _) ->
      mark_loops_rec visited ty1;
      mark_loops_rec visited ty2
    | Ttuple tyl -> List.iter (mark_loops_rec visited) tyl
    | Tconstr (p, tyl, _) ->
      let _p', s = best_type_path p in
      List.iter (mark_loops_rec visited) (apply_subst s tyl)
    | Tpackage (_, _, tyl) -> List.iter (mark_loops_rec visited) tyl
    | Tvariant row -> (
      if List.memq px !visited_objects then add_alias px
      else
        let row = row_repr row in
        if not (static_row row) then visited_objects := px :: !visited_objects;
        match row.row_name with
        | Some (_p, tyl) when namable_row row ->
          List.iter (mark_loops_rec visited) tyl
        | _ -> iter_row (mark_loops_rec visited) row)
    | Tobject (fi, nm) ->
      if List.memq px !visited_objects then add_alias px
      else (
        if opened_object ty then visited_objects := px :: !visited_objects;
        match !nm with
        | None ->
          let fields, _ = flatten_fields fi in
          List.iter
            (fun (_, kind, ty) ->
              if field_kind_repr kind = Fpresent then mark_loops_rec visited ty)
            fields
        | Some (_, l) -> List.iter (mark_loops_rec visited) (List.tl l))
    | Tfield (_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
      mark_loops_rec visited ty1;
      mark_loops_rec visited ty2
    | Tfield (_, _, _, ty2) -> mark_loops_rec visited ty2
    | Tnil -> ()
    | Tsubst ty -> mark_loops_rec visited ty
    | Tlink _ -> fatal_error "Printtyp.mark_loops_rec (2)"
    | Tpoly (ty, tyl) ->
      List.iter (fun t -> add_alias t) tyl;
      mark_loops_rec visited ty
    | Tunivar _ -> add_named_var ty

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty

let reset_loop_marks () =
  visited_objects := [];
  aliased := [];
  delayed := []

let reset () =
  unique_names := Ident.empty;
  reset_names ();
  reset_loop_marks ()

let reset_and_mark_loops ty =
  reset ();
  mark_loops ty

let reset_and_mark_loops_list tyl =
  reset ();
  List.iter mark_loops tyl

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        let ty = repr ty in
        if List.memq ty tyl then Btype.newgenty (Tsubst ty) :: tyl
        else ty :: tyl)
      [] tyl
  in
  List.rev params

let mark_loops_constructor_arguments = function
  | Cstr_tuple l -> List.iter mark_loops l
  | Cstr_record l -> List.iter (fun l -> mark_loops l.ld_type) l

let find_inlined_type name (printing_context : printing_context option) =
  match printing_context with
  | None -> None
  | Some {inlined_types} ->
    inlined_types
    |> List.find_opt (fun inlined_type ->
           match inlined_type with
           | Record {type_name} -> type_name = name)

(* Disabled in classic mode when printing an unification error *)

let rec tree_of_typexp ?(printing_context : printing_context option) sch ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names && not (List.memq px !delayed) then
    let mark = is_non_gen sch ty in
    let name = name_of_type (if mark then new_weak_name ty else new_name) px in
    Otyp_var (mark, name)
  else
    let pr_typ () =
      match ty.desc with
      | Tvar _ ->
        (*let lev =
          if is_non_gen sch ty then "/" ^ string_of_int ty.level else "" in*)
        let non_gen = is_non_gen sch ty in
        let name_gen = if non_gen then new_weak_name ty else new_name in
        Otyp_var (non_gen, name_of_type name_gen ty)
      | Tarrow (l, ty1, ty2, _, arity) ->
        let pr_arrow l ty1 ty2 =
          let lab = string_of_label l in
          let t1 =
            if is_optional l then
              match (repr ty1).desc with
              | Tconstr (path, [ty], _) when Path.same path Predef.path_option
                ->
                tree_of_typexp ?printing_context sch ty
              | _ -> Otyp_stuff "<hidden>"
            else tree_of_typexp ?printing_context sch ty1
          in
          (* should pass arity here? *)
          Otyp_arrow (lab, t1, tree_of_typexp ?printing_context sch ty2, arity)
        in
        pr_arrow l ty1 ty2
      | Ttuple tyl -> Otyp_tuple (tree_of_typlist ?printing_context sch tyl)
      | Tconstr (p, _tyl, _abbrev)
        when printing_context
             |> find_inlined_type (Path.name p)
             |> Option.is_some -> (
        match
          find_inlined_type (Path.name p) printing_context |> Option.get
        with
        | Record {labels} ->
          (* Print inlined records as actual inlined record structures, not a reference to the inlined type only. *)
          Otyp_record (List.map (tree_of_label ?printing_context) labels))
      | Tconstr (p, tyl, _abbrev) ->
        let p', s = best_type_path p in
        let tyl' = apply_subst s tyl in
        if is_nth s && not (tyl' = []) then
          tree_of_typexp ?printing_context sch (List.hd tyl')
        else
          Otyp_constr
            (tree_of_path p', tree_of_typlist ?printing_context sch tyl')
      | Tvariant row -> (
        let row = row_repr row in
        let fields =
          if row.row_closed then
            Ext_list.filter row.row_fields (fun (_, f) ->
                row_field_repr f <> Rabsent)
          else row.row_fields
        in
        let present =
          Ext_list.filter fields (fun (_, f) ->
              match row_field_repr f with
              | Rpresent _ -> true
              | _ -> false)
        in
        let all_present = List.length present = List.length fields in
        match row.row_name with
        | Some (p, tyl) when namable_row row ->
          let p', s = best_type_path p in
          let id = tree_of_path p' in
          let args =
            tree_of_typlist ?printing_context sch (apply_subst s tyl)
          in
          let out_variant =
            if is_nth s then List.hd args else Otyp_constr (id, args)
          in
          if row.row_closed && all_present then out_variant
          else
            let non_gen = is_non_gen sch px in
            let tags =
              if all_present then None else Some (List.map fst present)
            in
            Otyp_variant (non_gen, Ovar_typ out_variant, row.row_closed, tags)
        | _ ->
          let non_gen =
            (not (row.row_closed && all_present)) && is_non_gen sch px
          in
          let fields =
            List.map (tree_of_row_field ?printing_context sch) fields
          in
          let tags =
            if all_present then None else Some (List.map fst present)
          in
          Otyp_variant (non_gen, Ovar_fields fields, row.row_closed, tags))
      | Tobject (fi, nm) -> tree_of_typobject ?printing_context sch fi !nm
      | Tnil | Tfield _ -> tree_of_typobject ?printing_context sch ty None
      | Tsubst ty -> tree_of_typexp ?printing_context sch ty
      | Tlink _ -> fatal_error "Printtyp.tree_of_typexp"
      | Tpoly (ty, []) -> tree_of_typexp ?printing_context sch ty
      | Tpoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        let tyl = List.map repr tyl in
        if tyl = [] then tree_of_typexp ?printing_context sch ty
        else
          let old_delayed = !delayed in
          (* Make the names delayed, so that the real type is
             printed once when used as proxy *)
          List.iter add_delayed tyl;
          let tl = List.map (name_of_type new_name) tyl in
          let tr = Otyp_poly (tl, tree_of_typexp ?printing_context sch ty) in
          (* Forget names when we leave scope *)
          remove_names tyl;
          delayed := old_delayed;
          tr
      | Tunivar _ -> Otyp_var (false, name_of_type new_name ty)
      | Tpackage (p, n, tyl) ->
        let n =
          List.map (fun li -> String.concat "." (Longident.flatten li)) n
        in
        Otyp_module (Path.name p, n, tree_of_typlist ?printing_context sch tyl)
    in
    if List.memq px !delayed then
      delayed := Ext_list.filter !delayed (( != ) px);
    if is_aliased px && aliasable ty then (
      check_name_of_type px;
      Otyp_alias (pr_typ (), name_of_type new_name px))
    else pr_typ ()

and tree_of_row_field ?printing_context sch (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither (true, [], _, _) -> (l, false, [])
  | Rpresent (Some ty) -> (l, false, [tree_of_typexp ?printing_context sch ty])
  | Reither (c, tyl, _, _) ->
    if c (* contradiction: constant constructor with an argument *) then
      (l, true, tree_of_typlist ?printing_context sch tyl)
    else (l, false, tree_of_typlist ?printing_context sch tyl)
  | Rabsent -> (l, false, [] (* actually, an error *))

and tree_of_typlist ?printing_context sch tyl =
  List.map ((tree_of_typexp ?printing_context) sch) tyl

and tree_of_typobject ?printing_context sch fi nm =
  match nm with
  | None ->
    let pr_fields fi =
      let fields, rest = flatten_fields fi in
      let present_fields =
        List.fold_right
          (fun (n, k, t) l ->
            match field_kind_repr k with
            | Fpresent -> (n, t) :: l
            | _ -> l)
          fields []
      in
      let sorted_fields =
        List.sort (fun (n, _) (n', _) -> String.compare n n') present_fields
      in
      tree_of_typfields ?printing_context sch rest sorted_fields
    in
    let fields, rest = pr_fields fi in
    Otyp_object (fields, rest)
  | Some (p, ty :: tyl) ->
    let non_gen = is_non_gen sch (repr ty) in
    let args = tree_of_typlist ?printing_context sch tyl in
    let p', s = best_type_path p in
    assert (s = Id);
    Otyp_class (non_gen, tree_of_path p', args)
  | _ -> fatal_error "Printtyp.tree_of_typobject"

and is_non_gen sch ty = sch && is_Tvar ty && ty.level <> generic_level

and tree_of_typfields ?printing_context sch rest = function
  | [] ->
    let rest =
      match rest.desc with
      | Tvar _ | Tunivar _ -> Some (is_non_gen sch rest)
      | Tconstr _ -> Some false
      | Tnil -> None
      | _ -> fatal_error "typfields (1)"
    in
    ([], rest)
  | (s, t) :: l ->
    let field = (s, tree_of_typexp ?printing_context sch t) in
    let fields, rest = tree_of_typfields ?printing_context sch rest l in
    (field :: fields, rest)

and tree_of_type_decl id decl =
  reset ();

  let inlined_types = decl.type_inlined_types in
  let printing_context = {inlined_types} in
  let params = filter_params decl.type_params in

  (match decl.type_manifest with
  | Some ty ->
    let vars = free_variables ty in
    List.iter
      (function
        | {desc = Tvar (Some "_")} as ty ->
          if List.memq ty vars then ty.desc <- Tvar None
        | _ -> ())
      params
  | None -> ());

  List.iter add_alias params;
  List.iter mark_loops params;
  List.iter check_name_of_type (List.map proxy params);
  let ty_manifest =
    match decl.type_manifest with
    | None -> None
    | Some ty ->
      let ty =
        (* Special hack to hide variant name *)
        match repr ty with
        | {desc = Tvariant row} -> (
          let row = row_repr row in
          match row.row_name with
          | Some (Pident id', _) when Ident.same id id' ->
            newgenty (Tvariant {row with row_name = None})
          | _ -> ty)
        | _ -> ty
      in
      mark_loops ty;
      Some ty
  in
  (match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant cstrs ->
    List.iter
      (fun c ->
        mark_loops_constructor_arguments c.cd_args;
        may mark_loops c.cd_res)
      cstrs
  | Type_record (l, _rep) -> List.iter (fun l -> mark_loops l.ld_type) l
  | Type_open -> ());

  let type_param = function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let type_defined decl =
    let abstr =
      match decl.type_kind with
      | Type_abstract ->
        decl.type_manifest = None || decl.type_private = Private
      | Type_record _ -> decl.type_private = Private
      | Type_variant tll ->
        decl.type_private = Private
        || List.exists (fun cd -> cd.cd_res <> None) tll
      | Type_open -> decl.type_manifest = None
    in
    let vari =
      List.map2
        (fun ty v ->
          if abstr || not (is_Tvar (repr ty)) then Variance.get_upper v
          else (true, true))
        decl.type_params decl.type_variance
    in
    ( Ident.name id,
      List.map2
        (fun ty cocn ->
          (type_param (tree_of_typexp ~printing_context false ty), cocn))
        params vari )
  in
  let tree_of_manifest ty1 =
    match ty_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp ~printing_context false ty, ty1)
  in
  let name, args = type_defined decl in
  let constraints = tree_of_constraints ~printing_context params in
  let untagged = ref false in
  let ty, priv =
    match decl.type_kind with
    | Type_abstract -> (
      match ty_manifest with
      | None -> (Otyp_abstract, Public)
      | Some ty -> (tree_of_typexp ~printing_context false ty, decl.type_private)
      )
    | Type_variant cstrs ->
      untagged := Ast_untagged_variants.process_untagged decl.type_attributes;
      ( tree_of_manifest
          (Otyp_sum (List.map (tree_of_constructor ~printing_context) cstrs)),
        decl.type_private )
    | Type_record (lbls, _rep) ->
      ( tree_of_manifest
          (Otyp_record (List.map (tree_of_label ~printing_context) lbls)),
        decl.type_private )
    | Type_open -> (tree_of_manifest Otyp_open, decl.type_private)
  in
  let immediate = Builtin_attributes.immediate decl.type_attributes in
  {
    otype_name = name;
    otype_params = args;
    otype_type = ty;
    otype_private = priv;
    otype_immediate = immediate;
    otype_unboxed = decl.type_unboxed.unboxed || !untagged;
    otype_cstrs = constraints;
  }

and tree_of_constructor_arguments ?printing_context = function
  | Cstr_tuple l -> tree_of_typlist ?printing_context false l
  | Cstr_record l -> [Otyp_record (List.map tree_of_label l)]

and tree_of_constructor ?printing_context cd =
  let name = Ident.name cd.cd_id in
  let nullary = Ast_untagged_variants.is_nullary_variant cd.cd_args in
  let repr =
    if not nullary then None
    else
      match Ast_untagged_variants.process_tag_type cd.cd_attributes with
      | Some Null -> Some "@as(null)"
      | Some Undefined -> Some "@as(undefined)"
      | Some (String s) -> Some (Printf.sprintf "@as(%S)" s)
      | Some (Int i) -> Some (Printf.sprintf "@as(%d)" i)
      | Some (Float f) -> Some (Printf.sprintf "@as(%s)" f)
      | Some (Bool b) -> Some (Printf.sprintf "@as(%b)" b)
      | Some (BigInt s) -> Some (Printf.sprintf "@as(%sn)" s)
      | Some (Untagged _) (* should never happen *) | None -> None
  in
  let arg () = tree_of_constructor_arguments ?printing_context cd.cd_args in
  match cd.cd_res with
  | None -> (name, arg (), None, repr)
  | Some res ->
    let nm = !names in
    names := [];
    let ret = tree_of_typexp ?printing_context false res in
    let args = arg () in
    names := nm;
    (name, args, Some ret, repr)

and tree_of_label ?printing_context l =
  let opt = l.ld_optional in
  let typ =
    match l.ld_type.desc with
    | Tconstr (p, [t1], _) when opt && Path.same p Predef.path_option -> t1
    | _ -> l.ld_type
  in
  ( Ident.name l.ld_id,
    l.ld_mutable = Mutable,
    opt,
    tree_of_typexp ?printing_context false typ )

and tree_of_constraints ?printing_context params =
  List.fold_right
    (fun ty list ->
      let ty' = unalias ty in
      if proxy ty != proxy ty' then
        let tr = tree_of_typexp ?printing_context true ty in
        (tr, tree_of_typexp ?printing_context true ty') :: list
      else list)
    params []

let typexp ?printing_context sch ppf ty =
  !Oprint.out_type ppf (tree_of_typexp ?printing_context sch ty)

let type_expr ppf ty = typexp false ppf ty

and type_sch ppf ty = typexp true ppf ty

and type_scheme ppf ty =
  reset_and_mark_loops ty;
  typexp true ppf ty

(* Maxence *)
let type_scheme_max ?(b_reset_names = true) ppf ty =
  if b_reset_names then reset_names ();
  typexp true ppf ty
(* End Maxence *)

let tree_of_type_scheme ty =
  reset_and_mark_loops ty;
  tree_of_typexp true ty

(* Print one type declaration *)

let tree_of_type_declaration id decl rs =
  Osig_type (tree_of_type_decl id decl, tree_of_rec rs)

let type_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration id decl Trec_not)

let constructor_arguments ppf a =
  let tys = tree_of_constructor_arguments a in
  !Oprint.out_type ppf (Otyp_tuple tys)

(* Print an extension declaration *)

let tree_of_extension_constructor id ext es =
  reset ();
  let ty_name = Path.name ext.ext_type_path in
  let ty_params = filter_params ext.ext_type_params in
  List.iter add_alias ty_params;
  List.iter mark_loops ty_params;
  List.iter check_name_of_type (List.map proxy ty_params);
  mark_loops_constructor_arguments ext.ext_args;
  may mark_loops ext.ext_ret_type;
  let type_param = function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let ty_params =
    List.map (fun ty -> type_param (tree_of_typexp false ty)) ty_params
  in
  let name = Ident.name id in
  let args, ret =
    match ext.ext_ret_type with
    | None -> (tree_of_constructor_arguments ext.ext_args, None)
    | Some res ->
      let nm = !names in
      names := [];
      let ret = tree_of_typexp false res in
      let args = tree_of_constructor_arguments ext.ext_args in
      names := nm;
      (args, Some ret)
  in
  let ext =
    {
      oext_name = name;
      oext_type_name = ty_name;
      oext_type_params = ty_params;
      oext_args = args;
      oext_ret_type = ret;
      oext_repr = None;
      oext_private = ext.ext_private;
    }
  in
  let es =
    match es with
    | Text_first -> Oext_first
    | Text_next -> Oext_next
    | Text_exception -> Oext_exception
  in
  Osig_typext (ext, es)

let extension_constructor id ppf ext =
  !Oprint.out_sig_item ppf (tree_of_extension_constructor id ext Text_first)

(* Print a value declaration *)

let tree_of_value_description id decl =
  (* Format.eprintf "@[%a@]@." raw_type_expr decl.val_type; *)
  let id = Ident.name id in
  let ty = tree_of_type_scheme decl.val_type in
  let vd =
    {oval_name = id; oval_type = ty; oval_prims = []; oval_attributes = []}
  in
  let vd =
    match decl.val_kind with
    | Val_prim p -> Primitive.print p vd
    | _ -> vd
  in
  Osig_value vd

let value_description id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_value_description id decl)

(* Print a class type *)

(* Print a module type *)

let wrap_env fenv ftree arg =
  let env = !printing_env in
  set_printing_env (fenv env);
  let tree = ftree arg in
  set_printing_env env;
  tree

let filter_rem_sig item rem =
  match (item, rem) with
  | Sig_class_type _, tydecl1 :: tydecl2 :: rem -> ([tydecl1; tydecl2], rem)
  | _ -> ([], rem)

let dummy =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_private = Public;
    type_manifest = None;
    type_variance = [];
    type_newtype_level = None;
    type_loc = Location.none;
    type_attributes = [];
    type_immediate = false;
    type_unboxed = unboxed_false_default_false;
    type_inlined_types = [];
  }

let hide_rec_items = function
  | Sig_type (id, _decl, rs) :: rem
    when rs = Trec_first && not !Clflags.real_paths ->
    let rec get_ids = function
      | Sig_type (id, _, Trec_next) :: rem -> id :: get_ids rem
      | _ -> []
    in
    let ids = id :: get_ids rem in
    set_printing_env
      (List.fold_right
         (fun id -> Env.add_type ~check:false (Ident.rename id) dummy)
         ids !printing_env)
  | _ -> ()

let rec tree_of_modtype ?(ellipsis = false) = function
  | Mty_ident p -> Omty_ident (tree_of_path p)
  | Mty_signature sg ->
    Omty_signature (if ellipsis then [Osig_ellipsis] else tree_of_signature sg)
  | Mty_functor (param, ty_arg, ty_res) ->
    let res =
      match ty_arg with
      | None -> tree_of_modtype ~ellipsis ty_res
      | Some mty ->
        wrap_env
          (Env.add_module ~arg:true param mty)
          (tree_of_modtype ~ellipsis)
          ty_res
    in
    Omty_functor
      (Ident.name param, may_map (tree_of_modtype ~ellipsis:false) ty_arg, res)
  | Mty_alias (_, p) -> Omty_alias (tree_of_path p)

and tree_of_signature sg =
  wrap_env (fun env -> env) (tree_of_signature_rec !printing_env false) sg

and tree_of_signature_rec env' in_type_group = function
  | [] -> []
  | item :: rem as items ->
    let in_type_group =
      match (in_type_group, item) with
      | true, Sig_type (_, _, Trec_next) -> true
      | _, Sig_type (_, _, (Trec_not | Trec_first)) ->
        set_printing_env env';
        true
      | _ ->
        set_printing_env env';
        false
    in
    let sg, rem = filter_rem_sig item rem in
    hide_rec_items items;
    let trees = trees_of_sigitem item in
    let env' = Env.add_signature (item :: sg) env' in
    trees @ tree_of_signature_rec env' in_type_group rem

and trees_of_sigitem = function
  | Sig_value (id, decl) -> [tree_of_value_description id decl]
  | Sig_type (id, _, _) when is_row_name (Ident.name id) -> []
  | Sig_type (id, decl, rs) -> [tree_of_type_declaration id decl rs]
  | Sig_typext (id, ext, es) -> [tree_of_extension_constructor id ext es]
  | Sig_module (id, md, rs) ->
    let ellipsis =
      List.exists
        (function
          | {txt = "..."}, Parsetree.PStr [] -> true
          | _ -> false)
        md.md_attributes
    in
    [tree_of_module id md.md_type rs ~ellipsis]
  | Sig_modtype (id, decl) -> [tree_of_modtype_declaration id decl]
  | Sig_class () -> []
  | Sig_class_type () -> []

and tree_of_modtype_declaration id decl =
  let mty =
    match decl.mtd_type with
    | None -> Omty_abstract
    | Some mty -> tree_of_modtype mty
  in
  Osig_modtype (Ident.name id, mty)

and tree_of_module id ?ellipsis mty rs =
  Osig_module (Ident.name id, tree_of_modtype ?ellipsis mty, tree_of_rec rs)

let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
let modtype_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)

(* For the toplevel: merge with tree_of_signature? *)

(* Refresh weak variable map in the toplevel *)
let refresh_weak () =
  let refresh t name (m, s) =
    if is_non_gen true (repr t) then (TypeMap.add t name m, StringSet.add name s)
    else (m, s)
  in
  let m, s =
    TypeMap.fold refresh !weak_var_map (TypeMap.empty, StringSet.empty)
  in
  named_weak_vars := s;
  weak_var_map := m

let print_items showval env x =
  refresh_weak ();
  let rec print showval env = function
    | [] -> []
    | item :: rem as items ->
      let _sg, rem = filter_rem_sig item rem in
      hide_rec_items items;
      let trees = trees_of_sigitem item in
      List.map (fun d -> (d, showval env item)) trees @ print showval env rem
  in
  print showval env x

(* Print a signature body (used by -i when compiling a .ml) *)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg = fprintf ppf "%a" print_signature (tree_of_signature sg)

(* Print an unification error *)

let same_path t t' =
  let t = repr t and t' = repr t' in
  t == t'
  ||
  match (t.desc, t'.desc) with
  | Tconstr (p, tl, _), Tconstr (p', tl', _) -> (
    let p1, s1 = best_type_path p and p2, s2 = best_type_path p' in
    match (s1, s2) with
    | Nth n1, Nth n2 when n1 = n2 -> true
    | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
      let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
      List.length tl = List.length tl' && List.for_all2 same_type tl tl'
    | _ -> false)
  | _ -> false

let type_expansion t ppf t' =
  if same_path t t' then (
    add_delayed (proxy t);
    type_expr ppf t)
  else
    let t' = if proxy t == proxy t' then unalias t' else t' in
    fprintf ppf "@[<2>%a@ =@ %a@]" type_expr t type_expr t'

let type_path_expansion tp ppf tp' =
  if Path.same tp tp' then path ppf tp
  else fprintf ppf "@[<2>%a@ =@ %a@]" path tp path tp'

let rec trace fst txt ppf = function
  | (t1, t1') :: (t2, t2') :: rem ->
    if not fst then fprintf ppf "@,";
    fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@] %a" (type_expansion t1) t1' txt
      (type_expansion t2) t2' (trace false txt) rem
  | _ -> ()

let rec filter_trace keep_last = function
  | [(_, t1'); (_, t2')] when is_Tvar t1' || is_Tvar t2' -> []
  | (t1, t1') :: (t2, t2') :: rem ->
    let rem' = filter_trace keep_last rem in
    if
      is_constr_row ~allow_ident:true t1'
      || is_constr_row ~allow_ident:true t2'
      || (same_path t1 t1' && same_path t2 t2' && not (keep_last && rem' = []))
    then rem'
    else (t1, t1') :: (t2, t2') :: rem'
  | _ -> []

let rec type_path_list ppf = function
  | [(tp, tp')] -> type_path_expansion tp ppf tp'
  | (tp, tp') :: rem ->
    fprintf ppf "%a@;<2 0>%a" (type_path_expansion tp) tp' type_path_list rem
  | [] -> ()

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match repr t with
  | {desc = Tvariant row} as t when (row_repr row).row_name <> None ->
    newty2 t.level
      (Tvariant
         {
           (row_repr row) with
           row_name = None;
           row_more = newvar2 (row_more row).level;
         })
  | _ -> t

let prepare_expansion (t, t') =
  let t' = hide_variant_name t' in
  mark_loops t;
  if not (same_path t t') then mark_loops t';
  (t, t')

let may_prepare_expansion compact (t, t') =
  match (repr t').desc with
  | (Tvariant _ | Tobject _) when compact ->
    mark_loops t;
    (t, t)
  | _ -> prepare_expansion (t, t')

let print_tags ppf fields =
  match fields with
  | [] -> ()
  | (t, _) :: fields ->
    fprintf ppf "%s" (!print_res_poly_identifier t);
    List.iter
      (fun (t, _) -> fprintf ppf ",@ %s" (!print_res_poly_identifier t))
      fields

let has_explanation t3 t4 =
  match (t3.desc, t4.desc) with
  | Tfield _, (Tnil | Tconstr _)
  | (Tnil | Tconstr _), Tfield _
  | Tnil, Tconstr _
  | Tconstr _, Tnil
  | _, Tvar _
  | Tvar _, _
  | Tvariant _, Tvariant _ ->
    true
  | Tfield (l, _, _, {desc = Tnil}), Tfield (l', _, _, {desc = Tnil}) -> l = l'
  | _ -> false

let rec mismatch = function
  | (_, t) :: (_, t') :: rem -> (
    match mismatch rem with
    | Some _ as m -> m
    | None -> if has_explanation t t' then Some (t, t') else None)
  | [] -> None
  | _ -> assert false

let explanation unif t3 t4 ppf =
  match (t3.desc, t4.desc) with
  | Ttuple [], Tvar _ | Tvar _, Ttuple [] ->
    fprintf ppf "@,Self type cannot escape its class"
  | Tconstr (p, _, _), Tvar _ when unif && t4.level < Path.binding_time p ->
    fprintf ppf "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
      path p
  | Tvar _, Tconstr (p, _, _) when unif && t3.level < Path.binding_time p ->
    fprintf ppf "@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
      path p
  | Tvar _, Tunivar _ | Tunivar _, Tvar _ ->
    fprintf ppf "@,The universal variable %a would escape its scope" type_expr
      (if is_Tunivar t3 then t3 else t4)
  | Tvar _, _ | _, Tvar _ ->
    let t, t' = if is_Tvar t3 then (t3, t4) else (t4, t3) in
    if occur_in Env.empty t t' then
      fprintf ppf "@,@[<hov>The type variable %a occurs inside@ %a@]" type_expr
        t type_expr t'
    else
      fprintf ppf "@,@[<hov>This instance of %a is ambiguous:@ %s@]" type_expr
        t' "it would escape the scope of its equation"
  | Tfield (lab, _, _, _), _ when lab = dummy_method ->
    fprintf ppf "@,Self type cannot be unified with a closed object type"
  | _, Tfield (lab, _, _, _) when lab = dummy_method ->
    fprintf ppf "@,Self type cannot be unified with a closed object type"
  | Tfield (l, _, _, {desc = Tnil}), Tfield (l', _, _, {desc = Tnil})
    when l = l' ->
    fprintf ppf "@,Types for method %s are incompatible" l
  | (Tnil | Tconstr _), Tfield (l, _, _, _) ->
    fprintf ppf "@,@[The first object type has no field %s@]" l
  | Tfield (l, _, _, _), (Tnil | Tconstr _) ->
    fprintf ppf "@,@[The second object type has no field %s@]" l
  | Tnil, Tconstr _ | Tconstr _, Tnil ->
    fprintf ppf
      "@,@[The %s object type has an abstract row, it cannot be closed@]"
      (if t4.desc = Tnil then "first" else "second")
  | Tvariant row1, Tvariant row2 -> (
    let row1 = row_repr row1 and row2 = row_repr row2 in
    match
      (row1.row_fields, row1.row_closed, row2.row_fields, row2.row_closed)
    with
    | [], true, [], true ->
      fprintf ppf "@,These two variant types have no intersection"
    | [], true, (_ :: _ as fields), _ ->
      fprintf ppf
        "@,@[The first variant type does not allow tag(s)@ @[<hov>%a@]@]"
        print_tags fields
    | (_ :: _ as fields), _, [], true ->
      fprintf ppf
        "@,@[The second variant type does not allow tag(s)@ @[<hov>%a@]@]"
        print_tags fields
    | [(l1, _)], true, [(l2, _)], true when l1 = l2 ->
      fprintf ppf "@,Types for tag %s are incompatible"
        (!print_res_poly_identifier l1)
    | _ -> ())
  | _ -> ()

let warn_on_missing_def env ppf t =
  match t.desc with
  | Tconstr (p, _, _) -> (
    try ignore (Env.find_type p env : Types.type_declaration)
    with Not_found ->
      fprintf ppf
        "@,\
         @[%a is abstract because no corresponding cmi file was found in \
         path.@]"
        path p)
  | _ -> ()

let explanation unif mis ppf =
  match mis with
  | None -> ()
  | Some (t3, t4) -> explanation unif t3 t4 ppf

let ident_same_name id1 id2 =
  if Ident.equal id1 id2 && not (Ident.same id1 id2) then (
    add_unique id1;
    add_unique id2)

let rec path_same_name p1 p2 =
  match (p1, p2) with
  | Pident id1, Pident id2 -> ident_same_name id1 id2
  | Pdot (p1, s1, _), Pdot (p2, s2, _) when s1 = s2 -> path_same_name p1 p2
  | Papply (p1, p1'), Papply (p2, p2') ->
    path_same_name p1 p2;
    path_same_name p1' p2'
  | _ -> ()

let type_same_name t1 t2 =
  match ((repr t1).desc, (repr t2).desc) with
  | Tconstr (p1, _, _), Tconstr (p2, _, _) ->
    path_same_name (fst (best_type_path p1)) (fst (best_type_path p2))
  | _ -> ()

let rec trace_same_names = function
  | (t1, t1') :: (t2, t2') :: rem ->
    type_same_name t1 t2;
    type_same_name t1' t2';
    trace_same_names rem
  | _ -> ()

let unification_error env unif tr txt1 ppf txt2 =
  reset ();
  trace_same_names tr;
  let tr = List.map (fun (t, t') -> (t, hide_variant_name t')) tr in
  let mis = mismatch tr in
  match tr with
  | [] | _ :: [] -> assert false
  | t1 :: t2 :: tr -> (
    try
      let tr = filter_trace (mis = None) tr in
      let t1, t1' = may_prepare_expansion (tr = []) t1
      and t2, t2' = may_prepare_expansion (tr = []) t2 in
      let tr = List.map prepare_expansion tr in
      fprintf ppf "@[<v>@[%t@;<1 2>%a@ %t@;<1 2>%a@]%a%t@]" txt1
        (type_expansion t1) t1' txt2 (type_expansion t2) t2'
        (trace false "is not compatible with type")
        tr (explanation unif mis);
      if env <> Env.empty then (
        warn_on_missing_def env ppf t1;
        warn_on_missing_def env ppf t2)
    with exn -> raise exn)

let report_unification_error ppf env ?(unif = true) tr txt1 txt2 =
  wrap_printing_env env (fun () -> unification_error env unif tr txt1 ppf txt2)

let super_type_expansion ~tag t ppf t' =
  let tag = Format.String_tag tag in
  if same_path t t' then (
    Format.pp_open_stag ppf tag;
    type_expr ppf t;
    Format.pp_close_stag ppf ())
  else
    let t' = if proxy t == proxy t' then unalias t' else t' in
    fprintf ppf "@[<2>";
    Format.pp_open_stag ppf tag;
    fprintf ppf "%a" type_expr t;
    Format.pp_close_stag ppf ();
    fprintf ppf "@ @{<dim>(defined as@}@ ";
    Format.pp_open_stag ppf tag;
    fprintf ppf "%a" type_expr t';
    Format.pp_close_stag ppf ();
    fprintf ppf "@{<dim>)@}";
    fprintf ppf "@]"

let super_trace ppf =
  let rec super_trace first_report ppf = function
    | (t1, t1') :: (t2, t2') :: rem ->
      fprintf ppf "@,@,@[<v 2>";
      if first_report then fprintf ppf "The incompatible parts:@,"
      else fprintf ppf "Further expanded:@,";
      fprintf ppf "@[<hov>%a@ vs@ %a@]%a"
        (super_type_expansion ~tag:"error" t1)
        t1'
        (super_type_expansion ~tag:"info" t2)
        t2' (super_trace false) rem;
      fprintf ppf "@]"
    | _ -> ()
  in
  super_trace true ppf

let super_unification_error ?print_extra_info unif tr txt1 ppf txt2 =
  reset ();
  trace_same_names tr;
  let tr = List.map (fun (t, t') -> (t, hide_variant_name t')) tr in
  let mis = mismatch tr in
  match tr with
  | [] | _ :: [] -> assert false
  | t1 :: t2 :: tr -> (
    try
      let tr = filter_trace (mis = None) tr in
      let t1, t1' = may_prepare_expansion (tr = []) t1
      and t2, t2' = may_prepare_expansion (tr = []) t2 in
      let tr = List.map prepare_expansion tr in
      fprintf ppf "@[<v 0>@[<hov 2>%t@ %a@]@,@[<hov 2>%t@ %a@]%a%t%t@]" txt1
        (super_type_expansion ~tag:"error" t1) t1' txt2
        (super_type_expansion ~tag:"info" t2)
        t2' super_trace tr (explanation unif mis) (fun ppf ->
          match print_extra_info with
          | None -> ()
          | Some f -> f ppf t1 t2)
    with exn -> raise exn)

let super_report_unification_error ?print_extra_info ppf env ?(unif = true) tr
    txt1 txt2 =
  wrap_printing_env env (fun () ->
      super_unification_error ?print_extra_info unif tr txt1 ppf txt2)

let trace fst keep_last txt ppf tr =
  trace_same_names tr;
  try
    match tr with
    | t1 :: t2 :: tr' ->
      if fst then trace fst txt ppf (t1 :: t2 :: filter_trace keep_last tr')
      else trace fst txt ppf (filter_trace keep_last tr)
    | _ -> ()
  with exn -> raise exn

let report_subtyping_error ppf env tr1 txt1 tr2 =
  wrap_printing_env env (fun () ->
      reset ();
      let tr1 = List.map prepare_expansion tr1
      and tr2 = List.map prepare_expansion tr2 in
      fprintf ppf "@[<v>%a" (trace true (tr2 = []) txt1) tr1;
      if tr2 = [] then fprintf ppf "@]"
      else
        let mis = mismatch tr2 in
        fprintf ppf "%a%t@]"
          (trace false (mis = None) "is not compatible with type")
          tr2 (explanation true mis))

let report_ambiguous_type_error ppf env (tp0, tp0') tpl txt1 txt2 txt3 =
  wrap_printing_env env (fun () ->
      reset ();
      List.iter
        (fun (tp, tp') ->
          path_same_name tp0 tp;
          path_same_name tp0' tp')
        tpl;
      match tpl with
      | [] -> assert false
      | [(tp, tp')] ->
        fprintf ppf "@[%t@;<1 2>%a@ %t@;<1 2>%a@]" txt1 (type_path_expansion tp)
          tp' txt3 (type_path_expansion tp0) tp0'
      | _ ->
        fprintf ppf "@[%t@;<1 2>@[<hv>%a@]@ %t@;<1 2>%a@]" txt2 type_path_list
          tpl txt3 (type_path_expansion tp0) tp0')
