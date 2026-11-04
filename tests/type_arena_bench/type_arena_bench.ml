(* Microbenchmark: Type_arena against today's boxed type nodes, on real
   descriptors from real signatures.

   One request = what Env and the type checker do to dependency types:
   unmarshal the runtime package's .cmi files, copy their signatures into
   the request (boxed: the real [Subst.signature Subst.identity], as
   [Env.read_pers_struct] does; arena: [Type_arena.Import.signature]), then
   [exprs] times: instantiate a random value type (copy of its generic nodes
   with [Tsubst] marks, restored afterwards), "unify" (link fresh type
   variables to new [int] nodes or to each other, 20 % of the time under a
   snapshot whose trail is half backtracked), run two marking traversals
   over the instance (an occur check and a level update), and keep 70 % of
   the instances until the request ends (the typed tree). The arena variant
   runs each request in its own store, freed at the end; with [-export] it
   also exports the kept instances to boxed nodes, as saving a .cmt would.

   The two variants run the same algorithm, written twice against the two
   APIs, with the same random sequence; the checksum (node count and level
   sum of the kept instances) shows they did the same work.

   Usage: type_arena_bench.exe (boxed|arena) DOMAINS REQUESTS EXPRS CMI_DIR
     [-export] *)

let generic_level = Btype.generic_level
let pivot_level = -1
let path_int = Predef.path_int

(* Per-domain id counter, as Compiler_request_state.type_node_id. *)
let id_key = Domain.DLS.new_key (fun () -> ref 0)
let new_id () =
  let r = Domain.DLS.get id_key in
  incr r;
  !r

module Rng = struct
  type t = {mutable s: int}
  let make seed = {s = (seed * 2654435761) + 1}
  let int r n =
    r.s <- (r.s * 25214903917) + 11;
    (r.s lsr 17) land 0x3fffffff mod n
end

let rec collect_values acc (sg : Types.signature) =
  List.fold_left
    (fun acc (item : Types.signature_item) ->
      match item with
      | Sig_value (_, v) -> v.val_type :: acc
      | Sig_module (_, {md_type = Mty_signature sg}, _) -> collect_values acc sg
      | _ -> acc)
    acc sg

let rec collect_arena_values acc (sg : Type_arena.Sig.signature) =
  List.fold_left
    (fun acc (item : Type_arena.Sig.signature_item) ->
      match item with
      | Sig_value (_, v) -> v.val_type :: acc
      | Sig_module (_, {md_type = Mty_signature sg}, _) ->
        collect_arena_values acc sg
      | _ -> acc)
    acc sg

module Boxed = struct
  open Types

  let rec repr t =
    match get_desc t with
    | Tlink t' -> repr t'
    | _ -> t

  let newty level desc = Transient_expr.create desc ~level ~id:(new_id ())

  let copy_desc f = function
    | Tvar _ -> Tvar None
    | Tarrow (args, ret) ->
      Tarrow (List.map (fun (a : arg) -> {a with typ = f a.typ}) args, f ret)
    | Ttuple l -> Ttuple (List.map f l)
    | Tconstr (p, l, _) -> Tconstr (p, List.map f l, ref Mnil)
    | Tobject t -> Tobject (f t)
    | Tfield fd -> Tfield {fd with typ = f fd.typ; rest = f fd.rest}
    | Tnil -> Tnil
    | Tvariant row ->
      let field = function
        | Rpresent o -> Rpresent (Option.map f o)
        | Reither (b1, l, b2, _) -> Reither (b1, List.map f l, b2, ref None)
        | Rabsent -> Rabsent
      in
      Tvariant
        {
          row with
          row_fields = List.map (fun (l, fi) -> (l, field fi)) row.row_fields;
          row_more = f row.row_more;
          row_name = Option.map (fun (p, l) -> (p, List.map f l)) row.row_name;
        }
    | Tunivar _ as d -> d
    | Tpoly (t, l) -> Tpoly (f t, List.map f l)
    | Tpackage (p, n, l) -> Tpackage (p, n, List.map f l)
    | Tlink _ | Tsubst _ -> assert false

  let iter_desc f = function
    | Tvar _ | Tnil | Tunivar _ -> ()
    | Tarrow (args, ret) ->
      List.iter (fun (a : arg) -> f a.typ) args;
      f ret
    | Ttuple l | Tconstr (_, l, _) | Tpackage (_, _, l) -> List.iter f l
    | Tobject t | Tlink t | Tsubst t -> f t
    | Tfield fd ->
      f fd.typ;
      f fd.rest
    | Tvariant row ->
      List.iter
        (fun (_, fi) ->
          match fi with
          | Rpresent (Some t) -> f t
          | Reither (_, l, _, _) -> List.iter f l
          | _ -> ())
        row.row_fields;
      f row.row_more
    | Tpoly (t, l) ->
      f t;
      List.iter f l

  (* Returns the instance and its fresh nodes. *)
  let instance level t =
    let saved = ref [] and copies = ref [] in
    let rec copy t =
      let t = repr t in
      match get_desc t with
      | Tsubst t' -> t'
      | d ->
        if get_level t <> generic_level then t
        else
          let t' = newty level Tnil in
          saved := (t, d) :: !saved;
          Transient_expr.set_desc t (Tsubst t');
          Transient_expr.set_desc t' (copy_desc copy d);
          copies := t' :: !copies;
          t'
    in
    let r = copy t in
    List.iter (fun (t, d) -> Transient_expr.set_desc t d) !saved;
    (r, !copies)

  (* Trail entries: node, old descriptor, old level. *)
  let unify rng level copies ~logged ~backtrack =
    let trail = ref [] in
    let set_desc t d =
      if logged then trail := (t, get_desc t, get_level t) :: !trail;
      Transient_expr.set_desc t d
    in
    let set_level t l =
      if logged then trail := (t, get_desc t, get_level t) :: !trail;
      Transient_expr.set_level t l
    in
    let last_var = ref None in
    List.iter
      (fun t ->
        match get_desc t with
        | Tvar _ ->
          let r = Rng.int rng 10 in
          if r < 5 then
            set_desc t (Tlink (newty level (Tconstr (path_int, [], ref Mnil))))
          else if r < 7 then
            match !last_var with
            | Some v when v != t ->
              let v = repr v in
              if v != t then set_desc t (Tlink v)
            | _ -> last_var := Some t
          else (
            set_level t (level - 1);
            last_var := Some t)
        | _ -> ())
      copies;
    if backtrack then
      List.iter
        (fun (t, d, l) ->
          Transient_expr.set_desc t d;
          Transient_expr.set_level t l)
        !trail

  (* Marking traversal (Btype.mark_type style): returns nodes and level sum. *)
  let rec mark t acc =
    let t = repr t in
    let l = get_level t in
    if l >= 0 then (
      Transient_expr.set_level t (pivot_level - l);
      let n, s = acc in
      let acc = ref (n + 1, s + l) in
      iter_desc (fun c -> acc := mark c !acc) (get_desc t);
      !acc)
    else acc

  let rec unmark t =
    let t = repr t in
    let l = get_level t in
    if l < 0 then (
      Transient_expr.set_level t (pivot_level - l);
      iter_desc unmark (get_desc t))

  let walk t =
    let r = mark t (0, 0) in
    unmark t;
    r

  let request ~exprs ~seed cmis =
    let t0 = Unix.gettimeofday () in
    let sigs =
      List.map
        (fun bytes ->
          let (_ : string), (sg : signature) = Marshal.from_string bytes 0 in
          Subst.signature Subst.identity sg)
        cmis
    in
    let values = Array.of_list (List.fold_left collect_values [] sigs) in
    let t1 = Unix.gettimeofday () in
    let rng = Rng.make seed in
    let kept = ref [] in
    for e = 1 to exprs do
      let level = 2 + (e mod 50) in
      let v = values.(Rng.int rng (Array.length values)) in
      let inst, copies = instance level v in
      let logged = Rng.int rng 5 = 0 in
      let backtrack = logged && Rng.int rng 2 = 0 in
      unify rng level copies ~logged ~backtrack;
      ignore (walk inst);
      ignore (walk inst);
      if Rng.int rng 10 < 7 then kept := inst :: !kept
    done;
    let n, s =
      List.fold_left
        (fun (n, s) t ->
          let n', s' = walk t in
          (n + n', s + s'))
        (0, 0) !kept
    in
    let t2 = Unix.gettimeofday () in
    (n, s, t1 -. t0, t2 -. t1, 0.)
end

module Arena = struct
  module A = Type_arena
  open A

  let rec repr t =
    let t' = link_target t in
    if t' == t then t else repr t'

  let newty level desc = A.newty ~level ~id:(new_id ()) desc

  let copy_desc f = function
    | Tvar _ -> Tvar None
    | Tarrow (args, ret) ->
      Tarrow (List.map (fun (a : arg) -> {a with typ = f a.typ}) args, f ret)
    | Ttuple l -> Ttuple (List.map f l)
    | Tconstr (p, l, _) -> Tconstr (p, List.map f l, ref Mnil)
    | Tobject t -> Tobject (f t)
    | Tfield fd -> Tfield {fd with typ = f fd.typ; rest = f fd.rest}
    | Tnil -> Tnil
    | Tvariant row ->
      let field = function
        | Rpresent o -> Rpresent (Option.map f o)
        | Reither (b1, l, b2, _) -> Reither (b1, List.map f l, b2, ref None)
        | Rabsent -> Rabsent
      in
      Tvariant
        {
          row with
          row_fields = List.map (fun (l, fi) -> (l, field fi)) row.row_fields;
          row_more = f row.row_more;
          row_name = Option.map (fun (p, l) -> (p, List.map f l)) row.row_name;
        }
    | Tunivar _ as d -> d
    | Tpoly (t, l) -> Tpoly (f t, List.map f l)
    | Tpackage (p, n, l) -> Tpackage (p, n, List.map f l)
    | Tlink _ | Tsubst _ -> assert false

  let iter_desc f = function
    | Tvar _ | Tnil | Tunivar _ -> ()
    | Tarrow (args, ret) ->
      List.iter (fun (a : arg) -> f a.typ) args;
      f ret
    | Ttuple l | Tconstr (_, l, _) | Tpackage (_, _, l) -> List.iter f l
    | Tobject t | Tlink t | Tsubst t -> f t
    | Tfield fd ->
      f fd.typ;
      f fd.rest
    | Tvariant row ->
      List.iter
        (fun (_, fi) ->
          match fi with
          | Rpresent (Some t) -> f t
          | Reither (_, l, _, _) -> List.iter f l
          | _ -> ())
        row.row_fields;
      f row.row_more
    | Tpoly (t, l) ->
      f t;
      List.iter f l

  let instance level t =
    let saved = ref [] and copies = ref [] in
    let rec copy t =
      let t = repr t in
      let s = subst_target t in
      if s != t then s
      else if get_level t <> generic_level then t
      else
        let d = get_desc t in
        let t' = newty level Tnil in
        saved := (t, d) :: !saved;
        set_desc t (Tsubst t');
        set_desc t' (copy_desc copy d);
        copies := t' :: !copies;
        t'
    in
    let r = copy t in
    List.iter (fun (t, d) -> set_desc t d) !saved;
    (r, !copies)

  let unify rng level copies ~logged ~backtrack =
    let trail = ref [] in
    let log t =
      if logged then trail := (t, get_desc t, get_level t) :: !trail
    in
    let last_var = ref None in
    List.iter
      (fun t ->
        match get_desc t with
        | Tvar _ ->
          let r = Rng.int rng 10 in
          if r < 5 then (
            log t;
            set_desc t (Tlink (newty level (Tconstr (path_int, [], ref Mnil)))))
          else if r < 7 then
            match !last_var with
            | Some v when v != t ->
              let v = repr v in
              if v != t then (
                log t;
                set_desc t (Tlink v))
            | _ -> last_var := Some t
          else (
            log t;
            set_level t (level - 1);
            last_var := Some t)
        | _ -> ())
      copies;
    if backtrack then
      List.iter
        (fun (t, d, l) ->
          set_desc t d;
          set_level t l)
        !trail

  let rec mark t acc =
    let t = repr t in
    let l = get_level t in
    if l >= 0 then (
      set_level t (pivot_level - l);
      let n, s = acc in
      let acc = ref (n + 1, s + l) in
      iter_desc (fun c -> acc := mark c !acc) (get_desc t);
      !acc)
    else acc

  let rec unmark t =
    let t = repr t in
    let l = get_level t in
    if l < 0 then (
      set_level t (pivot_level - l);
      iter_desc unmark (get_desc t))

  let walk t =
    let r = mark t (0, 0) in
    unmark t;
    r

  let request ~export ~exprs ~seed cmis =
    with_store (fun () ->
        let t0 = Unix.gettimeofday () in
        let sigs =
          List.map
            (fun bytes ->
              let (_ : string), (sg : Types.signature) =
                Marshal.from_string bytes 0
              in
              Import.run (fun c -> Import.signature c sg))
            cmis
        in
        let values =
          Array.of_list (List.fold_left collect_arena_values [] sigs)
        in
        let t1 = Unix.gettimeofday () in
        let rng = Rng.make seed in
        let kept = ref [] in
        for e = 1 to exprs do
          let level = 2 + (e mod 50) in
          let v = values.(Rng.int rng (Array.length values)) in
          let inst, copies = instance level v in
          let logged = Rng.int rng 5 = 0 in
          let backtrack = logged && Rng.int rng 2 = 0 in
          unify rng level copies ~logged ~backtrack;
          ignore (walk inst);
          ignore (walk inst);
          if Rng.int rng 10 < 7 then kept := inst :: !kept
        done;
        let n, s =
          List.fold_left
            (fun (n, s) t ->
              let n', s' = walk t in
              (n + n', s + s'))
            (0, 0) !kept
        in
        let t2 = Unix.gettimeofday () in
        (if export then
           let boxed =
             Export.run (fun e -> List.map (Export.type_expr e) !kept)
           in
           ignore (Sys.opaque_identity boxed));
        let t3 = Unix.gettimeofday () in
        (n, s, t1 -. t0, t2 -. t1, t3 -. t2))
end

let () =
  let variant = Sys.argv.(1) in
  let domains = int_of_string Sys.argv.(2) in
  let requests = int_of_string Sys.argv.(3) in
  let exprs = int_of_string Sys.argv.(4) in
  let dir = Sys.argv.(5) in
  let export = Array.length Sys.argv > 6 && Sys.argv.(6) = "-export" in
  let cmis =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".cmi")
    |> List.sort compare
    |> List.map (fun f ->
        let contents =
          In_channel.with_open_bin (Filename.concat dir f) In_channel.input_all
        in
        let start = String.length Config.cmi_magic_number in
        String.sub contents start
          (Marshal.total_size (Bytes.unsafe_of_string contents) start))
  in
  let worker k () =
    let n = ref 0 and s = ref 0 in
    let ti = ref 0. and te = ref 0. and tx = ref 0. in
    let g0 = Gc.counters () in
    for r = 1 to requests do
      let seed = (k * 7919) + r in
      let n', s', a, b, c =
        match variant with
        | "boxed" -> Boxed.request ~exprs ~seed cmis
        | "arena" -> Arena.request ~export ~exprs ~seed cmis
        | v -> failwith ("unknown variant " ^ v)
      in
      n := !n + n';
      s := !s + s';
      ti := !ti +. a;
      te := !te +. b;
      tx := !tx +. c
    done;
    let minor1, prom1, major1 = Gc.counters () in
    let minor0, prom0, major0 = g0 in
    (!n, !s, !ti, !te, !tx, minor1 -. minor0, prom1 -. prom0, major1 -. major0)
  in
  let gc0 = Gc.quick_stat () in
  let start = Unix.gettimeofday () in
  let results =
    if domains = 1 then [worker 0 ()]
    else
      List.init domains (fun k -> Domain.spawn (worker k))
      |> List.map Domain.join
  in
  let wall = Unix.gettimeofday () -. start in
  let gc1 = Gc.quick_stat () in
  let sum f = List.fold_left (fun a r -> a +. f r) 0. results in
  let checksum =
    List.fold_left
      (fun a (n, s, _, _, _, _, _, _) -> a + n + (s mod 1000003))
      0 results
  in
  Printf.printf
    "variant=%s%s domains=%d requests=%d exprs=%d wall=%.3f import=%.3f \
     exprs_s=%.3f export=%.3f minor_Mw=%.1f promoted_Mw=%.2f major_Mw=%.1f \
     minors=%d majors=%d checksum=%d\n"
    variant
    (if export then "+export" else "")
    domains requests exprs wall
    (sum (fun (_, _, a, _, _, _, _, _) -> a))
    (sum (fun (_, _, _, b, _, _, _, _) -> b))
    (sum (fun (_, _, _, _, c, _, _, _) -> c))
    (sum (fun (_, _, _, _, _, m, _, _) -> m) /. 1e6)
    (sum (fun (_, _, _, _, _, _, p, _) -> p) /. 1e6)
    (sum (fun (_, _, _, _, _, _, _, j) -> j) /. 1e6)
    (gc1.minor_collections - gc0.minor_collections)
    (gc1.major_collections - gc0.major_collections)
    checksum
