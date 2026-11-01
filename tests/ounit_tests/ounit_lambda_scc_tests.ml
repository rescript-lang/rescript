open OUnit

(* [bind_rec] splits a [let rec] group into its strongly connected
   components, binds each before the components that use it, and makes a
   component of one binding that doesn't refer to itself a plain [let].
   These tests read the bindings back as a list of [`Let x] and
   [`Rec [x; ...]], outermost first. *)

let ids names = List.map (fun name -> (name, Ident.create name)) names

let outside = Ident.create "outside"

(* A body that refers to the given bindings, in this order. *)
let refers (group : (string * Ident.t) list) names =
  Lambda.apply (Lambda.var outside)
    (List.map (fun name -> Lambda.var (List.assoc name group)) names)
    Location.none

let shape lam =
  let rec go acc (lam : Lambda.t) =
    match lam with
    | Llet (_, id, _, rest) -> go (`Let (Ident.name id) :: acc) rest
    | Lletrec (bindings, rest) ->
      let names =
        List.sort String.compare
          (List.map (fun (id, _) -> Ident.name id) bindings)
      in
      go (`Rec names :: acc) rest
    | _ -> List.rev acc
  in
  go [] lam

let bind group (bodies : (string * string list) list) =
  let bindings =
    List.map
      (fun (name, names) -> (List.assoc name group, refers group names))
      bodies
  in
  shape (Lambda_scc.bind_rec bindings (Lambda.var outside))

let index_of item items =
  let rec go i = function
    | [] -> assert_failure "missing binding"
    | x :: rest -> if x = item then i else go (i + 1) rest
  in
  go 0 items

let suites =
  __FILE__
  >::: [
         ( "a reference stays an edge when an earlier body had it too"
         >:: fun _ ->
           (* [x] refers to [c] first; [b]'s edge to [c] must survive that,
              or the cycle [b] <-> [c] falls apart into two plain lets *)
           let group = ids ["x"; "b"; "c"] in
           assert_equal
             ~printer:(fun _ -> "unexpected order")
             [`Rec ["b"; "c"]; `Let "x"]
             (bind group [("x", ["c"]); ("b", ["c"]); ("c", ["b"])]) );
         ( "a mixed group keeps cycles, self-recursion and plain lets apart"
         >:: fun _ ->
           let group = ids ["a"; "b"; "c"; "d"; "e"] in
           let result =
             bind group
               [
                 ("a", ["b"]);
                 (* a body referring to the same binding twice *)
                 ("b", ["a"; "a"]);
                 ("c", ["a"]);
                 ("d", ["d"]);
                 ("e", []);
               ]
           in
           assert_equal ~printer:string_of_int 4 (List.length result);
           List.iter
             (fun item -> ignore (index_of item result))
             [`Rec ["a"; "b"]; `Let "c"; `Rec ["d"]; `Let "e"];
           assert_bool "the cycle is bound before its user"
             (index_of (`Rec ["a"; "b"]) result < index_of (`Let "c") result) );
         ( "a body that refers to every binding" >:: fun _ ->
           let group = ids ["a"; "b"; "c"] in
           assert_equal
             ~printer:(fun _ -> "unexpected order")
             [`Let "b"; `Let "c"; `Rec ["a"]]
             (bind group [("a", ["a"; "b"; "c"]); ("b", []); ("c", ["b"])]) );
       ]
