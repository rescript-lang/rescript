(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

type t = {mutable stamp: int; name: string; mutable flags: int}

let[@inlnie] max (x : int) y = if x >= y then x else y
let global_flag = 1
let predef_exn_flag = 2

(* A stamp of 0 denotes a persistent identifier *)

type counter_state = {
  mutable currentstamp: int;
  mutable reinit_level: int;
  mutable allocation_capture: t list ref option;
}

let counter_key =
  Domain.DLS.new_key (fun () ->
      {currentstamp = 0; reinit_level = -1; allocation_capture = None})

let counter () = Domain.DLS.get counter_key

(* Captured after the compiler's predefined identifiers and static module
   values have been initialized. Requests may reuse stamps only within their
   isolated environments. *)
let request_baseline = Atomic.make None
let capture_request_baseline () =
  Atomic.set request_baseline (Some (counter ()).currentstamp)

let with_fresh action =
  let previous = counter () in
  let baseline =
    match Atomic.get request_baseline with
    | Some baseline -> baseline
    | None -> previous.currentstamp
  in
  Domain.DLS.set counter_key
    {
      currentstamp = baseline;
      reinit_level = baseline;
      allocation_capture = None;
    };
  Fun.protect action ~finally:(fun () -> Domain.DLS.set counter_key previous)

let record_allocation state id =
  match state.allocation_capture with
  | Some captured -> captured := id :: !captured
  | None -> ()

let create s =
  let state = counter () in
  state.currentstamp <- state.currentstamp + 1;
  let id = {name = s; stamp = state.currentstamp; flags = 0} in
  record_allocation state id;
  id

let create_predef_exn s =
  let state = counter () in
  state.currentstamp <- state.currentstamp + 1;
  let id = {name = s; stamp = state.currentstamp; flags = predef_exn_flag} in
  record_allocation state id;
  id

let create_persistent s = {name = s; stamp = 0; flags = global_flag}

let rename i =
  let state = counter () in
  state.currentstamp <- state.currentstamp + 1;
  let id = {i with stamp = state.currentstamp} in
  record_allocation state id;
  id

let name i = i.name

let unique_name i = i.name ^ "_" ^ string_of_int i.stamp

let unique_toplevel_name i = i.name ^ "/" ^ string_of_int i.stamp

let persistent i = i.stamp = 0

let equal i1 i2 = i1.name = i2.name

let same ({stamp; name} : t) i2 =
  if stamp <> 0 then stamp = i2.stamp else i2.stamp = 0 && name = i2.name

let binding_time i = i.stamp

let current_time () = (counter ()).currentstamp
let with_allocation_capture action =
  let state = counter () in
  let previous = state.allocation_capture in
  let captured = ref [] in
  state.allocation_capture <- Some captured;
  Fun.protect
    (fun () ->
      let result = action () in
      (result, Array.of_list (List.rev !captured)))
    ~finally:(fun () ->
      state.allocation_capture <- previous;
      match previous with
      | Some outer -> outer := !captured @ !outer
      | None -> ())
let set_current_time t =
  let state = counter () in
  state.currentstamp <- max state.currentstamp t

let reinit () =
  let state = counter () in
  if state.reinit_level < 0 then state.reinit_level <- state.currentstamp
  else state.currentstamp <- state.reinit_level

let hide i = {i with stamp = -1}

let make_global i = i.flags <- i.flags lor global_flag

let global i = i.flags land global_flag <> 0

let is_predef_exn i = i.flags land predef_exn_flag <> 0

let print ppf i =
  match i.stamp with
  | 0 -> fprintf ppf "%s!" i.name
  | -1 -> fprintf ppf "%s#" i.name
  | n -> fprintf ppf "%s/%i%s" i.name n (if global i then "g" else "")

type 'a tbl = Empty | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data = {ident: t; data: 'a; previous: 'a data option}

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl =
    match l with
    | Empty -> 0
    | Node (_, _, _, h) -> h
  and hr =
    match r with
    | Empty -> 0
    | Node (_, _, _, h) -> h
  in
  Node (l, d, r, if hl >= hr then hl + 1 else hr + 1)

let balance l d r =
  let hl =
    match l with
    | Empty -> 0
    | Node (_, _, _, h) -> h
  and hr =
    match r with
    | Empty -> 0
    | Node (_, _, _, h) -> h
  in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _)
      when (match ll with
             | Empty -> 0
             | Node (_, _, _, h) -> h)
           >=
           match lr with
           | Empty -> 0
           | Node (_, _, _, h) -> h ->
      mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node (lrl, lrd, lrr, _), _) ->
      mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _)
      when (match rr with
             | Empty -> 0
             | Node (_, _, _, h) -> h)
           >=
           match rl with
           | Empty -> 0
           | Node (_, _, _, h) -> h ->
      mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
      mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else mknode l d r

let rec add id data = function
  | Empty -> Node (Empty, {ident = id; data; previous = None}, Empty, 1)
  | Node (l, k, r, h) ->
    let c = compare id.name k.ident.name in
    if c = 0 then Node (l, {ident = id; data; previous = Some k}, r, h)
    else if c < 0 then balance (add id data l) k r
    else balance l k (add id data r)

let rec find_stamp s = function
  | None -> raise Not_found
  | Some k -> if k.ident.stamp = s then k.data else find_stamp s k.previous

let rec find_same id = function
  | Empty -> raise Not_found
  | Node (l, k, r, _) ->
    let c = compare id.name k.ident.name in
    if c = 0 then
      if id.stamp = k.ident.stamp then k.data
      else find_stamp id.stamp k.previous
    else find_same id (if c < 0 then l else r)

let rec find_name name = function
  | Empty -> raise Not_found
  | Node (l, k, r, _) ->
    let c = compare name k.ident.name in
    if c = 0 then (k.ident, k.data) else find_name name (if c < 0 then l else r)

let rec get_all = function
  | None -> []
  | Some k -> (k.ident, k.data) :: get_all k.previous

let rec find_all name = function
  | Empty -> []
  | Node (l, k, r, _) ->
    let c = compare name k.ident.name in
    if c = 0 then (k.ident, k.data) :: get_all k.previous
    else find_all name (if c < 0 then l else r)

let rec fold_aux f stack accu = function
  | Empty -> (
    match stack with
    | [] -> accu
    | a :: l -> fold_aux f l accu a)
  | Node (l, k, r, _) -> fold_aux f (l :: stack) (f k accu) r

let fold_name f tbl accu = fold_aux (fun k -> f k.ident k.data) [] accu tbl

let rec fold_data f d accu =
  match d with
  | None -> accu
  | Some k -> f k.ident k.data (fold_data f k.previous accu)

let fold_all f tbl accu = fold_aux (fun k -> fold_data f (Some k)) [] accu tbl

(* let keys tbl = fold_name (fun k _ accu -> k::accu) tbl [] *)

let rec iter f = function
  | Empty -> ()
  | Node (l, k, r, _) ->
    iter f l;
    f k.ident k.data;
    iter f r

(* Idents for sharing keys *)

(* They should be 'totally fresh' -> neg numbers *)
let key_name = ""

let make_key_generator () =
  let c = ref 1 in
  fun id ->
    let stamp = !c in
    decr c;
    {id with name = key_name; stamp}

let compare x y =
  let c = x.stamp - y.stamp in
  if c <> 0 then c
  else
    let c = compare x.name y.name in
    if c <> 0 then c else compare x.flags y.flags

let output oc id = output_string oc (unique_name id)
let hash i = Char.code i.name.[0] lxor i.stamp

let original_equal = equal
include Identifiable.Make (struct
  type nonrec t = t
  let compare = compare
  let output = output
  let print = print
  let hash = hash
  let equal = same
end)
let equal = original_equal
