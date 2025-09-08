(* Copyright (C) Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t =
  | Arity_info of int list * bool
      (**
     the last one means it can take any params later, 
     for an exception: it is (Determin (true,[], true))
     1. approximation sound but not complete 

  *)
  | Arity_na

let equal (x : t) y =
  match x with
  | Arity_na -> y = Arity_na
  | Arity_info (xs, a) -> (
    match y with
    | Arity_info (ys, b) ->
      a = b && Ext_list.for_all2_no_exn xs ys (fun x y -> x = y)
    | Arity_na -> false)

let pp = Format.fprintf

let print (fmt : Format.formatter) (x : t) =
  match x with
  | Arity_na -> pp fmt "?"
  | Arity_info (ls, tail) ->
    pp fmt "@[";
    pp fmt "[";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> pp fmt ",")
      (fun fmt x -> Format.pp_print_int fmt x)
      fmt ls;
    if tail then pp fmt "@ *";
    pp fmt "]@]"

let print_arities_tbl (fmt : Format.formatter)
    (arities_tbl : (Ident.t, t ref) Hashtbl.t) =
  Hashtbl.fold
    (fun (i : Ident.t) (v : t ref) _ -> pp fmt "@[%s -> %a@]@." i.name print !v)
    arities_tbl ()

let merge (n : int) (x : t) : t =
  match x with
  | Arity_na -> Arity_info ([n], false)
  | Arity_info (xs, tail) -> Arity_info (n :: xs, tail)

let non_function_arity_info = Arity_info ([], false)

let raise_arity_info = Arity_info ([], true)

let na = Arity_na

let info args b1 = Arity_info (args, b1)

let first_arity_na (x : t) =
  match x with
  | Arity_na | Arity_info ([], _) -> true
  | _ -> false

let get_first_arity (x : t) =
  match x with
  | Arity_na | Arity_info ([], _) -> None
  | Arity_info (x :: _, _) -> Some x

let extract_arity (x : t) =
  match x with
  | Arity_na -> []
  | Arity_info (xs, _) -> xs

(* let update_arity (x : t) xs =    *)

let rec merge_arities_aux (acc : int list) (xs : int list) (ys : int list)
    (tail : bool) (tail2 : bool) =
  match (xs, ys) with
  | [], [] -> info (List.rev acc) (tail && tail2)
  | [], y :: ys when tail -> merge_arities_aux (y :: acc) [] ys tail tail2
  | x :: xs, [] when tail2 -> merge_arities_aux (x :: acc) [] xs tail tail2
  | x :: xs, y :: ys when x = y -> merge_arities_aux (y :: acc) xs ys tail tail2
  | _, _ -> info (List.rev acc) false

let merge_arities xs ys t t2 = merge_arities_aux [] xs ys t t2
