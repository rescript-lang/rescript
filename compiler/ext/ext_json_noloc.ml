(* Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
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

(* This file is only used in bsb watcher searlization *)
type t =
  | True
  | False
  | Null
  | Flo of string
  | Str of string
  | Arr of t array
  | Obj of t Map_string.t

(** poor man's serialization *)
let naive_escaped (text : string) : string =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c ->
        let code = Char.code c in
        if code < 0x20 then Printf.bprintf buf "\\u%04x" code
        else Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let quot x = "\"" ^ naive_escaped x ^ "\""

let true_ = True

let false_ = False

let null = Null

let str s = Str s

let flo s = Flo s

let arr s = Arr s

let obj s = Obj s

let kvs s = Obj (Map_string.of_list s)

let rec encode_buf (x : t) (buf : Buffer.t) : unit =
  let a str = Buffer.add_string buf str in
  match x with
  | Null -> a "null"
  | Str s -> a (quot s)
  | Flo s ->
    a s
    (*
         since our parsing keep the original float representation, we just dump it as is, there is no cases like [nan] *)
  | Arr content -> (
    match content with
    | [||] -> a "[]"
    | _ ->
      a "[ ";
      encode_buf (Array.unsafe_get content 0) buf;
      for i = 1 to Array.length content - 1 do
        a " , ";
        encode_buf (Array.unsafe_get content i) buf
      done;
      a " ]")
  | True -> a "true"
  | False -> a "false"
  | Obj map ->
    if Map_string.is_empty map then a "{}"
    else (
      (*prerr_endline "WEIRD";
        prerr_endline (string_of_int @@ Map_string.cardinal map ); *)
      a "{ ";
      let (_ : int) =
        Map_string.fold map 0 (fun k v i ->
            if i <> 0 then a " , ";
            a (quot k);
            a " : ";
            encode_buf v buf;
            i + 1)
      in
      a " }")

let to_string x =
  let buf = Buffer.create 1024 in
  encode_buf x buf;
  Buffer.contents buf

let to_channel (oc : out_channel) x =
  let buf = Buffer.create 1024 in
  encode_buf x buf;
  Buffer.output_buffer oc buf

let to_file name v =
  let ochan = open_out_bin name in
  to_channel ochan v;
  close_out ochan
