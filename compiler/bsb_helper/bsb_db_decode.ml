(* Copyright (C) 2019 - Present Hongbo Zhang, Authors of ReScript
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

let bsbuild_cache = Literals.bsbuild_cache

type group =
  | Dummy
  | Group of {
      modules: string array;
      dir_length: int;
      dir_info_offset: int;
      module_info_offset: int;
    }

type t = {lib: group; dev: group; content: string (* string is whole content*)}

type cursor = int ref

(*TODO: special case when module_count is zero *)
let rec decode (x : string) : t =
  let (offset : cursor) = ref 0 in
  let lib = decode_single x offset in
  let dev = decode_single x offset in
  {lib; dev; content = x}

and decode_single (x : string) (offset : cursor) : group =
  let module_number = Ext_pervasives.parse_nat_of_string x offset in
  incr offset;
  if module_number <> 0 then (
    let modules = decode_modules x offset module_number in
    let dir_info_offset = !offset in
    let module_info_offset = String.index_from x dir_info_offset '\n' + 1 in
    let dir_length =
      Char.code x.[module_info_offset] - 48
      (* Char.code '0'*)
    in
    offset := module_info_offset + 1 + (dir_length * module_number) + 1;
    Group {modules; dir_info_offset; module_info_offset; dir_length})
  else Dummy

and decode_modules (x : string) (offset : cursor) module_number : string array =
  let result = Array.make module_number "" in
  let last = ref !offset in
  let cur = ref !offset in
  let tasks = ref 0 in
  while !tasks <> module_number do
    if String.unsafe_get x !cur = '\n' then (
      let offs = !last in
      let len = !cur - !last in
      Array.unsafe_set result !tasks (Ext_string.unsafe_sub x offs len);
      incr tasks;
      last := !cur + 1);
    incr cur
  done;
  offset := !cur;
  result

(* TODO: shall we check the consistency of digest *)
let read_build_cache ~dir : t =
  let all_content = Ext_io.load_file (Filename.concat dir bsbuild_cache) in
  decode all_content

type module_info = {case: bool; (* which is Bsb_db.case*) dir_name: string}

let find_opt ({content = whole} as db : t) lib (key : string) :
    module_info option =
  match if lib then db.lib else db.dev with
  | Dummy -> None
  | Group ({modules} as group) -> (
    let i = Ext_string_array.find_sorted modules key in
    match i with
    | None -> None
    | Some count ->
      let encode_len = group.dir_length in
      let index =
        Ext_string.get_1_2_3_4 whole
          ~off:(group.module_info_offset + 1 + (count * encode_len))
          encode_len
      in
      let case = not (index mod 2 = 0) in
      let ith = index lsr 1 in
      let dir_name_start =
        if ith = 0 then group.dir_info_offset
        else Ext_string.index_count whole group.dir_info_offset '\t' ith + 1
      in
      let dir_name_finish = String.index_from whole dir_name_start '\t' in
      Some
        {
          case;
          dir_name =
            String.sub whole dir_name_start (dir_name_finish - dir_name_start);
        })

let find db dependent_module is_not_lib_dir =
  let opt = find_opt db true dependent_module in
  match opt with
  | Some _ -> opt
  | None -> if is_not_lib_dir then find_opt db false dependent_module else None
