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

(* Reads build files missing from [Build_file_store]. Open each file once and
   read that descriptor: on case-insensitive filesystems a lowercase alias can
   remain visible to [Sys.file_exists] briefly after its CMI is removed, and a
   vanished file must count as missing. *)
let read_file path =
  match
    Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_SHARE_DELETE] 0
  with
  | descriptor ->
    let channel = Unix.in_channel_of_descr descriptor in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Some (really_input_string channel (in_channel_length channel)))
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None
  | exception Unix.Unix_error (error, _, _) ->
    raise (Sys_error (path ^ ": " ^ Unix.error_message error))

(* Interfaces read by earlier requests on this domain, by path, with the
   stored contents they were read from. Reading the same interfaces, above all
   the runtime's, is a sizable part of compiling a small module. The values
   can't be shared between domains: copying a signature marks its types in
   place for the duration of the copy. *)
type cached_cmi = {contents: string; cmi: Cmi_format.cmi_infos}

type cmi_cache = {
  mutable generation: int;
  mutable size: int;  (** The length of the cached contents. *)
  cmis: (string, cached_cmi) Hashtbl.t;
}

(* A decoded interface takes a few times the size of its contents, and every
   domain keeps its own, so a domain that would cache more contents than this
   starts over. The runtime's interfaces take 2 MB. *)
let cmi_cache_budget = 16 * 1024 * 1024

let cmi_cache_key =
  Domain.DLS.new_key (fun () ->
      {generation = -1; size = 0; cmis = Hashtbl.create 64})

let empty_cmi_cache cache ~generation =
  Hashtbl.reset cache.cmis;
  cache.size <- 0;
  cache.generation <- generation

let read_cached_cmi filename path contents =
  let cache = Domain.DLS.get cmi_cache_key in
  if cache.generation <> Build_file_store.generation () then
    empty_cmi_cache cache ~generation:(Build_file_store.generation ());
  match Hashtbl.find_opt cache.cmis path with
  | Some cached when cached.contents == contents -> cached.cmi
  | previous ->
    let cmi = Cmi_format.read_cmi_contents filename contents in
    Option.iter
      (fun previous ->
        cache.size <- cache.size - String.length previous.contents)
      previous;
    if cache.size + String.length contents > cmi_cache_budget then
      empty_cmi_cache cache ~generation:cache.generation;
    Hashtbl.replace cache.cmis path {contents; cmi};
    cache.size <- cache.size + String.length contents;
    cmi

(* A request that fails can stop with a type of a cached interface still
   marked, so the domain reads its interfaces again. *)
let discard_cached_cmis () =
  empty_cmi_cache (Domain.DLS.get cmi_cache_key) ~generation:(-1)

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  Misc.find_map_in_path_uncap (Config.get_load_path ()) (unit_name ^ ".cmi")
    (fun filename ->
      let path = Compiler_request_state.resolve_path filename in
      match Build_file_store.read path with
      | None -> None
      | Some contents ->
        let cmi =
          if Build_file_store.enabled () then
            read_cached_cmi filename path contents
          else Cmi_format.read_cmi_contents filename contents
        in
        Some Env.Persistent_signature.{filename; cmi})
