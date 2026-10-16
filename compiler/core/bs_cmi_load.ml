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

(* A host that compiles many modules in one process, such as rewatch, would
   otherwise read the same interfaces again for every module, and reading the
   large Stdlib ones is a sizable part of compiling a small module. With the
   cache enabled, each domain keeps the interfaces it reads. Domains can't share
   them: copying a signature (Subst) marks its types in place until the copy is
   done.

   An entry is used only while the file's identity, size and modification time
   are unchanged. Like Git with racily clean files, a file modified in the last
   two seconds is always read again: a rewrite of the same size within one tick
   of a coarse file system clock leaves all of these unchanged. *)
let cache_enabled = Atomic.make false
let enable_cache () = Atomic.set cache_enabled true

let recently_modified_seconds = 2.

(* Bytes of interface files a domain keeps at most: the runtime's are about
   1.5 MB. Past the limit, the domain starts over. *)
let max_cached_bytes = 16 * 1024 * 1024

type cache = {
  entries: (string, (int * int * int * float) * Cmi_format.cmi_infos) Hashtbl.t;
  mutable cached_bytes: int;
}

let cache =
  Domain.DLS.new_key (fun () -> {entries = Hashtbl.create 64; cached_bytes = 0})

let read_cmi_channel filename path descriptor channel =
  if not (Atomic.get cache_enabled) then
    Cmi_format.read_cmi_channel filename channel
  else
    let {Unix.st_dev; st_ino; st_size; st_mtime} = Unix.fstat descriptor in
    if Unix.gettimeofday () -. st_mtime < recently_modified_seconds then
      Cmi_format.read_cmi_channel filename channel
    else
      let file_id = (st_dev, st_ino, st_size, st_mtime) in
      let cache = Domain.DLS.get cache in
      match Hashtbl.find_opt cache.entries path with
      | Some (cached_id, cmi) when cached_id = file_id -> cmi
      | previous ->
        let cmi = Cmi_format.read_cmi_channel filename channel in
        (match previous with
        | Some ((_, _, size, _), _) ->
          cache.cached_bytes <- cache.cached_bytes - size
        | None -> ());
        if cache.cached_bytes + st_size > max_cached_bytes then (
          Hashtbl.reset cache.entries;
          cache.cached_bytes <- 0);
        Hashtbl.replace cache.entries path (file_id, cmi);
        cache.cached_bytes <- cache.cached_bytes + st_size;
        cmi

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  (* On case-insensitive filesystems a lowercase alias can remain visible to
     [Sys.file_exists] briefly after its CMI is removed. Open each candidate
     once and parse that descriptor, so a vanished CMI is a missing module. *)
  let name = unit_name ^ ".cmi" in
  let lower_name = String.uncapitalize_ascii name in
  let rec find = function
    | [] -> None
    | directory :: rest ->
      let rec try_names = function
        | [] -> find rest
        | name :: names -> (
          let filename = Filename.concat directory name in
          let path = Compiler_request_state.resolve_path filename in
          match Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 with
          | descriptor ->
            let channel = Unix.in_channel_of_descr descriptor in
            let cmi =
              Fun.protect
                (fun () -> read_cmi_channel filename path descriptor channel)
                ~finally:(fun () -> close_in_noerr channel)
            in
            Some Env.Persistent_signature.{filename; cmi}
          | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) ->
            try_names names
          | exception Unix.Unix_error (error, _, _) ->
            raise (Sys_error (path ^ ": " ^ Unix.error_message error)))
      in
      try_names (if lower_name = name then [name] else [lower_name; name])
  in
  find (Config.get_load_path ())

module For_test = struct
  let cached_interfaces () = Hashtbl.length (Domain.DLS.get cache).entries
end
