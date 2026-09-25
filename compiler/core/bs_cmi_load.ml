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

let load_cmi ~unit_name : Env.Persistent_signature.t option =
  Compiler_phase_trace.dependency ("dependency.search_open:" ^ unit_name)
    (fun () ->
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
              match Unix.openfile path [Unix.O_RDONLY] 0 with
              | descriptor ->
                let channel = Unix.in_channel_of_descr descriptor in
                let cmi =
                  Fun.protect
                    (fun () -> Cmi_format.read_cmi_channel filename channel)
                    ~finally:(fun () -> close_in_noerr channel)
                in
                Some Env.Persistent_signature.{filename; cmi}
              | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _)
                ->
                try_names names
              | exception Unix.Unix_error (error, _, _) ->
                raise (Sys_error (path ^ ": " ^ Unix.error_message error)))
          in
          try_names (if lower_name = name then [name] else [lower_name; name])
      in
      find (Config.get_load_path ()))
