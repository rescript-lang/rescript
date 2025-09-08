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

let ( // ) = Ext_path.combine
let vendor_ninja = Bsb_global_paths.vendor_ninja

let make_world_deps cwd (config : Bsb_config_types.t option)
    (ninja_args : string array) =
  let package_specs, jsx =
    match config with
    | None ->
      (* When this running bsb does not read rescript.json,
         we will read such json file to know which [package-specs]
         it wants
      *)
      Bsb_config_parse.deps_from_bsconfig ()
    | Some config -> (config.package_specs, config.jsx)
  in
  let args =
    if Ext_array.is_empty ninja_args then [|vendor_ninja|]
    else Array.append [|vendor_ninja|] ninja_args
  in
  let lib_artifacts_dir = Bsb_config.lib_bs in
  let queue = Bsb_build_util.walk_all_deps cwd in
  (* let oc = open_out_bin ".deps.log" in
     queue |> Queue.iter (fun ({top; proj_dir} : Bsb_build_util.package_context) ->
     match top with
     | Expect_none -> ()
     | Expect_name s ->
      output_string oc s ;
      output_string oc " : ";
      output_string oc proj_dir;
      output_string oc "\n"
     );
     close_out oc ; *)
  queue
  |> Queue.iter (fun ({top; proj_dir} : Bsb_build_util.package_context) ->
         match top with
         | Expect_none -> ()
         | Expect_name s ->
           print_endline ("Dependency on " ^ s);
           let lib_bs_dir = proj_dir // lib_artifacts_dir in
           Bsb_build_util.mkp lib_bs_dir;
           let _config : _ option =
             Bsb_ninja_regen.regenerate_ninja
               ~package_kind:(Dependency {package_specs; jsx})
               ~per_proj_dir:proj_dir ~forced:false ~warn_legacy_config:false
               ~warn_as_error:None
           in
           let command =
             {Bsb_unix.cmd = vendor_ninja; cwd = lib_bs_dir; args}
           in
           let eid = Bsb_unix.run_command_execv command in
           if eid <> 0 then Bsb_unix.command_fatal_error command eid;
           (* When ninja is not regenerated, ninja will still do the build,
              still need reinstall check
              Note that we can check if ninja print "no work to do",
              then don't need reinstall more
           *)
           Bsb_log.info "@{<info>Installation started@}@.";
           let install_dir = proj_dir // "lib" // "ocaml" in
           Bsb_build_util.mkp install_dir;
           let install_command =
             {
               Bsb_unix.cmd = vendor_ninja;
               cwd = install_dir;
               args = [|vendor_ninja; "-f"; ".." // "bs" // "install.ninja"|];
             }
           in
           let eid = Bsb_unix.run_command_execv install_command in
           if eid <> 0 then Bsb_unix.command_fatal_error install_command eid;
           Bsb_log.info "@{<info>Installation finished@}@.");
  print_endline "Dependency Finished"
