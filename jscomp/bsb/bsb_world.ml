(* Copyright (C) 2017- Authors of BuckleScript
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


let (//) = Ext_path.combine

(** TODO: create the animation effect 
    logging installed files
*)
#if BS_NATIVE then
let install_targets ~backend cwd (config : Bsb_config_types.t option) =
#else
let install_targets cwd (config : Bsb_config_types.t option) =
#end
  
  let install ~destdir file = 
    if Bsb_file.install_if_exists ~destdir file  then 
      begin 
        ()

      end
  in
#if BS_NATIVE then
  let install_filename_sans_extension destdir namespace nested x = 
#else
  let install_filename_sans_extension destdir namespace x = 
#end
    let x = 
      match namespace with 
      | None -> x 
      | Some ns -> Ext_namespace.make ~ns x in 
    install ~destdir (cwd // x ^  Literals.suffix_ml) ;
    install ~destdir (cwd // x ^  Literals.suffix_re) ;
    install ~destdir (cwd // x ^ Literals.suffix_mli) ;
    install ~destdir (cwd // x ^  Literals.suffix_rei) ;
#if BS_NATIVE then
    (* The library file generated by bsb for each external dep has the 
       same name because they're in different folders and because it makes
       linking easier. *)
    let artifacts_directory = cwd // Bsb_config.lib_bs // nested in
    install ~destdir (artifacts_directory // Literals.library_file ^ Literals.suffix_a) ;
    install ~destdir (artifacts_directory // Literals.library_file ^ Literals.suffix_cma) ;
    install ~destdir (artifacts_directory // Literals.library_file ^ Literals.suffix_cmxa) ;
    
    install ~destdir (artifacts_directory // x ^ Literals.suffix_cmi) ;
    install ~destdir (artifacts_directory // x ^ Literals.suffix_cmj) ;
    install ~destdir (artifacts_directory // x ^ Literals.suffix_cmt) ;
    install ~destdir (artifacts_directory // x ^ Literals.suffix_cmti) ;
#else
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmi) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmj) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmt) ;
    install ~destdir (cwd // Bsb_config.lib_bs//x ^ Literals.suffix_cmti) ;

#end
  in   
  match config with 
  | None -> ()
  | Some {files_to_install; namespace; package_name} -> 
#if BS_NATIVE then
    let nested = begin match backend with
      | Bsb_config_types.Js       -> "js"
      | Bsb_config_types.Native   -> "native"
      | Bsb_config_types.Bytecode -> "bytecode"
    end in
    let destdir = cwd // Bsb_config.lib_ocaml // nested in (* lib is already there after building, so just mkdir [lib/ocaml] *)
    if not @@ Sys.file_exists destdir then begin Bsb_build_util.mkp destdir  end;
#else
    let destdir = cwd // Bsb_config.lib_ocaml in (* lib is already there after building, so just mkdir [lib/ocaml] *)
    if not @@ Sys.file_exists destdir then begin Unix.mkdir destdir 0o777  end;
#end

    begin
      Bsb_log.info "@{<info>Installing started@}@.";
      begin match namespace with 
        | None -> ()
        | Some x -> 
#if BS_NATIVE then
          install_filename_sans_extension destdir None nested  x
#else
          install_filename_sans_extension destdir None  x
#end
      end;
#if BS_NATIVE then
      String_hash_set.iter files_to_install (install_filename_sans_extension destdir namespace nested);
#else
      String_hash_set.iter files_to_install (install_filename_sans_extension destdir namespace);
#end
      Bsb_log.info "@{<info>Installing finished@} @.";
    end


#if BS_NATIVE then
let build_bs_deps cwd ~root_project_dir ~backend ~main_config:(main_config : Bsb_config_types.t) : Bsb_dependency_info.t =
#else

let build_bs_deps cwd deps =

#end
  let bsc_dir = Bsb_build_util.get_bsc_dir ~cwd in
  let vendor_ninja = bsc_dir // "ninja.exe" in
#if BS_NATIVE then
  let ocaml_dir = Bsb_build_util.get_ocaml_dir cwd in
  let dependency_info = Bsb_dependency_info.{
    all_external_deps = [];
    all_ocamlfind_dependencies = [];
    all_ocaml_dependencies = Depend.StringSet.empty;
    all_clibs = [];
    all_c_linker_flags = [];
    all_toplevel_ppxes = String_map.empty;
  } in
  
  let all_ppxes = ref String_map.empty in
#end
  
  (* @Idea could this be parallelized? We're not taking advantage of ninja here 
     and it seems like we're just going one dep at a time when we could parallelize 
              Ben - August 9th 2017 *)
  Bsb_build_util.walk_all_deps  cwd
    (fun {top; cwd} ->
       if not top then
         begin 
#if BS_NATIVE then
          (* @Esy this probably doesn't do what we think it does. *)
          let build_artifacts_dir = Bsb_build_util.get_build_artifacts_location cwd in
          let config = 
            Bsb_config_parse.interpret_json 
              ~override_package_specs:(Some main_config.package_specs)
              ~bsc_dir
              ~generate_watch_metadata:false
              ~not_dev:true
              cwd in
          let _did_regen = 
            Bsb_ninja_regen.regenerate_ninja 
              ~dependency_info
              ~is_top_level:false
              ~not_dev:true
              ~root_project_dir
              ~forced:true
              ~backend
              ~build_library:None
              ~main_config:config
              ~ocaml_dir
              cwd bsc_dir in (* set true to force regenrate ninja file so we have [config_opt]*)
          all_ppxes := List.fold_left Bsb_config_types.(fun all_ppxes e -> 
            match e with
            | {kind = Ppx} -> 
                String_map.update config.Bsb_config_types.package_name (function
                  | None -> Some [e]
                  | Some l -> Some (e :: l)
                ) all_ppxes
            | _ -> all_ppxes
          ) !all_ppxes config.Bsb_config_types.entries;
          
          (* Append at the head for a correct topological sort. 
             walk_all_deps does a simple DFS, so all we need to do is to append at the head of 
             a list to build a topologically sorted list of external deps.
           *)
          if List.mem backend Bsb_config_types.(config.allowed_build_kinds) then begin
            dependency_info.all_c_linker_flags <- (Bsb_config_types.(config.c_linker_flags)) @ dependency_info.all_c_linker_flags;
            dependency_info.all_clibs <- (Bsb_config_types.(config.static_libraries)) @ dependency_info.all_clibs;
            dependency_info.all_ocamlfind_dependencies <- Bsb_config_types.(config.ocamlfind_dependencies) @ dependency_info.all_ocamlfind_dependencies;
            dependency_info.all_ocaml_dependencies <- List.fold_left (fun acc v -> Depend.StringSet.add v acc) dependency_info.all_ocaml_dependencies Bsb_config_types.(config.ocaml_dependencies);
            
            let has_at_least_one_lib_entry = List.exists (fun (g : Bsb_file_groups.file_group) -> match g with
              | {is_ppx = false; sources} -> not (String_map.is_empty sources)
              | _ -> false) config.bs_file_groups in
            let nested = begin match backend with 
            | Bsb_config_types.Js -> "js"
            | Bsb_config_types.Bytecode -> 
              if has_at_least_one_lib_entry then 
                dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // "bytecode") :: dependency_info.all_external_deps;
                
              "bytecode"
            | Bsb_config_types.Native -> 
              if has_at_least_one_lib_entry then 
                dependency_info.all_external_deps <- (build_artifacts_dir // Bsb_config.lib_ocaml // "native") :: dependency_info.all_external_deps;
                
              "native"
            end in
            let command = 
              {Bsb_unix.cmd = vendor_ninja;
                cwd = build_artifacts_dir // Bsb_config.lib_bs // nested;
                args  = [|vendor_ninja|] ;
                env = Array.append (Unix.environment ()) [| "BSB_BACKEND=" ^ nested |] ;
              } in     
            let eid =
              Bsb_unix.run_command_execv
              command in 
            if eid <> 0 then   
              Bsb_unix.command_fatal_error command eid;
          
            if Bsb_config_types.(config.build_script) <> None then begin
              let filename = build_artifacts_dir // Literals.dot_static_libraries in
              if not (Sys.file_exists filename) then  ()
                (* Bsb_exception.missing_static_libraries_file (Bsb_config_types.(config.package_name))  *)
              else begin
                let artifacts_installed = ref [] in
                let ic = open_in_bin filename in
                (try
                  while true do
                    artifacts_installed := (String.trim (input_line ic)) :: !artifacts_installed
                  done
                with End_of_file -> ());
                close_in ic;
                (* @Todo This is just for the 3.0 release, so it goes a bit smoother. Once all of our packages 
                   are fixed we don't need to dedupe. 
                            April 17th 2018
                 *)
                dependency_info.all_clibs <- (List.filter (fun i -> 
                  let is_already_linked = List.mem i dependency_info.all_clibs in
                  if is_already_linked then 
                    Bsb_log.warn "@{<warn>Warning@} package %s: `static-libraries` doesn't need to have '%s' \
                      as it's automatically linked by the build-script, you can safely remove it from that list.@." config.package_name i;
                  not is_already_linked) !artifacts_installed) @ dependency_info.all_clibs;
              end
           end;
           
           (* When ninja is not regenerated, ninja will still do the build, 
              still need reinstall check
              Note that we can check if ninja print "no work to do", 
              then don't need reinstall more
            *)
            install_targets ~backend build_artifacts_dir (Some config);
          end     
        end else begin
          dependency_info.all_toplevel_ppxes <- List.fold_left (fun all_toplevel_ppxes ({package_name} : Bsb_config_types.dependency) ->
            match String_map.find_opt package_name !all_ppxes with
            | None -> all_toplevel_ppxes
            | Some v -> String_map.add package_name v all_toplevel_ppxes
          ) dependency_info.all_toplevel_ppxes main_config.bs_dependencies
        end
      );
  (* Reverse order here so the leaf deps are at the beginning *)
  dependency_info.all_external_deps <- List.rev dependency_info.all_external_deps;
  dependency_info.all_ocamlfind_dependencies <- List.rev dependency_info.all_ocamlfind_dependencies;
  dependency_info
#else
           let config_opt = Bsb_ninja_regen.regenerate_ninja ~not_dev:true
               ~generate_watch_metadata:false
               ~override_package_specs:(Some deps) 
               ~forced:true
               cwd bsc_dir  in (* set true to force regenrate ninja file so we have [config_opt]*)
           let command = 
            {Bsb_unix.cmd = vendor_ninja;
              cwd = cwd // Bsb_config.lib_bs;
              args  = [|vendor_ninja|]
             } in     
           let eid =
             Bsb_unix.run_command_execv
             command in 
           if eid <> 0 then   
            Bsb_unix.command_fatal_error command eid;
           (* When ninja is not regenerated, ninja will still do the build, 
              still need reinstall check
              Note that we can check if ninja print "no work to do", 
              then don't need reinstall more
           *)
           install_targets cwd config_opt;
         end
    )
#end


#if BS_NATIVE then
let make_world_deps cwd ~root_project_dir ~backend ~main_config =
  Bsb_log.info "Making the dependency world!@.";
  build_bs_deps cwd ~root_project_dir ~backend ~main_config
#else
let make_world_deps cwd (config : Bsb_config_types.t option) =
  Bsb_log.info "Making the dependency world!@.";
  let deps =
    match config with
    | None ->
      (* When this running bsb does not read bsconfig.json,
         we will read such json file to know which [package-specs]
         it wants
      *)
      Bsb_config_parse.package_specs_from_bsconfig ()
    | Some {package_specs} -> package_specs in
  build_bs_deps cwd deps
#end
