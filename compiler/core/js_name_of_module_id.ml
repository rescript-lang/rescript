(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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
(*
let (=)  (x : int) (y:float) = assert false 
*)

#ifdef BROWSER

let string_of_module_id_in_browser (x : Lam_module_ident.t) =  
  match x.kind with
  | External {name} -> name
  | Runtime | Ml -> 
    "./stdlib/" ^ x.id.name ^ ".js"

let string_of_module_id 
    (id : Lam_module_ident.t)
    ~output_dir:(_:string)
    (_module_system : Js_packages_info.module_system)
  = string_of_module_id_in_browser id

#else
  
let (//) = Filename.concat 


let fix_path_for_windows : string -> string = 
  if Ext_sys.is_windows_or_cygwin then Ext_string.replace_backward_slash
  else fun s -> s 


(* dependency is runtime module *)  
let get_runtime_module_path 
    (dep_module_id : Lam_module_ident.t) 
    (current_package_info : Js_packages_info.t)
    (module_system : Js_packages_info.module_system) = 
  let current_info_query = 
    Js_packages_info.query_package_infos current_package_info
      module_system  in
  (* Runtime package is pre-compiled and always uses .js suffix *)
  let js_file =  
    Ext_namespace.js_name_of_modulename dep_module_id.id.name 
      Upper Literals.suffix_js in 
  match current_info_query with        
  | Package_not_found -> assert false
  | Package_script -> 
    Js_packages_info.runtime_package_path module_system js_file          
  | Package_found pkg -> 
    let  dep_path  = 
      "lib" // Js_packages_info.runtime_dir_of_module_system module_system in 
    if  Js_packages_info.is_runtime_package current_package_info then 
      Ext_path.node_rebase_file
        ~from:pkg.rel_path
        ~to_:dep_path 
        js_file
        (* TODO: we assume that both [x] and [path] could only be relative path
            which is guaranteed by [-bs-package-output]
        *)
    else  
      match module_system with 
      | Commonjs | Esmodule -> 
        Js_packages_info.runtime_package_path module_system js_file              
      (* Note we did a post-processing when working on Windows *)
      | Es6_global 
        -> 
        (* lib/ocaml/xx.cmj --               
            HACKING: FIXME
            maybe we can caching relative package path calculation or employ package map *)
        (* assert false  *)
        Ext_path.rel_normalized_absolute_path              
          ~from:(
            Js_packages_info.get_output_dir 
              current_package_info
              ~package_dir:(Lazy.force Ext_path.package_dir)
              module_system )
          (*Invariant: the package path to rescript, it is used to 
            calculate relative js path
          *)
          (!Runtime_package.path // dep_path // js_file)

(* [output_dir] is decided by the command line argument *)
let string_of_module_id 
    (dep_module_id : Lam_module_ident.t) 
    ~(output_dir : string )
    (module_system : Js_packages_info.module_system)        
  : string =
  let current_package_info = Js_packages_state.get_packages_info ()  in 
  fix_path_for_windows (    
    match dep_module_id.kind  with 
    | External {name} -> name (* the literal string for external package *)
    (* This may not be enough, 
        1. For cross packages, we may need settle 
        down a single js package
        2. We may need es6 path for dead code elimination
         But frankly, very few JS packages have no dependency, 
         so having plugin may sound not that bad   
    *)
    | Runtime  -> 
      get_runtime_module_path dep_module_id current_package_info module_system
    | Ml  -> 
      let current_info_query = 
        Js_packages_info.query_package_infos 
          current_package_info
          module_system  
      in
      match Lam_compile_env.get_package_path_from_cmj dep_module_id with 
      | (package_path, dep_package_info, case) -> 


        let dep_info_query =  
          Js_packages_info.query_package_infos dep_package_info module_system 
        in
        match dep_info_query, current_info_query with
        | Package_not_found , _  -> 
          Bs_exception.error (Missing_ml_dependency dep_module_id.id.name)
        | Package_script , Package_found _  -> 
          Bs_exception.error (Dependency_script_module_dependent_not dep_module_id.id.name)
        | (Package_script  | Package_found _ ), Package_not_found -> assert false

        | Package_found ({suffix} as pkg), Package_script 
          ->
          let js_file =  
            Ext_namespace.js_name_of_modulename dep_module_id.id.name case suffix in
          (* External package imports: check if pkg_rel_path ends with "/."
             which indicates the dependency uses in-source builds *)
          if Ext_string.ends_with pkg.pkg_rel_path "/." then begin
            let cmj_file = dep_module_id.id.name ^ Literals.suffix_cmj in
            match Config_util.find_opt cmj_file with
            | Some cmj_path ->
              (* External packages store .cmj at node_modules/<pkg>/lib/bs/<source_dir>/<module>.cmj
                 Example: /Users/barry/Projects/great-project/node_modules/a/lib/bs/src/A-A.cmj
                 We extract "src" from this path. *)
              let cmj_dir = Filename.dirname cmj_path in
              let lib_bs_pattern = "/lib/bs/" in
              let source_dir =
                try
                  let rec find_lib_bs pos =
                    if pos < 0 then None
                    else if Ext_string.starts_with (String.sub cmj_dir pos (String.length cmj_dir - pos)) lib_bs_pattern then
                      Some (pos + String.length lib_bs_pattern)
                    else
                      find_lib_bs (pos - 1)
                  in
                  match find_lib_bs (String.length cmj_dir - 1) with
                  | Some start_idx ->
                    String.sub cmj_dir start_idx (String.length cmj_dir - start_idx)
                  | None -> "."
                with Not_found -> "."
              in
              (* Extract package name from pkg_rel_path: "a/." -> "a" *)
              let pkg_name = 
                String.sub pkg.pkg_rel_path 0 (String.length pkg.pkg_rel_path - 2)
              in
              if source_dir = "." then begin
                pkg.pkg_rel_path // js_file
              end else begin
                let result = pkg_name // source_dir // js_file in
                (* Reconstruct: "a" + "src" + "A.res.js" = "a/src/A.res.js" *)
                result
              end
            | None -> 
              pkg.pkg_rel_path // js_file
          end else begin
            pkg.pkg_rel_path // js_file
          end
        | Package_found ({suffix } as dep_pkg),
          Package_found cur_pkg -> 
          let js_file =  
            Ext_namespace.js_name_of_modulename dep_module_id.id.name case suffix in 

          if  Js_packages_info.same_package_by_name current_package_info  dep_package_info then 
            (* Same-package imports: both files are in the same package.
               
               Rewatch passes the full directory path via -bs-package-output, e.g. 
               "/Users/barry/Projects/great-project/src/core/intl", so rel_path contains 
               the actual directory.
               
               BSB passes ninja's $in_d variable which expands per-file to the source directory.
               With the fix in bsb_package_specs.ml, this also contains the full source directory
               path, e.g. "/Users/barry/Projects/great-project/src/core/intl".
               
               When both rel_path = ".":
               - Current file in src/core/Core_TempTests.res has rel_path = "."
               - Dependency in src/core/intl/Core_IntlTests.res has rel_path = "."
               
               Calling node_rebase_file(".", ".", "Core_IntlTests.mjs") would incorrectly
               produce "./Core_IntlTests.mjs" when we need "./intl/Core_IntlTests.mjs".
               
               To handle this, we extract the actual source directory from the dependency's
               .cmj file path.
            *)
            if cur_pkg.rel_path = "." && dep_pkg.rel_path = "." then
              (* Both rel_path are "." - extract actual source directories from .cmj locations.
                 
                 In-source builds store .cmj files at lib/bs/<source_dir>/<module>.cmj
                 Example: /Users/barry/Projects/great-project/lib/bs/src/core/intl/Core_IntlTests.cmj
                 
                 We extract the source directory to calculate correct relative import paths. *)
              let cmj_file = dep_module_id.id.name ^ Literals.suffix_cmj in
              match Config_util.find_opt cmj_file with
              | Some cmj_path ->
                let cmj_dir = Filename.dirname cmj_path in
                (* Platform-independent: look for "lib<sep>bs<sep>" where <sep> is / or \\ *)
                let source_dir =
                  try
                    let sep = Filename.dir_sep.[0] in
                    let lib_bs = "lib" ^ Filename.dir_sep ^ "bs" ^ Filename.dir_sep in
                    (* Find "lib/bs/" or "lib\\bs\\" in the path and extract everything after it *)
                    let idx = String.rindex_from cmj_dir (String.length cmj_dir - 1) sep in
                    let rec find_lib_bs pos =
                      if pos < 0 then None
                      else if Ext_string.starts_with (String.sub cmj_dir pos (String.length cmj_dir - pos)) lib_bs then
                        Some (pos + String.length lib_bs)
                      else
                        find_lib_bs (pos - 1)
                    in
                    match find_lib_bs idx with
                    | Some start_idx ->
                      (* Example: extract "src/core/intl" from ".../lib/bs/src/core/intl" or "...\\lib\\bs\\src\\core\\intl" *)
                      String.sub cmj_dir start_idx (String.length cmj_dir - start_idx)
                    | None -> cmj_dir
                  with Not_found -> cmj_dir
                in
                Ext_path.node_rebase_file
                  ~from:(Ext_path.absolute_cwd_path output_dir)
                  ~to_:(Ext_path.absolute_cwd_path source_dir)
                  (Filename.basename js_file)
              | None ->
                Ext_path.node_rebase_file
                  ~from:cur_pkg.rel_path
                  ~to_:dep_pkg.rel_path 
                  js_file
            else
              (* rel_path values contain directory information, use them directly *)
              Ext_path.node_rebase_file
                ~from:cur_pkg.rel_path
                ~to_:dep_pkg.rel_path 
                js_file
              (* TODO: we assume that both [x] and [path] could only be relative path
                  which is guaranteed by [-bs-package-output]
              *)
          else
            if Js_packages_info.is_runtime_package dep_package_info then 
              get_runtime_module_path dep_module_id current_package_info module_system
            else begin
              match module_system with 
                | Commonjs | Esmodule -> 
                  (* External package imports: importing from a different package.
                     
                     When dep_pkg.rel_path = "." (dependency uses in-source builds),
                     pkg_rel_path becomes "package_name/." (e.g., "a/."), which would
                     generate invalid imports like "a/./A.res.js" instead of "a/src/A.res.js".
                     
                     We extract the actual source directory from the dependency's .cmj file
                     location and reconstruct the import path correctly.
                  *)
                  (* External package imports: check if pkg_rel_path ends with "/." or "\."
                     which indicates the dependency uses in-source builds *)
                  let ends_with_dot = 
                    Ext_string.ends_with dep_pkg.pkg_rel_path "/." ||
                    Ext_string.ends_with dep_pkg.pkg_rel_path "\\."
                  in
                  if ends_with_dot then begin
                    let cmj_file = dep_module_id.id.name ^ Literals.suffix_cmj in
                    (* Prefer lib/bs over lib/ocaml as lib/bs preserves source directory structure *)
                    let lib_bs_pattern = "lib" ^ Filename.dir_sep ^ "bs" ^ Filename.dir_sep in
                    let lib_ocaml_pattern = "lib" ^ Filename.dir_sep ^ "ocaml" ^ Filename.dir_sep in
                    let cmj_opt =
                      match Config_util.find_opt cmj_file with
                      | Some path when Ext_string.contain_substring path lib_bs_pattern -> 
                        Some path
                      | Some ocaml_path ->
                        (* Found lib/ocaml, derive lib/bs path from it *)
                        let pkg_root = 
                          try
                            let sep = Filename.dir_sep.[0] in
                            let rec find_lib_ocaml pos =
                              if pos < 0 then None
                              else if Ext_string.starts_with (String.sub ocaml_path pos (String.length ocaml_path - pos)) lib_ocaml_pattern then
                                Some (String.sub ocaml_path 0 pos)
                              else
                                let next_pos = 
                                  try String.rindex_from ocaml_path (pos - 1) sep
                                  with Not_found -> -1
                                in
                                find_lib_ocaml next_pos
                            in
                            find_lib_ocaml (String.length ocaml_path - 1)
                          with Not_found -> None
                        in
                        (match pkg_root with
                         | Some root ->
                           (* The actual cmj file is in lib/bs/src/, not lib/bs/ directly
                              Try a glob search to find it *)
                           let (//) = Filename.concat in
                           let rec find_in_dir dir =
                             let full_path = dir // cmj_file in
                             if Sys.file_exists full_path then Some full_path
                             else
                               try
                                 let subdirs = Sys.readdir dir in
                                 Array.fold_left (fun acc subdir ->
                                   match acc with
                                   | Some _ -> acc
                                   | None ->
                                     let sub_path = dir // subdir in
                                     if Sys.is_directory sub_path then find_in_dir sub_path
                                     else None
                                 ) None subdirs
                               with _ -> None
                           in
                           let lib_bs_dir = root // "lib" // "bs" in
                           (match find_in_dir lib_bs_dir with
                            | Some bs_path ->
                              Some bs_path
                            | None ->
                              Some ocaml_path)
                         | None -> Some ocaml_path)
                      | None -> None
                    in
                match cmj_opt with
              | Some cmj_path ->
                (* External packages store .cmj at node_modules/<pkg>/lib/bs/<source_dir>/<module>.cmj
                   Example: /Users/barry/Projects/rescript/node_modules/a/lib/bs/src/A-A.cmj
                   Or on Windows: C:\Users\barry\node_modules\a\lib\bs\src\A-A.cmj
                   We extract "src" from this path.
                   
                   For namespaced packages, there may be a namespace file at the root (lib/bs/A.cmj)
                   and the actual module in a subdirectory (lib/bs/src/A-A.cmj). We want the latter. *)
                let cmj_dir = Filename.dirname cmj_path in
                let sep = Filename.dir_sep.[0] in
                let lib_bs_pattern = "lib" ^ Filename.dir_sep ^ "bs" ^ Filename.dir_sep in
                let source_dir =
                  try
                    let idx = String.rindex_from cmj_dir (String.length cmj_dir - 1) sep in
                    let rec find_lib_bs pos =
                      if pos < 0 then None
                      else if Ext_string.starts_with (String.sub cmj_dir pos (String.length cmj_dir - pos)) lib_bs_pattern then
                        Some (pos + String.length lib_bs_pattern)
                      else
                        let next_pos = 
                          try String.rindex_from cmj_dir (pos - 1) sep
                          with Not_found -> -1
                        in
                        find_lib_bs next_pos
                    in
                    match find_lib_bs idx with
                    | Some start_idx ->
                      String.sub cmj_dir start_idx (String.length cmj_dir - start_idx)
                    | None -> "."
                  with Not_found -> "."
                in
                      (* Extract package name from pkg_rel_path: "a/." or "a\\." -> "a" *)
                      let pkg_name = 
                        String.sub dep_pkg.pkg_rel_path 0 (String.length dep_pkg.pkg_rel_path - 2)
                      in
                      (* If source_dir is ".", we found a namespace file at the root.
                         Try to find the actual module in subdirectories by searching lib/bs recursively. *)
                      let final_source_dir =
                        if source_dir = "." then begin
                          (* Derive package root from cmj_path: .../node_modules/a/lib/bs/A.cmj -> .../node_modules/a *)
                          try
                            let rec find_pkg_root path =
                              let parent = Filename.dirname path in
                              let basename = Filename.basename path in
                              if basename = pkg_name then Some path  (* Return the path itself, not parent *)
                              else if parent = path then None
                              else find_pkg_root parent
                            in
                            match find_pkg_root cmj_path with
                            | Some pkg_root ->
                              let (//) = Filename.concat in
                              let lib_bs_dir = pkg_root // "lib" // "bs" in
                              (* Recursively search for the module file in subdirectories.
                                 For namespaced modules, the file may be A-Namespace.cmj instead of A.cmj *)
                              let rec find_in_dir dir =
                                (* Use the original module name directly, don't try to extract from js_file *)
                                let module_base = dep_module_id.id.name in
                                (* Check both exact match (A.cmj) and namespace pattern (A-*.cmj) *)
                                let cmj_exact = module_base ^ Literals.suffix_cmj in
                                let cmj_pattern_prefix = module_base ^ "-" in
                                
                                (* First check if dir itself contains a matching file *)
                                let found_in_current_dir =
                                  if dir <> lib_bs_dir then begin
                                    try
                                      let files = Sys.readdir dir in
                                      Array.fold_left (fun acc file ->
                                        match acc with
                                        | Some _ -> acc
                                        | None ->
                                          if file = cmj_exact || Ext_string.starts_with file cmj_pattern_prefix then begin
                                            if Ext_string.ends_with file Literals.suffix_cmj then begin
                                              let full_path = dir // file in
                                              if Sys.file_exists full_path && not (Sys.is_directory full_path) then
                                                (* Found in a subdirectory, extract relative path from lib/bs/ *)
                                                let rel_from_lib_bs = String.sub dir (String.length lib_bs_dir + 1) (String.length dir - String.length lib_bs_dir - 1) in
                                                Some rel_from_lib_bs
                                              else None
                                            end else None
                                          end else None
                                      ) None files
                                    with _ -> None
                                  end else None
                                in
                                
                                match found_in_current_dir with
                                | Some _ -> found_in_current_dir
                                | None ->
                                  (* Not found in current dir, search subdirectories *)
                                  try
                                    let subdirs = Sys.readdir dir in
                                    Array.fold_left (fun acc subdir ->
                                      match acc with
                                      | Some _ -> acc
                                      | None ->
                                        let sub_path = dir // subdir in
                                        if Sys.is_directory sub_path then find_in_dir sub_path
                                        else None
                                    ) None subdirs
                                  with _ -> None
                              in
                              (match find_in_dir lib_bs_dir with
                               | Some subdir -> subdir
                               | None -> ".")
                            | None -> "."
                          with _ -> "."
                        end else
                          source_dir
                      in
                      if final_source_dir = "." then
                        (* Still couldn't find it, use default *)
                        dep_pkg.pkg_rel_path // js_file
                      else begin
                        let result = pkg_name // final_source_dir // js_file in
                        (* Reconstruct: "a" + "src" + "A.res.js" = "a/src/A.res.js" *)
                        result
                      end
                    | None -> 
                      dep_pkg.pkg_rel_path // js_file
                  end else begin
                    dep_pkg.pkg_rel_path // js_file
                  end
              (* Note we did a post-processing when working on Windows *)
              | Es6_global 
                ->             
                begin 
                  Ext_path.rel_normalized_absolute_path              
                    ~from:(
                      Js_packages_info.get_output_dir 
                        current_package_info
                        ~package_dir:(Lazy.force Ext_path.package_dir)
                        module_system 
                    )
                    (package_path // dep_pkg.rel_path // js_file)              
                end
            end
        | Package_script, Package_script 
          -> 
          (* Use configured suffix instead of hardcoded .js *)
          let js_file =  
            Ext_namespace.js_name_of_modulename dep_module_id.id.name case !Js_config.default_suffix in 
          match Config_util.find_opt js_file with 
          | Some file -> 
            let basename = Filename.basename file in 
            let dirname = Filename.dirname file in 
            Ext_path.node_rebase_file
              ~from:(
                Ext_path.absolute_cwd_path 
                  output_dir)
              ~to_:(
                Ext_path.absolute_cwd_path 

                  dirname)
              basename  
          | None -> 
            Bs_exception.error (Js_not_found js_file))

#endif
