(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none

type dir_index = int 
let lib_dir_index = 0

let get_dev_index, get_current_number_of_dev_groups =
  let dir_index = ref 0 in 
  ((fun () -> incr dir_index ; !dir_index),
   (fun _ -> !dir_index ))



(** 
   0 : lib 
   1 : dev 1 
   2 : dev 2 
*)  
type  file_group = 
  { dir : string ;
    sources : Binary_cache.file_group_rouces; 
    resources : string list ;
    bs_dependencies : string list ;
    public : public ;
    dir_index : dir_index 
  } 

(**
    [intervals] are used for side effect so we can patch `bsconfig.json` to add new files 
     we need add a new line in the end,
     otherwise it will be idented twice
*)

type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;    
    globbed_dirs : string list ; 
  }

let (//) = Ext_filename.combine

let (|?)  m (key, cb) =
  m  |> Ext_json.test key cb 

let get_list_string  =  Bsb_build_util.get_list_string



let print_arrays file_array oc offset  =
  let indent = String.make offset ' ' in 
  let p_str s = 
    output_string oc indent ; 
    output_string oc s ;
    output_string oc "\n"
  in
  let len = String_vec.length file_array in 
  match len with 
  | 0
    -> output_string oc "[ ]\n"
  | 1 
    -> output_string oc ("[ \"" ^ String_vec.get file_array 0  ^ "\" ]\n")
  | _ (* first::(_::_ as rest) *)
    -> 
    output_string oc "[ \n";
    String_vec.iter_range ~from:0 ~to_:(len - 2 ) 
      (fun s -> p_str @@ "\"" ^ s ^ "\",") file_array;
    p_str @@ "\"" ^ (String_vec.last file_array) ^ "\"";

    p_str "]" 


let warning_unused_file : _ format = "WARNING: file %s under %s is ignored due to that it is not a valid module name"

let  handle_list_files dir  loc_start loc_end : Ext_file_pp.interval list * _ =  
  (** detect files to be populated later  *)
  let files_array = Bsb_dir.readdir dir  in 
  let dyn_file_array = String_vec.make (Array.length files_array) in 
  let files  =
    Array.fold_left (fun acc name -> 
        match Ext_string.is_valid_source_name name with 
        | Good ->   begin 
            let new_acc = Binary_cache.map_update ~dir acc name  in 
            String_vec.push name dyn_file_array ;
            new_acc 
          end 
        | Invalid_module_name -> 
          print_endline 
            (Printf.sprintf warning_unused_file
               name dir 
            ) ; 
          acc 
        | Suffix_mismatch -> acc 
      ) String_map.empty files_array in 
  [{Ext_file_pp.loc_start ;
    loc_end; action = (`print (print_arrays dyn_file_array))}],
  files






let empty = { files = []; intervals  = []; globbed_dirs = [];  }



let (++) (u : t)  (v : t)  = 
  if u == empty then v 
  else if v == empty then u 
  else 
    {
      files = u.files @ v.files ; 
      intervals = u.intervals @ v.intervals ; 
      globbed_dirs = u.globbed_dirs @ v.globbed_dirs ; 
    }


(** [dir_index] can be inherited  *)
let rec 
  parsing_simple_dir dir_index  cwd dir =
  parsing_source dir_index cwd 
    (`Obj (String_map.singleton Bsb_build_schemas.dir dir))
and parsing_source (dir_index : int) cwd (x : Ext_json.t )
  : t  =
  match x with 
  | `Str _ as dir -> 
    parsing_simple_dir dir_index cwd dir   
  | `Obj x -> 

    let cur_sources = ref String_map.empty in
    let resources = ref [] in 
    let bs_dependencies = ref [] in
    let public = ref Export_all in (* TODO: move to {!Bsb_default} later*)

    let current_dir_index = 
      match String_map.find_opt Bsb_build_schemas.type_ x with 
      | Some (`Str {str="dev"}) -> get_dev_index ()
      | Some _ -> failwith "type only support dev"    
      | None -> dir_index in 
    if !Bsb_config.no_dev && current_dir_index <> lib_dir_index then empty 
    else 
      let dir = 
        match String_map.find_opt Bsb_build_schemas.dir x with 
        | Some (`Str{str=s}) -> 
          cwd // Ext_filename.simple_convert_node_path_to_os_path s 
        | Some _ -> failwith "dir expected to be a string"
        | None -> cwd   (* TODO: It is an error here? *)
      in
      let cur_update_queue = ref [] in 
      let cur_globbed_dirs = ref [] in 
      begin match String_map.find_opt Bsb_build_schemas.files x with 
        | Some (`Arr {loc_start;loc_end; content = [||] }) -> (* [ ] *) 
          let tasks, files =  handle_list_files  dir  loc_start loc_end in
          cur_update_queue := tasks ;
          cur_sources := files
        | Some (`Arr {loc_start;loc_end; content = s }) -> (* [ a,b ] *)      
          cur_sources := 
            Array.fold_left (fun acc (s : Ext_json.t) ->
                match s with 
                | `Str {str = s} -> 
                  Binary_cache.map_update ~dir acc s
                | _ -> acc
              ) String_map.empty s    
        | Some (`Obj m) -> (* { excludes : [], slow_re : "" }*)
          let excludes = 
            match String_map.find_opt Bsb_build_schemas.excludes m with 
            | None -> []   
            | Some (`Arr {content = arr}) -> get_list_string arr 
            | Some _ -> failwith "excludes expect array "in 
          let slow_re = String_map.find_opt Bsb_build_schemas.slow_re m in 
          let predicate = 
            match slow_re, excludes with 
            | Some (`Str {str = s}), [] -> 
              let re = Str.regexp s  in 
              fun name -> Str.string_match re name 0 
            | Some (`Str {str = s}) , _::_ -> 
              let re = Str.regexp s in   
              fun name -> Str.string_match re name 0 && not (List.mem name excludes)
            | Some _, _ -> failwith "slow-re expect a string literal"
            | None , _ -> failwith "missing field: slow-re"  in 
          let file_array = Bsb_dir.readdir dir in 
          cur_sources := Array.fold_left (fun acc name -> 
              if predicate name then 
                Binary_cache.map_update  ~dir acc name 
              else acc
            ) String_map.empty file_array;
          cur_globbed_dirs := [dir]              
        | None ->  (* No setting on [!files]*)
          let file_array = Bsb_dir.readdir dir in 
          (** We should avoid temporary files *)
          cur_sources := 
            Array.fold_left (fun acc name -> 
                match Ext_string.is_valid_source_name name with 
                | Good -> 
                  Binary_cache.map_update  ~dir acc name 
                | Invalid_module_name -> 
                  print_endline 
                    (Printf.sprintf warning_unused_file
                       name dir 
                    ) ; 
                  acc 
                | Suffix_mismatch ->  acc
              ) String_map.empty file_array;
          cur_globbed_dirs :=  [dir]  
        | Some _ -> failwith "files field expect array or object "

      end;
      x   
      |? (Bsb_build_schemas.bs_dependencies, `Arr (fun s -> bs_dependencies := get_list_string s ))
      |?  (Bsb_build_schemas.resources ,
           `Arr (fun s  ->
               resources := get_list_string s
             ))
      |? (Bsb_build_schemas.public, `Str (fun s -> 
          if s = Bsb_build_schemas.export_all then public := Export_all else 
          if s = Bsb_build_schemas.export_none then public := Export_none else 
            failwith ("invalid str for" ^ s )
        ))
      |? (Bsb_build_schemas.public, `Arr (fun s -> 
          public := Export_set (String_set.of_list (get_list_string s ) )
        ) )
      |> ignore ;
      let cur_file = 
        {dir = dir; 
         sources = !cur_sources; 
         resources = !resources;
         bs_dependencies = !bs_dependencies;
         public = !public;
         dir_index = current_dir_index;
        } in 
      let children, children_update_queue, children_globbed_dirs = 
        match String_map.find_opt Bsb_build_schemas.subdirs x with 
        | Some s -> 
          let res  = parsing_sources current_dir_index dir s in 
          res.files ,
          res.intervals,
          res.globbed_dirs
        | None -> [], [], []  in 

      {
        files =  cur_file :: children;
        intervals = !cur_update_queue @ children_update_queue ;
        globbed_dirs = !cur_globbed_dirs @ children_globbed_dirs;
      } 
  | _ -> empty 
(* and parsing_simple_dir dir_index cwd  dir  : t = 
   parsing_source dir_index cwd (String_map.singleton Bsb_build_schemas.dir dir)
*)

and  parsing_arr_sources dir_index cwd (file_groups : Ext_json.t array)  = 
  Array.fold_left (fun  origin x ->
      parsing_source dir_index cwd x ++ origin 
    ) empty  file_groups 

and  parsing_sources dir_index cwd (sources : Ext_json.t )  = 
  match sources with   
  | `Arr file_groups -> 
    parsing_arr_sources dir_index cwd file_groups.Ext_json.content
  | _ -> parsing_source dir_index cwd sources



