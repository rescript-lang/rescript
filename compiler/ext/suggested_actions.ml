type suggested_action =
  | ApplyAutomaticMigrationsForFullProject
  | ApplyAutomaticMigrationsForCurrentFile

type record = {loc: Warnings.loc; action: suggested_action}

let suggestions : record list ref = ref []

let file_of_record (r : record) = r.loc.loc_start.pos_fname

let add r =
  let exists =
    List.exists
      (fun x -> x.action = r.action && file_of_record x = file_of_record r)
      !suggestions
  in
  if not exists then suggestions := r :: !suggestions

let clear () = suggestions := []

let has r =
  List.exists
    (fun x -> x.action = r.action && file_of_record x = file_of_record r)
    !suggestions

let action_id_json = function
  | ApplyAutomaticMigrationsForFullProject ->
    "\"ApplyAutomaticMigrationsForFullProject\""
  | ApplyAutomaticMigrationsForCurrentFile ->
    "\"ApplyAutomaticMigrationsForCurrentFile\""

let print_block_if_any () =
  match List.rev !suggestions with
  | [] -> ()
  | xs ->
    let tag_name_of_action = function
      | ApplyAutomaticMigrationsForCurrentFile -> "apply_all_migrations_in_file"
      | ApplyAutomaticMigrationsForFullProject ->
        "apply_all_migrations_in_project"
    in
    let description_of_action = function
      | ApplyAutomaticMigrationsForCurrentFile ->
        "Applies all automatic migrations in the current file."
      | ApplyAutomaticMigrationsForFullProject ->
        "Applies all automatic migrations in the project."
    in
    let print_entry ({loc; action} : record) =
      let path =
        let f = loc.loc_start.pos_fname in
        if Filename.is_relative f then Ext_path.absolute_cwd_path f else f
      in
      let tag = tag_name_of_action action in
      Printf.printf "  <%s>\n" tag;
      Printf.printf "    <description>%s</description>\n"
        (description_of_action action);
      print_endline "    <command>";
      Printf.printf "      perform-action %s %s\n" path (action_id_json action);
      print_endline "    </command>";
      Printf.printf "  </%s>\n" tag
    in
    print_endline "<suggested_actions>";
    List.iter print_entry xs;
    print_endline "</suggested_actions>";
    flush stdout
