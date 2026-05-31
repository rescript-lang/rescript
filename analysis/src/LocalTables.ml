open SharedTypes

type 'a table = (string * (int * int), 'a Declared.t) Hashtbl.t
type names_used = (string, unit) Hashtbl.t

type t = {
  names_used: names_used;
  mutable result_rev: Completion.t list;
  constructor_table: Constructor.t table;
  modules_table: Module.t table;
  types_table: Type.t table;
  value_table: Types.type_expr table;
  included_value_table: (string * Types.type_expr) table;
}

let create () =
  {
    names_used = Hashtbl.create 1;
    result_rev = [];
    constructor_table = Hashtbl.create 1;
    modules_table = Hashtbl.create 1;
    types_table = Hashtbl.create 1;
    value_table = Hashtbl.create 1;
    included_value_table = Hashtbl.create 1;
  }

let populate_values ~env local_tables =
  env.QueryEnv.file.stamps
  |> Stamps.iter_values (fun _ declared ->
         Hashtbl.replace local_tables.value_table
           (declared.name.txt, declared.name.loc |> Loc.start)
           declared)

let populate_included_values ~env local_tables =
  env.QueryEnv.file.stamps
  |> Stamps.iter_values (fun _ declared ->
         match declared.module_path with
         | ModulePath.IncludedModule (source, _) ->
           let path = Path.name source in
           let declared = {declared with item = (path, declared.item)} in
           Hashtbl.replace local_tables.included_value_table
             (declared.name.txt, declared.name.loc |> Loc.start)
             declared
         | _ -> ())

let populate_constructors ~env local_tables =
  env.QueryEnv.file.stamps
  |> Stamps.iter_constructors (fun _ declared ->
         Hashtbl.replace local_tables.constructor_table
           (declared.name.txt, declared.extent_loc |> Loc.start)
           declared)

let populate_types ~env local_tables =
  env.QueryEnv.file.stamps
  |> Stamps.iter_types (fun _ declared ->
         Hashtbl.replace local_tables.types_table
           (declared.name.txt, declared.name.loc |> Loc.start)
           declared)

let populate_modules ~env local_tables =
  env.QueryEnv.file.stamps
  |> Stamps.iter_modules (fun _ declared ->
         Hashtbl.replace local_tables.modules_table
           (declared.name.txt, declared.extent_loc |> Loc.start)
           declared)
