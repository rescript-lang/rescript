open SharedTypes

type 'a table = (string * (int * int), 'a Declared.t) Hashtbl.t
type namesUsed = (string, unit) Hashtbl.t

type t = {
  namesUsed: namesUsed;
  mutable resultRev: Completion.t list;
  constructorTable: Constructor.t table;
  modulesTable: Module.t table;
  typesTable: Type.t table;
  valueTable: Types.type_expr table;
  includedValueTable: (string * Types.type_expr) table;
}

let create () =
  {
    namesUsed = Hashtbl.create 1;
    resultRev = [];
    constructorTable = Hashtbl.create 1;
    modulesTable = Hashtbl.create 1;
    typesTable = Hashtbl.create 1;
    valueTable = Hashtbl.create 1;
    includedValueTable = Hashtbl.create 1;
  }

let populateValues ~env localTables =
  env.QueryEnv.file.stamps
  |> Stamps.iterValues (fun _ declared ->
         Hashtbl.replace localTables.valueTable
           (declared.name.txt, declared.name.loc |> Loc.start)
           declared)

let populateIncludedValues ~env localTables =
  env.QueryEnv.file.stamps
  |> Stamps.iterValues (fun _ declared ->
         match declared.modulePath with
         | ModulePath.IncludedModule (source, _) ->
           let path = Path.name source in
           let declared = {declared with item = (path, declared.item)} in
           Hashtbl.replace localTables.includedValueTable
             (declared.name.txt, declared.name.loc |> Loc.start)
             declared
         | _ -> ())

let populateConstructors ~env localTables =
  env.QueryEnv.file.stamps
  |> Stamps.iterConstructors (fun _ declared ->
         Hashtbl.replace localTables.constructorTable
           (declared.name.txt, declared.extentLoc |> Loc.start)
           declared)

let populateTypes ~env localTables =
  env.QueryEnv.file.stamps
  |> Stamps.iterTypes (fun _ declared ->
         Hashtbl.replace localTables.typesTable
           (declared.name.txt, declared.name.loc |> Loc.start)
           declared)

let populateModules ~env localTables =
  env.QueryEnv.file.stamps
  |> Stamps.iterModules (fun _ declared ->
         Hashtbl.replace localTables.modulesTable
           (declared.name.txt, declared.extentLoc |> Loc.start)
           declared)
