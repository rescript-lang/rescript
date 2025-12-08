let runConfig = RunConfig.runConfig

(* Location printer: `filename:line: ' *)
let posToString (pos : Lexing.position) =
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (file |> Filename.basename)
  ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col

module Cli = struct
  let debug = ref false
  let ci = ref false

  (** The command was a -cmt variant (e.g. -exception-cmt) *)
  let cmtCommand = ref false

  let experimental = ref false
  let json = ref false

  (* names to be considered live values *)
  let liveNames = ref ([] : string list)

  (* paths of files where all values are considered live *)

  let livePaths = ref ([] : string list)

  (* paths of files to exclude from analysis *)
  let excludePaths = ref ([] : string list)
end

module StringSet = Set.Make (String)

module LocSet = Set.Make (struct
  include Location

  let compare = compare
end)

module FileSet = Set.Make (String)

module FileHash = struct
  include Hashtbl.Make (struct
    type t = string

    let hash (x : t) = Hashtbl.hash x
    let equal (x : t) y = x = y
  end)
end

(* NOTE: FileReferences has been moved to FileDeps module *)

module Path = struct
  type t = Name.t list

  let toName (path : t) =
    path |> List.rev_map Name.toString |> String.concat "." |> Name.create

  let toString path = path |> toName |> Name.toString

  let withoutHead path =
    match
      path |> List.rev_map (fun n -> n |> Name.toInterface |> Name.toString)
    with
    | _ :: tl -> tl |> String.concat "."
    | [] -> ""

  let onOkPath ~whenContainsApply ~f path =
    match path |> Path.flatten with
    | `Ok (id, mods) -> f (Ident.name id :: mods |> String.concat ".")
    | `Contains_apply -> whenContainsApply

  let fromPathT path =
    match path |> Path.flatten with
    | `Ok (id, mods) -> Ident.name id :: mods |> List.rev_map Name.create
    | `Contains_apply -> []

  let moduleToImplementation path =
    match path |> List.rev with
    | moduleName :: rest ->
      (moduleName |> Name.toImplementation) :: rest |> List.rev
    | [] -> path

  let moduleToInterface path =
    match path |> List.rev with
    | moduleName :: rest -> (moduleName |> Name.toInterface) :: rest |> List.rev
    | [] -> path

  let toModuleName ~isType path =
    match path with
    | _ :: tl when not isType -> tl |> toName
    | _ :: _ :: tl when isType -> tl |> toName
    | _ -> "" |> Name.create

  let typeToInterface path =
    match path with
    | typeName :: rest -> (typeName |> Name.toInterface) :: rest
    | [] -> path
end

module OptionalArgs = struct
  type t = {
    mutable count: int;
    mutable unused: StringSet.t;
    mutable alwaysUsed: StringSet.t;
  }

  let empty =
    {unused = StringSet.empty; alwaysUsed = StringSet.empty; count = 0}

  let fromList l =
    {unused = StringSet.of_list l; alwaysUsed = StringSet.empty; count = 0}

  let isEmpty x = StringSet.is_empty x.unused

  let call ~argNames ~argNamesMaybe x =
    let nameSet = argNames |> StringSet.of_list in
    let nameSetMaybe = argNamesMaybe |> StringSet.of_list in
    let nameSetAlways = StringSet.diff nameSet nameSetMaybe in
    if x.count = 0 then x.alwaysUsed <- nameSetAlways
    else x.alwaysUsed <- StringSet.inter nameSetAlways x.alwaysUsed;
    argNames
    |> List.iter (fun name -> x.unused <- StringSet.remove name x.unused);
    x.count <- x.count + 1

  let combine x y =
    let unused = StringSet.inter x.unused y.unused in
    x.unused <- unused;
    y.unused <- unused;
    let alwaysUsed = StringSet.inter x.alwaysUsed y.alwaysUsed in
    x.alwaysUsed <- alwaysUsed;
    y.alwaysUsed <- alwaysUsed

  let iterUnused f x = StringSet.iter f x.unused
  let iterAlwaysUsed f x = StringSet.iter (fun s -> f s x.count) x.alwaysUsed

  let foldUnused f x init = StringSet.fold f x.unused init

  let foldAlwaysUsed f x init =
    StringSet.fold (fun s acc -> f s x.count acc) x.alwaysUsed init
end

module DeclKind = struct
  type t =
    | Exception
    | RecordLabel
    | VariantCase
    | Value of {
        isToplevel: bool;
        mutable optionalArgs: OptionalArgs.t;
        sideEffects: bool;
      }

  let isType dk =
    match dk with
    | RecordLabel | VariantCase -> true
    | Exception | Value _ -> false

  let toString dk =
    match dk with
    | Exception -> "Exception"
    | RecordLabel -> "RecordLabel"
    | VariantCase -> "VariantCase"
    | Value _ -> "Value"
end

type posAdjustment = FirstVariant | OtherVariant | Nothing

type decl = {
  declKind: DeclKind.t;
  moduleLoc: Location.t;
  posAdjustment: posAdjustment;
  path: Path.t;
  pos: Lexing.position;
  posEnd: Lexing.position;
  posStart: Lexing.position;
  mutable resolvedDead: bool option;
  mutable report: bool;
}

module ExnSet = Set.Make (Exn)

type missingThrowInfo = {
  exnName: string;
  exnTable: (Exn.t, LocSet.t) Hashtbl.t;
  locFull: Location.t;
  missingAnnotations: ExnSet.t;
  throwSet: ExnSet.t;
}

type severity = Warning | Error
type deadOptional = WarningUnusedArgument | WarningRedundantOptionalArgument

type termination =
  | ErrorHygiene
  | ErrorNotImplemented
  | ErrorTermination
  | TerminationAnalysisInternal

type deadWarning =
  | WarningDeadException
  | WarningDeadType
  | WarningDeadValue
  | WarningDeadValueWithSideEffects
  | IncorrectDeadAnnotation

type description =
  | Circular of {message: string}
  | ExceptionAnalysis of {message: string}
  | ExceptionAnalysisMissing of missingThrowInfo
  | DeadModule of {message: string}
  | DeadOptional of {deadOptional: deadOptional; message: string}
  | DeadWarning of {deadWarning: deadWarning; path: string; message: string}
  | Termination of {termination: termination; message: string}

type issue = {
  name: string;
  severity: severity;
  loc: Location.t;
  description: description;
}
