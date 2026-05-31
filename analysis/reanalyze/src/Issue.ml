(** Issue types for dead code analysis.
    
    These types represent the various issues that can be reported. *)

module ExnSet = Set.Make (Exn)

type missing_throw_info = {
  exn_name: string;
  exn_table: (Exn.t, LocSet.t) Hashtbl.t;
  loc_full: Location.t;
  missing_annotations: ExnSet.t;
  throw_set: ExnSet.t;
}

type severity = Warning | Error
type dead_optional = WarningUnusedArgument | WarningRedundantOptionalArgument

type termination =
  | ErrorHygiene
  | ErrorNotImplemented
  | ErrorTermination
  | TerminationAnalysisInternal

type dead_warning =
  | WarningDeadException
  | WarningDeadType
  | WarningDeadValue
  | WarningDeadValueWithSideEffects
  | IncorrectDeadAnnotation

type description =
  | Circular of {message: string}
  | ExceptionAnalysis of {message: string}
  | ExceptionAnalysisMissing of missing_throw_info
  | DeadModule of {message: string}
  | DeadOptional of {dead_optional: dead_optional; message: string}
  | DeadWarning of {dead_warning: dead_warning; path: string; message: string}
  | Termination of {termination: termination; message: string}

type t = {
  name: string;
  severity: severity;
  loc: Location.t;
  description: description;
}
