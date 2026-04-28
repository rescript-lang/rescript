(** Issue types for dead code analysis.
    
    These types represent the various issues that can be reported. *)

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

type t = {
  name: string;
  severity: severity;
  loc: Location.t;
  description: description;
}
