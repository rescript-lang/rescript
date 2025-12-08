(** Analysis result - immutable output from the solver.
    
    The solver returns this instead of logging directly.
    All side effects (logging, JSON output) happen in the reporting phase. *)

open Common

type t = {issues: issue list}
(** Immutable analysis result *)

let empty = {issues = []}

let add_issue result issue = {issues = issue :: result.issues}

let add_issues result new_issues =
  {issues = List.rev_append new_issues result.issues}

let get_issues result = result.issues |> List.rev

let issue_count result = List.length result.issues

(** Create a dead code issue *)
let make_dead_issue ~loc ~deadWarning ~path ~message =
  {
    name =
      (match deadWarning with
      | WarningDeadException -> "Warning Dead Exception"
      | WarningDeadType -> "Warning Dead Type"
      | WarningDeadValue -> "Warning Dead Value"
      | WarningDeadValueWithSideEffects ->
        "Warning Dead Value With Side Effects"
      | IncorrectDeadAnnotation -> "Incorrect Dead Annotation");
    severity = Warning;
    loc;
    description = DeadWarning {deadWarning; path; message};
  }

(** Create a dead module issue *)
let make_dead_module_issue ~loc ~moduleName =
  {
    name = "Warning Dead Module";
    severity = Warning;
    loc;
    description =
      DeadModule
        {
          message =
            Format.asprintf "@{<info>%s@} %s"
              (moduleName |> Name.toInterface |> Name.toString)
              "is a dead module as all its items are dead.";
        };
  }
