(** Analysis result - immutable output from the solver.
    
    The solver returns this instead of logging directly.
    All side effects (logging, JSON output) happen in the reporting phase. *)

type t = {issues: Issue.t list}
(** Immutable analysis result *)

let empty = {issues = []}

let add_issue result issue = {issues = issue :: result.issues}

let add_issues result new_issues =
  {issues = List.rev_append new_issues result.issues}

(* Issues are collected by iterating reactive collections, whose order follows
   the hash of their keys - and [issues_by_file] is keyed by absolute path, so
   the same project reports in a different order on another machine. Order the
   report by source position instead, keyed on the basename so it does not
   depend on where the project is checked out. *)
let issue_sort_key (issue : Issue.t) =
  let pos = issue.Issue.loc.Location.loc_start in
  ( Filename.basename pos.Lexing.pos_fname,
    pos.pos_lnum,
    pos.pos_cnum - pos.pos_bol,
    issue.Issue.name,
    pos.pos_fname )

let get_issues result =
  result.issues |> List.rev
  |> List.stable_sort (fun a b -> compare (issue_sort_key a) (issue_sort_key b))

let issue_count result = List.length result.issues

(** Create a dead code issue *)
let make_dead_issue ~loc ~dead_warning ~path ~message : Issue.t =
  {
    Issue.name =
      (match dead_warning with
      | Issue.WarningDeadException -> "Warning Dead Exception"
      | WarningDeadType -> "Warning Dead Type"
      | WarningDeadValue -> "Warning Dead Value"
      | WarningDeadValueWithSideEffects ->
        "Warning Dead Value With Side Effects"
      | IncorrectDeadAnnotation -> "Incorrect Dead Annotation");
    severity = Warning;
    loc;
    description = DeadWarning {dead_warning; path; message};
  }

(** Create a dead module issue *)
let make_dead_module_issue ~loc ~module_name : Issue.t =
  {
    Issue.name = "Warning Dead Module";
    severity = Warning;
    loc;
    description =
      DeadModule
        {
          message =
            Format.asprintf "@{<info>%s@} %s"
              (module_name |> Name.to_interface |> Name.to_string)
              "is a dead module as all its items are dead.";
        };
  }
