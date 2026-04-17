module StringSet = Set.Make (String)

type severity = SeverityError | SeverityWarning

type finding = {
  rule: string;
  path: string;
  range: int * int * int * int;
  severity: severity;
  message: string;
  symbol: string option;
}

type raw_finding = {
  rule: string;
  abs_path: string;
  loc: Location.t;
  severity: severity;
  message: string;
  symbol: string option;
}

type single_use_function_rule = {enabled: bool; severity: severity}

type forbidden_reference_rule = {
  enabled: bool;
  severity: severity;
  items: string list list;
}

type config = {
  forbidden_reference: forbidden_reference_rule;
  single_use_function: single_use_function_rule;
}

type ast_summary = {
  parse_errors: raw_finding list;
  local_function_bindings: StringSet.t;
}

let severity_to_string = function
  | SeverityError -> "error"
  | SeverityWarning -> "warning"

let severity_of_string = function
  | "error" -> Some SeverityError
  | "warning" -> Some SeverityWarning
  | _ -> None

let loc_key ({Location.loc_start; loc_end} : Location.t) =
  Printf.sprintf "%d:%d:%d:%d" loc_start.pos_lnum
    (loc_start.pos_cnum - loc_start.pos_bol)
    loc_end.pos_lnum
    (loc_end.pos_cnum - loc_end.pos_bol)

let finding_of_raw ~display_base (finding : raw_finding) =
  {
    rule = finding.rule;
    path = Lint_support.Path.display ~base:display_base finding.abs_path;
    range = Lint_support.Range.of_loc finding.loc;
    severity = finding.severity;
    message = finding.message;
    symbol = finding.symbol;
  }

let raw_finding ~rule ~abs_path ~loc ~severity ~message ?symbol () =
  {rule; abs_path; loc; severity; message; symbol}
