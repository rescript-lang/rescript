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

type prefer_switch_rule = {
  enabled: bool;
  rewrite_if: bool;
  rewrite_ternary: bool;
}

type no_optional_some_rule = {enabled: bool}

type rewrite_config = {
  prefer_switch: prefer_switch_rule;
  no_optional_some: no_optional_some_rule;
}

type config = {
  forbidden_reference: forbidden_reference_rule;
  single_use_function: single_use_function_rule;
  rewrite: rewrite_config;
}

type rule_info = {
  namespace: string;
  rule: string;
  summary: string;
  details: string;
  rewrite_note: string option;
}

type rule_listing = {
  namespace: string;
  rule: string;
  active: bool;
  summary: string;
  details: string;
  settings: (string * rule_setting_value) list;
}

and rule_setting_value =
  | RuleSettingBool of bool
  | RuleSettingString of string
  | RuleSettingStringList of string list

type ast_summary = {
  parse_errors: raw_finding list;
  local_function_bindings: StringSet.t;
}

let severity_to_string = function
  | SeverityError -> "error"
  | SeverityWarning -> "warning"

let rule_setting_value_to_text = function
  | RuleSettingBool value -> string_of_bool value
  | RuleSettingString value -> value
  | RuleSettingStringList [] -> "(none)"
  | RuleSettingStringList values -> String.concat ", " values

let forbidden_reference_rule_info =
  {
    namespace = "lint";
    rule = "forbidden-reference";
    summary = "Report references to configured modules, values, and types.";
    details =
      "Uses typed references when available so opened modules and aliases \
       resolve to the real symbol path before matching.";
    rewrite_note = None;
  }

let single_use_function_rule_info =
  {
    namespace = "lint";
    rule = "single-use-function";
    summary =
      "Report local non-exported functions that are defined once and used once.";
    details =
      "Counts local function bindings and same-file typed references, then \
       reports helpers that only have a single real use site.";
    rewrite_note = None;
  }

let prefer_switch_rule_info =
  {
    namespace = "rewrite";
    rule = "prefer-switch";
    summary = "Rewrite `if` and ternary control flow into canonical `switch`.";
    details =
      "Simple boolean branches become `switch condition`, and `else if` chains \
       collapse into guarded `switch ()` cases.";
    rewrite_note = Some "rewrote `if` / ternary branches into `switch`";
  }

let no_optional_some_rule_info =
  {
    namespace = "rewrite";
    rule = "no-optional-some";
    summary =
      "Rewrite redundant optional-argument wrapping from `?Some(expr)` to the \
       direct labeled form.";
    details =
      "Turns `~label=?Some(expr)` into `~label=expr` when the argument is \
       already in an optional position.";
    rewrite_note = Some "rewrote `~label=?Some(expr)` into `~label=expr`";
  }

let configurable_rule_infos =
  [
    forbidden_reference_rule_info;
    single_use_function_rule_info;
    prefer_switch_rule_info;
    no_optional_some_rule_info;
  ]

let rewrite_rule_infos =
  configurable_rule_infos
  |> List.filter (fun (rule_info : rule_info) -> rule_info.namespace = "rewrite")

let rewrite_note_for_rule rule =
  rewrite_rule_infos
  |> List.find_map (fun (rule_info : rule_info) ->
         if rule_info.rule = rule then rule_info.rewrite_note else None)
  |> Option.value ~default:"applied rewrite rule"

let rule_listings_of_config (config : config) =
  let item_paths = config.forbidden_reference.items |> List.map (String.concat ".") in
  [
    {
      namespace = forbidden_reference_rule_info.namespace;
      rule = forbidden_reference_rule_info.rule;
      active =
        config.forbidden_reference.enabled && config.forbidden_reference.items <> [];
      summary = forbidden_reference_rule_info.summary;
      details = forbidden_reference_rule_info.details;
      settings =
        [
          ("enabled", RuleSettingBool config.forbidden_reference.enabled);
          ( "severity",
            RuleSettingString
              (severity_to_string config.forbidden_reference.severity) );
          ("items", RuleSettingStringList item_paths);
        ];
    };
    {
      namespace = single_use_function_rule_info.namespace;
      rule = single_use_function_rule_info.rule;
      active = config.single_use_function.enabled;
      summary = single_use_function_rule_info.summary;
      details = single_use_function_rule_info.details;
      settings =
        [
          ("enabled", RuleSettingBool config.single_use_function.enabled);
          ( "severity",
            RuleSettingString
              (severity_to_string config.single_use_function.severity) );
        ];
    };
    {
      namespace = prefer_switch_rule_info.namespace;
      rule = prefer_switch_rule_info.rule;
      active =
        config.rewrite.prefer_switch.enabled
        && (config.rewrite.prefer_switch.rewrite_if
           || config.rewrite.prefer_switch.rewrite_ternary);
      summary = prefer_switch_rule_info.summary;
      details = prefer_switch_rule_info.details;
      settings =
        [
          ("enabled", RuleSettingBool config.rewrite.prefer_switch.enabled);
          ("if", RuleSettingBool config.rewrite.prefer_switch.rewrite_if);
          ("ternary", RuleSettingBool config.rewrite.prefer_switch.rewrite_ternary);
        ];
    };
    {
      namespace = no_optional_some_rule_info.namespace;
      rule = no_optional_some_rule_info.rule;
      active = config.rewrite.no_optional_some.enabled;
      summary = no_optional_some_rule_info.summary;
      details = no_optional_some_rule_info.details;
      settings = [("enabled", RuleSettingBool config.rewrite.no_optional_some.enabled)];
    };
  ]

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
