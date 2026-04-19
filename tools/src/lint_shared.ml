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

type single_use_function_rule = {
  enabled: bool;
  severity: severity;
  message: string option;
}

type forbidden_reference_kind =
  | ForbiddenReferenceModule
  | ForbiddenReferenceValue
  | ForbiddenReferenceType

type forbidden_reference_item = {
  kind: forbidden_reference_kind;
  path: string list;
  message: string option;
}

type forbidden_reference_rule = {
  enabled: bool;
  severity: severity;
  message: string option;
  items: forbidden_reference_item list;
}

type alias_avoidance_rule = {
  enabled: bool;
  severity: severity;
  message: string option;
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
  forbidden_reference: forbidden_reference_rule list;
  single_use_function: single_use_function_rule;
  alias_avoidance: alias_avoidance_rule;
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
  instance: int option;
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

let forbidden_reference_kind_to_string = function
  | ForbiddenReferenceModule -> "module"
  | ForbiddenReferenceValue -> "value"
  | ForbiddenReferenceType -> "type"

let forbidden_reference_default_message = "Forbidden reference"

let single_use_function_default_message = "Local function is only used once"

let alias_avoidance_default_message =
  "Use the fully qualified reference directly instead of creating a local alias"

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

let alias_avoidance_rule_info =
  {
    namespace = "lint";
    rule = "alias-avoidance";
    summary =
      "Report local aliases that only shorten an existing qualified value, \
       type, or module reference.";
    details =
      "Flags pass-through aliases such as `let alias = Module.value`, `type \
       alias = Module.t`, and `module Alias = Long.Module.Path` so the \
       qualified reference can be used directly instead.";
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
    alias_avoidance_rule_info;
    prefer_switch_rule_info;
    no_optional_some_rule_info;
  ]

let rewrite_rule_infos =
  configurable_rule_infos
  |> List.filter (fun (rule_info : rule_info) ->
         rule_info.namespace = "rewrite")

let rewrite_note_for_rule rule =
  rewrite_rule_infos
  |> List.find_map (fun (rule_info : rule_info) ->
         if rule_info.rule = rule then rule_info.rewrite_note else None)
  |> Option.value ~default:"applied rewrite rule"

let effective_forbidden_reference_message (rule : forbidden_reference_rule) =
  Option.value rule.message ~default:forbidden_reference_default_message

let effective_forbidden_reference_item_message
    (rule : forbidden_reference_rule) (item : forbidden_reference_item) =
  Option.value item.message ~default:(effective_forbidden_reference_message rule)

let forbidden_reference_message_settings (rule : forbidden_reference_rule) =
  let has_item_message =
    rule.items |> List.exists (fun (item : forbidden_reference_item) ->
      item.message <> None)
  in
  let has_item_without_message =
    rule.items |> List.exists (fun (item : forbidden_reference_item) ->
      item.message = None)
  in
  match rule.message with
  | Some message -> [("message", RuleSettingString message)]
  | None when has_item_message && not has_item_without_message -> []
  | None when has_item_message ->
    [("default-message", RuleSettingString forbidden_reference_default_message)]
  | None -> [("message", RuleSettingString forbidden_reference_default_message)]

let effective_single_use_function_message (rule : single_use_function_rule) =
  Option.value rule.message ~default:single_use_function_default_message

let effective_alias_avoidance_message (rule : alias_avoidance_rule) =
  Option.value rule.message ~default:alias_avoidance_default_message

let rule_listings_of_config (config : config) =
  let instance_if_many total index =
    if total > 1 then Some (index + 1) else None
  in
  let forbidden_reference_listings =
    config.forbidden_reference
    |> List.mapi (fun index (rule : forbidden_reference_rule) ->
           let item_settings =
             rule.items
             |> List.mapi (fun item_index (item : forbidden_reference_item) ->
                    let prefix =
                      Printf.sprintf "item[%d]" (item_index + 1)
                    in
                    let base_settings =
                      [
                        ( prefix ^ ".kind",
                          RuleSettingString
                            (forbidden_reference_kind_to_string item.kind) );
                        (prefix ^ ".path", RuleSettingString (String.concat "." item.path));
                      ]
                    in
                    match item.message with
                    | None -> base_settings
                    | Some message ->
                      base_settings
                      @ [(prefix ^ ".message", RuleSettingString message)])
             |> List.concat
           in
           {
             namespace = forbidden_reference_rule_info.namespace;
             rule = forbidden_reference_rule_info.rule;
             instance =
               instance_if_many (List.length config.forbidden_reference) index;
             active = rule.enabled && rule.items <> [];
             summary = forbidden_reference_rule_info.summary;
             details = forbidden_reference_rule_info.details;
             settings =
               [
                 ("enabled", RuleSettingBool rule.enabled);
                 ( "severity",
                   RuleSettingString (severity_to_string rule.severity) );
               ]
               @ forbidden_reference_message_settings rule
               @ item_settings;
           })
  in
  let single_use_function_listings =
    let rule = config.single_use_function in
    [
      {
        namespace = single_use_function_rule_info.namespace;
        rule = single_use_function_rule_info.rule;
        instance = None;
        active = rule.enabled;
        summary = single_use_function_rule_info.summary;
        details = single_use_function_rule_info.details;
        settings =
          [
            ("enabled", RuleSettingBool rule.enabled);
            ("severity", RuleSettingString (severity_to_string rule.severity));
            ( "message",
              RuleSettingString (effective_single_use_function_message rule) );
          ];
      };
    ]
  in
  let alias_avoidance_listings =
    let rule = config.alias_avoidance in
    [
      {
        namespace = alias_avoidance_rule_info.namespace;
        rule = alias_avoidance_rule_info.rule;
        instance = None;
        active = rule.enabled;
        summary = alias_avoidance_rule_info.summary;
        details = alias_avoidance_rule_info.details;
        settings =
          [
            ("enabled", RuleSettingBool rule.enabled);
            ("severity", RuleSettingString (severity_to_string rule.severity));
            ( "message",
              RuleSettingString (effective_alias_avoidance_message rule) );
          ];
      };
    ]
  in
  forbidden_reference_listings @ single_use_function_listings
  @ alias_avoidance_listings
  @ [
      {
        namespace = prefer_switch_rule_info.namespace;
        rule = prefer_switch_rule_info.rule;
        instance = None;
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
            ( "ternary",
              RuleSettingBool config.rewrite.prefer_switch.rewrite_ternary );
          ];
      };
      {
        namespace = no_optional_some_rule_info.namespace;
        rule = no_optional_some_rule_info.rule;
        instance = None;
        active = config.rewrite.no_optional_some.enabled;
        summary = no_optional_some_rule_info.summary;
        details = no_optional_some_rule_info.details;
        settings =
          [("enabled", RuleSettingBool config.rewrite.no_optional_some.enabled)];
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
