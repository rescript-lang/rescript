open Analysis
open Lint_shared

type run_result = {output: string}

let stringify_setting_value_json = function
  | RuleSettingBool value -> string_of_bool value
  | RuleSettingString value -> Protocol.wrapInQuotes value
  | RuleSettingStringList values ->
    Protocol.array (values |> List.map Protocol.wrapInQuotes)

let stringify_setting_json (key, value) =
  (key, Some (stringify_setting_value_json value))

let stringify_settings_json settings =
  Lint_support.Json.stringify_compact_object
    (settings |> List.map stringify_setting_json)

let stringify_setting_text (key, value) =
  Printf.sprintf "- %s: %s" key (rule_setting_value_to_text value)

let stringify_rule_json (listed_rule : rule_listing) =
  Lint_support.Json.stringify_compact_object
    [
      ("namespace", Some (Protocol.wrapInQuotes listed_rule.namespace));
      ("rule", Some (Protocol.wrapInQuotes listed_rule.rule));
      ("instance", listed_rule.instance |> Option.map string_of_int);
      ("active", Some (string_of_bool listed_rule.active));
      ("summary", Some (Protocol.wrapInQuotes listed_rule.summary));
      ("details", Some (Protocol.wrapInQuotes listed_rule.details));
      ("settings", Some (stringify_settings_json listed_rule.settings));
    ]

let stringify_settings_text settings =
  settings |> List.map stringify_setting_text

let stringify_rule_text (listed_rule : rule_listing) =
  let lines =
    [
      Printf.sprintf "namespace: %s" listed_rule.namespace;
      Printf.sprintf "rule: %s" listed_rule.rule;
    ]
  in
  let lines =
    match listed_rule.instance with
    | None -> lines
    | Some instance -> lines @ [Printf.sprintf "instance: %d" instance]
  in
  let lines =
    lines
    @ [
        Printf.sprintf "active: %s" (string_of_bool listed_rule.active);
        Printf.sprintf "summary: %s" listed_rule.summary;
        Printf.sprintf "details: %s" listed_rule.details;
      ]
  in
  let lines =
    if listed_rule.settings = [] then lines
    else lines @ ("settings:" :: stringify_settings_text listed_rule.settings)
  in
  String.concat "\n" lines

let render_summary_json listed_rules =
  let total = List.length listed_rules in
  let active =
    listed_rules
    |> List.fold_left
         (fun count (listed_rule : rule_listing) ->
           if listed_rule.active then count + 1 else count)
         0
  in
  let inactive = total - active in
  Lint_support.Json.stringify_compact_object
    [
      ("active", Some (string_of_int active));
      ("inactive", Some (string_of_int inactive));
      ("total", Some (string_of_int total));
    ]

let render_summary_text listed_rules =
  let total = List.length listed_rules in
  let active =
    listed_rules
    |> List.fold_left
         (fun count (listed_rule : rule_listing) ->
           if listed_rule.active then count + 1 else count)
         0
  in
  let inactive = total - active in
  Printf.sprintf "summary: %d active, %d inactive, %d total" active inactive
    total

let render ~json listed_rules =
  if json then
    Lint_support.Json.stringify_compact_object
      [
        ( "rules",
          Some
            ("["
            ^ String.concat "," (listed_rules |> List.map stringify_rule_json)
            ^ "]") );
        ("summary", Some (render_summary_json listed_rules));
      ]
  else
    String.concat "\n\n"
      ((listed_rules |> List.map stringify_rule_text)
      @ [render_summary_text listed_rules])

let run ?config_path ?(json = false) target =
  let target =
    if Filename.is_relative target then Filename.concat (Sys.getcwd ()) target
    else target
  in
  if not (Files.exists target) then
    Error ("error: no such file or directory: " ^ target)
  else
    let target = Unix.realpath target in
    Lint_config.load ?config_path target
    |> Result.map (fun config ->
           {output = rule_listings_of_config config |> render ~json})
