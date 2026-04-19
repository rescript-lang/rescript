open Analysis
open Lint_shared

let default_config =
  {
    forbidden_reference = {enabled = true; severity = SeverityError; items = []};
    single_use_function = {enabled = true; severity = SeverityWarning};
    rewrite =
      {
        prefer_switch =
          {enabled = true; rewrite_if = true; rewrite_ternary = true};
        no_optional_some = {enabled = true};
      };
  }

let parse_rule_severity ~default json =
  match Option.bind (json |> Json.get "severity") Json.string with
  | None -> default
  | Some severity -> Option.value ~default (severity_of_string severity)

let parse_config_json json =
  let lint_json = json |> Json.get "lint" in
  let rules =
    match Option.bind lint_json (Json.get "rules") with
    | Some (Json.Object _) as rules -> rules
    | _ -> (
      match json |> Json.get "rules" with
      | Some (Json.Object _) as rules -> rules
      | _ -> None)
  in
  let get_rule name =
    match rules with
    | Some rules -> rules |> Json.get name
    | None -> None
  in
  let rewrite_json = json |> Json.get "rewrite" in
  let rewrite_rules =
    match Option.bind rewrite_json (Json.get "rules") with
    | Some (Json.Object _) as rules -> rules
    | _ -> None
  in
  let get_rewrite_rule name =
    match rewrite_rules with
    | Some rules -> rules |> Json.get name
    | None -> None
  in
  let forbidden_reference =
    match get_rule "forbidden-reference" with
    | Some rule ->
      {
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
        severity =
          parse_rule_severity
            ~default:default_config.forbidden_reference.severity rule;
        items =
          Lint_support.Json.string_array rule "items"
          |> List.map (fun item ->
                 item |> String.split_on_char '.'
                 |> List.filter (fun segment -> segment <> ""));
      }
    | None -> default_config.forbidden_reference
  in
  let single_use_function =
    match get_rule "single-use-function" with
    | Some rule ->
      {
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
        severity =
          parse_rule_severity
            ~default:default_config.single_use_function.severity rule;
      }
    | None -> default_config.single_use_function
  in
  let prefer_switch =
    match get_rewrite_rule "prefer-switch" with
    | Some rule ->
      {
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
        rewrite_if =
          Lint_support.Json.bool_with_default
            ~default:default_config.rewrite.prefer_switch.rewrite_if rule "if";
        rewrite_ternary =
          Lint_support.Json.bool_with_default
            ~default:default_config.rewrite.prefer_switch.rewrite_ternary rule
            "ternary";
      }
    | None -> default_config.rewrite.prefer_switch
  in
  let no_optional_some =
    match get_rewrite_rule "no-optional-some" with
    | Some rule ->
      {
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
      }
    | None -> default_config.rewrite.no_optional_some
  in
  {
    forbidden_reference;
    single_use_function;
    rewrite = {prefer_switch; no_optional_some};
  }

let discover_config_path start_path =
  let rec loop path =
    let hidden = Filename.concat path ".rescript-lint.json" in
    let visible = Filename.concat path "rescript-lint.json" in
    if Files.exists hidden then Some hidden
    else if Files.exists visible then Some visible
    else
      let parent = Filename.dirname path in
      if parent = path then None else loop parent
  in
  loop start_path

let load ?config_path target_path =
  let config_path =
    match config_path with
    | Some path ->
      let path =
        if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
        else path
      in
      Some path
    | None ->
      let start_path =
        if Lint_support.Path.is_directory target_path then target_path
        else Filename.dirname target_path
      in
      discover_config_path start_path
  in
  match config_path with
  | None -> Ok default_config
  | Some path ->
    Lint_support.Json.read_file path |> Result.map parse_config_json
