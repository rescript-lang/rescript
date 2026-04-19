open Analysis
open Lint_shared

let default_forbidden_reference_rule : forbidden_reference_rule =
  {enabled = true; severity = SeverityError; message = None; items = []}

let default_single_use_function_rule : single_use_function_rule =
  {enabled = true; severity = SeverityWarning; message = None}

let default_alias_avoidance_rule : alias_avoidance_rule =
  {enabled = true; severity = SeverityWarning; message = None}

let default_config =
  {
    forbidden_reference = [default_forbidden_reference_rule];
    single_use_function = default_single_use_function_rule;
    alias_avoidance = default_alias_avoidance_rule;
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

let parse_rule_message json =
  Option.bind (json |> Json.get "message") Json.string

let parse_forbidden_reference_kind = function
  | "module" -> Some ForbiddenReferenceModule
  | "value" -> Some ForbiddenReferenceValue
  | "type" -> Some ForbiddenReferenceType
  | _ -> None

let result_all results =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok value :: rest -> loop (value :: acc) rest
    | Error _ as error :: _ -> error
  in
  loop [] results

let parse_rule_objects ~rule = function
  | None -> Ok []
  | Some (Json.Object _ as rule_json) -> Ok [rule_json]
  | Some (Json.Array items) ->
    items
    |> List.mapi (fun index item ->
           match item with
           | Json.Object _ as rule_json -> Ok rule_json
           | _ ->
             Error
               (Printf.sprintf
                  "error: lint rule `%s` instance %d must be an object" rule
                  (index + 1)))
    |> result_all
  | Some _ ->
    Error
      (Printf.sprintf
         "error: lint rule `%s` must be an object or array of objects" rule)

let parse_singleton_rule ~rule ~default parse json =
  Result.bind
    (parse_rule_objects ~rule json)
    (function
      | [] -> Ok default
      | [rule_json] -> parse rule_json
      | _ ->
        Error
          (Printf.sprintf
             "error: lint rule `%s` does not support multiple instances" rule))

let parse_rule_instances ~rule ~default parse json =
  Result.bind
    (parse_rule_objects ~rule json)
    (function
      | [] -> Ok [default]
      | rule_jsons -> rule_jsons |> List.map parse |> result_all)

let parse_item_objects ~rule = function
  | None -> Ok []
  | Some (Json.Array items) ->
    items
    |> List.mapi (fun index item ->
           match item with
           | Json.Object _ as item_json -> Ok item_json
           | _ ->
             Error
               (Printf.sprintf
                  "error: lint rule `%s` item %d must be an object" rule
                  (index + 1)))
    |> result_all
  | Some _ ->
    Error
      (Printf.sprintf "error: lint rule `%s` field `items` must be an array of objects" rule)

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
  let parse_forbidden_reference_rule rule :
      (forbidden_reference_rule, string) result =
    let parse_item item_json : (forbidden_reference_item, string) result =
      let kind =
        match Option.bind (item_json |> Json.get "kind") Json.string with
        | None ->
          Error
            "error: lint rule `forbidden-reference` items must include `kind`"
        | Some kind -> (
          match parse_forbidden_reference_kind kind with
          | Some kind -> Ok kind
          | None ->
            Error
              (Printf.sprintf
                 "error: lint rule `forbidden-reference` item kind `%s` must be one of `module`, `value`, or `type`"
                 kind))
      in
      let path =
        match Option.bind (item_json |> Json.get "path") Json.string with
        | None ->
          Error
            "error: lint rule `forbidden-reference` items must include `path`"
        | Some path ->
          let path =
            path |> String.split_on_char '.'
            |> List.filter (fun segment -> segment <> "")
          in
          if path = [] then
            Error
              "error: lint rule `forbidden-reference` item path must not be empty"
          else Ok path
      in
      Result.bind kind (fun kind ->
          Result.map
            (fun path ->
              {
                kind;
                path;
                message = parse_rule_message item_json;
              })
            path)
    in
    Result.bind
      (parse_item_objects ~rule:"forbidden-reference" (rule |> Json.get "items"))
      (fun items_json ->
        Result.map
          (fun items ->
            ({
               enabled =
                 Lint_support.Json.bool_with_default ~default:true rule
                   "enabled";
               severity =
                 parse_rule_severity
                   ~default:default_forbidden_reference_rule.severity rule;
               message = parse_rule_message rule;
               items;
             } : forbidden_reference_rule))
          (items_json |> List.map parse_item |> result_all))
  in
  let parse_single_use_function_rule rule :
      (single_use_function_rule, string) result =
    Ok
      ({
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
        severity =
          parse_rule_severity ~default:default_single_use_function_rule.severity
            rule;
        message = parse_rule_message rule;
      } : single_use_function_rule)
  in
  let parse_alias_avoidance_rule rule : (alias_avoidance_rule, string) result =
    Ok
      ({
        enabled =
          Lint_support.Json.bool_with_default ~default:true rule "enabled";
        severity =
          parse_rule_severity ~default:default_alias_avoidance_rule.severity
            rule;
        message = parse_rule_message rule;
      } : alias_avoidance_rule)
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
  Result.bind
    (parse_rule_instances ~rule:"forbidden-reference"
       ~default:default_forbidden_reference_rule
       parse_forbidden_reference_rule (get_rule "forbidden-reference"))
    (fun forbidden_reference ->
      Result.bind
        (parse_singleton_rule ~rule:"single-use-function"
           ~default:default_single_use_function_rule
           parse_single_use_function_rule (get_rule "single-use-function"))
        (fun single_use_function ->
          Result.map
            (fun alias_avoidance ->
              {
                forbidden_reference;
                single_use_function;
                alias_avoidance;
                rewrite = {prefer_switch; no_optional_some};
              })
            (parse_singleton_rule ~rule:"alias-avoidance"
               ~default:default_alias_avoidance_rule
               parse_alias_avoidance_rule (get_rule "alias-avoidance"))))

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
    Result.bind (Lint_support.Json.read_file path) parse_config_json
