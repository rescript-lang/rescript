open Analysis
open Lint_shared

let snippet_of_raw ~source_cache (raw_finding : raw_finding) =
  Lint_support.Snippet.of_loc ~source_cache ~path:raw_finding.abs_path
    ~loc:raw_finding.loc
    ~is_warning:(raw_finding.severity = SeverityWarning)
    ()

let compact_finding_fields (finding : finding) =
  [
    ("rule", Some (Protocol.wrapInQuotes finding.rule));
    ("path", Some (Protocol.wrapInQuotes finding.path));
    ("range", Some (Lint_support.Range.to_compact_json finding.range));
    ( "severity",
      Some (Protocol.wrapInQuotes (severity_to_string finding.severity)) );
    ("message", Some (Protocol.wrapInQuotes finding.message));
    ("symbol", Protocol.optWrapInQuotes finding.symbol);
  ]

let stringify_text_finding ~source_cache ~display_base
    (raw_finding : raw_finding) =
  let finding = finding_of_raw ~display_base raw_finding in
  let lines =
    [
      Printf.sprintf "severity: %s" (severity_to_string finding.severity);
      Printf.sprintf "rule: %s" finding.rule;
      Printf.sprintf "path: %s" finding.path;
      Printf.sprintf "range: %s" (Lint_support.Range.to_text finding.range);
      Printf.sprintf "message: %s" finding.message;
    ]
  in
  let lines =
    match finding.symbol with
    | None -> lines
    | Some symbol -> lines @ [Printf.sprintf "symbol: %s" symbol]
  in
  match snippet_of_raw ~source_cache raw_finding with
  | None -> String.concat "\n" lines
  | Some snippet -> String.concat "\n" (lines @ ["snippet:"; snippet])

let stringify_text_findings ~display_base findings =
  let source_cache = Lint_support.Snippet.create_cache () in
  findings
  |> List.map (stringify_text_finding ~source_cache ~display_base)
  |> String.concat "\n\n"

let stringify_compact_finding finding =
  compact_finding_fields finding |> Lint_support.Json.stringify_compact_object

let stringify_compact_findings ~display_base findings =
  "["
  ^ String.concat ","
      (findings
      |> List.map (fun raw_finding ->
             raw_finding
             |> finding_of_raw ~display_base
             |> stringify_compact_finding))
  ^ "]"

let render ~json ~display_base findings =
  if json then stringify_compact_findings ~display_base findings
  else stringify_text_findings ~display_base findings
