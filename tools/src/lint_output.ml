open Analysis
open Lint_shared

let get_source source_cache path =
  match Hashtbl.find_opt source_cache path with
  | Some source -> source
  | None ->
    let source = Files.readFile path in
    Hashtbl.add source_cache path source;
    source

let trim_trailing_newlines value =
  let rec loop last =
    if last < 0 then ""
    else if value.[last] = '\n' || value.[last] = '\r' then loop (last - 1)
    else String.sub value 0 (last + 1)
  in
  loop (String.length value - 1)

let snippet_of_raw ~source_cache (raw_finding : raw_finding) =
  match get_source source_cache raw_finding.abs_path with
  | None -> None
  | Some source ->
    Code_frame.print ~highlight_style:Underlined ~context_lines_before:2
      ~context_lines_after:1 ~skip_blank_context:true
      ~is_warning:(raw_finding.severity = SeverityWarning)
      ~src:source ~start_pos:raw_finding.loc.loc_start
      ~end_pos:raw_finding.loc.loc_end
    |> trim_trailing_newlines
    |> fun snippet -> Some ("```text\n" ^ snippet ^ "\n```")

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
  let source_cache = Hashtbl.create 16 in
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
