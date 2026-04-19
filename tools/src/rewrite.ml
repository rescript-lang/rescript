open Analysis
open SharedTypes
open Lint_shared

type mode = Write | Diff

type applied_rule = {rule: string; count: int; note: string}

type file_result = {
  abs_path: string;
  changed: bool;
  applied_rules: applied_rule list;
  diff: string option;
  rewritten_contents: string option;
}

type rewritten_file = {
  changed: bool;
  source_contents: string;
  contents: string;
  applied_rules: applied_rule list;
}

type run_result = {output: string; changed_files: int}

type rule_counts = {prefer_switch: int; no_optional_some: int}

let package_for_path path =
  let uri = Uri.fromPath path in
  Packages.getPackage ~uri

let is_blank_line line = String.trim line = ""

let source_files_in_package package =
  package.projectFiles |> FileSet.elements
  |> List.filter_map (fun module_name ->
         Hashtbl.find_opt package.pathsForModule module_name
         |> Option.map getSrc)
  |> List.concat
  |> List.filter FindFiles.isSourceFile

let collect_files target_path =
  if Lint_support.Path.is_directory target_path then
    match package_for_path target_path with
    | Some package ->
      let package_files =
        source_files_in_package package
        |> List.filter (fun file -> Files.pathStartsWith file target_path)
        |> List.sort_uniq String.compare
      in
      if package_files = [] then
        Files.collect target_path FindFiles.isSourceFile
        |> List.sort_uniq String.compare
      else package_files
    | None ->
      Files.collect target_path FindFiles.isSourceFile
      |> List.sort_uniq String.compare
  else if FindFiles.isSourceFile target_path then [target_path]
  else []

let display_base target_path files =
  match files with
  | file :: _ -> (
    match package_for_path file with
    | Some package -> package.rootPath
    | None ->
      if Lint_support.Path.is_directory target_path then target_path
      else Filename.dirname target_path)
  | [] ->
    if Lint_support.Path.is_directory target_path then target_path
    else Filename.dirname target_path

let is_ternary_attribute ((name, _payload) : Parsetree.attribute) =
  match name.Location.txt with
  | "res.ternary" | "ns.ternary" -> true
  | _ -> false

let filter_ternary_attributes attrs =
  attrs |> List.filter (fun attr -> not (is_ternary_attribute attr))

let has_non_ternary_attributes attrs =
  attrs |> List.exists (fun attr -> not (is_ternary_attribute attr))

let is_ternary_expr (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_ifthenelse _ ->
    expr.pexp_attributes |> List.exists is_ternary_attribute
  | _ -> false

let should_rewrite_if ~(config : config) expr =
  if not config.rewrite.prefer_switch.enabled then false
  else if is_ternary_expr expr then config.rewrite.prefer_switch.rewrite_ternary
  else config.rewrite.prefer_switch.rewrite_if

let mklident ~loc txt = Location.mkloc (Longident.Lident txt) loc

let unit_expression ~loc =
  Ast_helper.Exp.construct ~loc (mklident ~loc "()") None

let bool_pattern ~loc value =
  Ast_helper.Pat.construct ~loc
    (mklident ~loc (if value then "true" else "false"))
    None

let make_boolean_switch ~loc ~attrs condition then_expr else_expr =
  Ast_helper.Exp.match_ ~loc ~attrs condition
    [
      Ast_helper.Exp.case (bool_pattern ~loc true) then_expr;
      Ast_helper.Exp.case (bool_pattern ~loc false) else_expr;
    ]

let make_guard_switch ~loc ~attrs branches fallback =
  let wildcard = Ast_helper.Pat.any ~loc () in
  let cases =
    branches
    |> List.map (fun (condition, expr) ->
           Ast_helper.Exp.case wildcard ~guard:condition expr)
  in
  Ast_helper.Exp.match_ ~loc ~attrs (unit_expression ~loc)
    (cases @ [Ast_helper.Exp.case wildcard fallback])

let verify_rewritten_source ~path ~contents =
  if Filename.check_suffix path ".resi" then
    let {Res_driver.invalid; diagnostics; _} =
      Res_driver.parse_interface_from_source ~for_printer:true
        ~display_filename:path ~source:contents
    in
    if invalid then
      let buf = Buffer.create 256 in
      let formatter = Format.formatter_of_buffer buf in
      Res_diagnostics.print_report ~formatter diagnostics contents;
      Format.pp_print_flush formatter ();
      Error
        (Printf.sprintf
           "error: rewrite produced invalid syntax for %s\n%s" path
           (Buffer.contents buf))
    else Ok ()
  else
    let {Res_driver.invalid; diagnostics; _} =
      Res_driver.parse_implementation_from_source ~for_printer:true
        ~display_filename:path ~source:contents
    in
    if invalid then
      let buf = Buffer.create 256 in
      let formatter = Format.formatter_of_buffer buf in
      Res_diagnostics.print_report ~formatter diagnostics contents;
      Format.pp_print_flush formatter ();
      Error
        (Printf.sprintf
           "error: rewrite produced invalid syntax for %s\n%s" path
           (Buffer.contents buf))
    else Ok ()

let applied_rules_of_counts (rule_counts : rule_counts) =
  [
    ("prefer-switch", rule_counts.prefer_switch);
    ("no-optional-some", rule_counts.no_optional_some);
  ]
  |> List.filter_map (fun (rule, count) ->
         if count <= 0 then None
         else Some {rule; count; note = rewrite_note_for_rule rule})

module Diff = struct
  type op = Equal of string | Delete of string | Insert of string

  type entry = {
    op: op;
    old_no: int option;
    new_no: int option;
  }

  let strip_trailing_cr line =
    let len = String.length line in
    if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1)
    else line

  let split_lines text =
    let lines = String.split_on_char '\n' text |> List.map strip_trailing_cr in
    match List.rev lines with
    | "" :: rest -> List.rev rest
    | _ -> lines

  let build_ops ~before ~after =
    let before = Array.of_list (split_lines before) in
    let after = Array.of_list (split_lines after) in
    let before_len = Array.length before in
    let after_len = Array.length after in
    let table = Array.make_matrix (before_len + 1) (after_len + 1) 0 in
    for before_index = before_len - 1 downto 0 do
      for after_index = after_len - 1 downto 0 do
        table.(before_index).(after_index) <-
          if before.(before_index) = after.(after_index) then
            table.(before_index + 1).(after_index + 1) + 1
          else
            max table.(before_index + 1).(after_index)
              table.(before_index).(after_index + 1)
      done
    done;
    let rec loop before_index after_index acc =
      if before_index < before_len && after_index < after_len
         && before.(before_index) = after.(after_index)
      then
        loop (before_index + 1) (after_index + 1)
          (Equal before.(before_index) :: acc)
      else if before_index < before_len
              && (after_index = after_len
                 || table.(before_index + 1).(after_index)
                    >= table.(before_index).(after_index + 1))
      then
        loop (before_index + 1) after_index
          (Delete before.(before_index) :: acc)
      else if after_index < after_len then
        loop before_index (after_index + 1)
          (Insert after.(after_index) :: acc)
      else List.rev acc
    in
    loop 0 0 []

  let annotate ops =
    let old_line = ref 1 in
    let new_line = ref 1 in
    ops
    |> List.map (fun op ->
           match op with
           | Equal _ ->
             let current_old = !old_line in
             let current_new = !new_line in
             incr old_line;
             incr new_line;
             {op; old_no = Some current_old; new_no = Some current_new}
           | Delete _ ->
             let current_old = !old_line in
             incr old_line;
             {op; old_no = Some current_old; new_no = None}
           | Insert _ ->
             let current_new = !new_line in
             incr new_line;
             {op; old_no = None; new_no = Some current_new})
    |> Array.of_list

  let is_change (entry : entry) =
    match entry.op with
    | Equal _ -> false
    | Delete _ | Insert _ -> true

  let count_lines entries start finish get_line =
    let count = ref 0 in
    for index = start to finish do
      if Option.is_some (get_line entries.(index)) then incr count
    done;
    !count

  let find_forward entries start finish get_line =
    let rec loop index =
      if index > finish then None
      else
        match get_line entries.(index) with
        | Some _ as line -> line
        | None -> loop (index + 1)
    in
    loop start

  let find_backward entries start get_line =
    let rec loop index =
      if index < 0 then None
      else
        match get_line entries.(index) with
        | Some _ as line -> line
        | None -> loop (index - 1)
    in
    loop start

  let line_start entries start finish get_line =
    match find_forward entries start finish get_line with
    | Some line -> line
    | None -> (
      match find_backward entries (start - 1) get_line with
      | Some line -> line + 1
      | None -> 1)

  let render_hunk entries start finish =
    let old_start = line_start entries start finish (fun entry -> entry.old_no) in
    let new_start = line_start entries start finish (fun entry -> entry.new_no) in
    let old_count =
      count_lines entries start finish (fun entry -> entry.old_no)
    in
    let new_count =
      count_lines entries start finish (fun entry -> entry.new_no)
    in
    let header =
      Printf.sprintf "@@ -%d,%d +%d,%d @@" old_start old_count new_start
        new_count
    in
    let lines =
      entries
      |> Array.to_list |> List.mapi (fun index entry -> (index, entry))
      |> List.filter_map (fun (index, entry) ->
             if index < start || index > finish then None
             else
               Some
                 (match entry.op with
                 | Equal line when is_blank_line line -> ""
                 | Equal line -> " " ^ line
                 | Delete line when is_blank_line line -> "-"
                 | Delete line -> "-" ^ line
                 | Insert line when is_blank_line line -> "+"
                 | Insert line -> "+" ^ line))
    in
    String.concat "\n" (header :: lines)

  let collect_hunks entries context =
    let len = Array.length entries in
    let changes =
      let rec loop index acc =
        if index >= len then List.rev acc
        else if is_change entries.(index) then loop (index + 1) (index :: acc)
        else loop (index + 1) acc
      in
      loop 0 []
    in
    let rec group acc = function
      | [] -> List.rev acc
      | change :: rest ->
        let start = max 0 (change - context) in
        let finish = ref (min (len - 1) (change + context)) in
        let rec absorb = function
          | next :: tail when next <= !finish + context + 1 ->
            finish := min (len - 1) (next + context);
            absorb tail
          | remaining -> remaining
        in
        let remaining = absorb rest in
        group ((start, !finish) :: acc) remaining
    in
    group [] changes

  let render ~path ~before ~after =
    let entries = build_ops ~before ~after |> annotate in
    let has_changes =
      entries |> Array.exists is_change
    in
    if not has_changes then None
    else
      let hunks = collect_hunks entries 3 in
      let body =
        hunks |> List.map (fun (start, finish) -> render_hunk entries start finish)
      in
      Some
        (String.concat "\n"
           ([
              Printf.sprintf "--- a/%s" path;
              Printf.sprintf "+++ b/%s" path;
            ]
           @ body))
end

let rewrite_file ~(config : config) path =
  let prefer_switch_count = ref 0 in
  let no_optional_some_count = ref 0 in
  let rec expr mapper (expression : Parsetree.expression) =
    match expression.pexp_desc with
    | Pexp_apply {funct; args; partial; transformed_jsx} ->
      let funct = expr mapper funct in
      let args =
        args
        |> List.map (fun (label, arg) -> (label, expr mapper arg))
        |> List.map
             (fun ((label, arg) : Asttypes.arg_label * Parsetree.expression) ->
               match (label, arg.pexp_desc) with
               | Asttypes.Optional {txt; loc}, Pexp_construct ({txt = Lident "Some"; _}, Some inner)
                 when config.rewrite.no_optional_some.enabled ->
                 incr no_optional_some_count;
                 (Asttypes.Labelled {txt; loc}, inner)
               | _ -> (label, arg))
      in
      {
        expression with
        pexp_desc = Pexp_apply {funct; args; partial; transformed_jsx};
      }
    | Pexp_ifthenelse _ when should_rewrite_if ~config expression ->
      let rec collect branches (current_expr : Parsetree.expression) =
        match current_expr.pexp_desc with
        | Pexp_ifthenelse (condition, then_expr, Some else_expr)
          when should_rewrite_if ~config current_expr
               && not
                    (branches <> []
                    && has_non_ternary_attributes current_expr.pexp_attributes)
          ->
          collect ((condition, then_expr) :: branches) else_expr
        | Pexp_ifthenelse (condition, then_expr, None)
          when should_rewrite_if ~config current_expr
               && not
                    (branches <> []
                    && has_non_ternary_attributes current_expr.pexp_attributes)
          ->
          (List.rev ((condition, then_expr) :: branches), None)
        | _ -> (List.rev branches, Some current_expr)
      in
      let branches, fallback = collect [] expression in
      let branches =
        branches
        |> List.map (fun (condition, then_expr) ->
               (expr mapper condition, expr mapper then_expr))
      in
      let fallback =
        match fallback with
        | Some fallback -> expr mapper fallback
        | None -> unit_expression ~loc:expression.pexp_loc
      in
      let attrs = filter_ternary_attributes expression.pexp_attributes in
      incr prefer_switch_count;
      (match branches with
      | [condition, then_expr] ->
        make_boolean_switch ~loc:expression.pexp_loc ~attrs condition then_expr
          fallback
      | _ ->
        make_guard_switch ~loc:expression.pexp_loc ~attrs branches fallback)
    | _ -> Ast_mapper.default_mapper.expr mapper expression
  in
  let mapper = {Ast_mapper.default_mapper with expr} in
  let finalize ~source ~contents =
    let applied_rules =
      applied_rules_of_counts
        {
          prefer_switch = !prefer_switch_count;
          no_optional_some = !no_optional_some_count;
        }
    in
    if applied_rules = [] then
      Ok {changed = false; source_contents = source; contents = source; applied_rules}
    else
      match verify_rewritten_source ~path ~contents with
      | Error _ as error -> error
      | Ok () ->
        Ok
          {
            changed = contents <> source;
            source_contents = source;
            contents;
            applied_rules;
          }
  in
  if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree; comments; source; _} =
      Res_driver.parsing_engine.parse_interface ~for_printer:true ~filename:path
    in
    let signature = mapper.signature mapper parsetree in
    let contents = Res_printer.print_interface signature ~comments in
    finalize ~source ~contents
  else if Filename.check_suffix path ".res" then
    let {Res_driver.parsetree; comments; source; _} =
      Res_driver.parsing_engine.parse_implementation ~for_printer:true
        ~filename:path
    in
    let structure = mapper.structure mapper parsetree in
    let contents =
      Res_printer.print_implementation ~width:Res_printer.default_print_width
        structure ~comments
    in
    finalize ~source ~contents
  else
    Error
      (Printf.sprintf
         "File extension not supported. This command accepts .res and .resi \
          files")

let mode_name = function
  | Write -> "write"
  | Diff -> "diff"

let status_for_mode mode changed =
  match (mode, changed) with
  | Write, true -> "rewritten"
  | Diff, true -> "preview"
  | _, false -> "unchanged"

let stringify_applied_rule_json (applied_rule : applied_rule) =
  Lint_support.Json.stringify_compact_object
    [
      ("rule", Some (Protocol.wrapInQuotes applied_rule.rule));
      ("count", Some (string_of_int applied_rule.count));
      ("note", Some (Protocol.wrapInQuotes applied_rule.note));
    ]

let stringify_applied_rules_json applied_rules =
  Protocol.array (applied_rules |> List.map stringify_applied_rule_json)

let stringify_applied_rules_text applied_rules =
  applied_rules
  |> List.map (fun applied_rule ->
         Printf.sprintf "- %s(%d): %s" applied_rule.rule applied_rule.count
           applied_rule.note)

let count_unchanged_files results =
  results
  |> List.fold_left
       (fun count (result : file_result) -> if result.changed then count else count + 1)
       0

let summarize_applied_rules results =
  let totals = Hashtbl.create 8 in
  results
  |> List.iter (fun (result : file_result) ->
         result.applied_rules
         |> List.iter (fun (applied_rule : applied_rule) ->
                let total =
                  Hashtbl.find_opt totals applied_rule.rule |> Option.value ~default:0
                in
                Hashtbl.replace totals applied_rule.rule (total + applied_rule.count)));
  rewrite_rule_infos
  |> List.filter_map (fun (rule_info : rule_info) ->
         match Hashtbl.find_opt totals rule_info.rule with
         | Some count when count > 0 ->
           Some
             {
               rule = rule_info.rule;
               count;
               note = rewrite_note_for_rule rule_info.rule;
             }
         | _ -> None)

let summary_label_for_mode = function
  | Write -> "rewritten"
  | Diff -> "previewed"

let stringify_summary_text ~mode results =
  let changed_files =
    results
    |> List.fold_left
         (fun count (result : file_result) -> if result.changed then count + 1 else count)
         0
  in
  let unchanged_files = count_unchanged_files results in
  let rule_totals = summarize_applied_rules results in
  let rule_summary =
    if rule_totals = [] then ""
    else
      ", rules: "
      ^ String.concat ", "
          (rule_totals
          |> List.map (fun (applied_rule : applied_rule) ->
                 Printf.sprintf "%s(%d)" applied_rule.rule applied_rule.count))
  in
  Printf.sprintf "summary: %s %d files, unchanged %d%s"
    (summary_label_for_mode mode) changed_files unchanged_files rule_summary

let stringify_text_file_result ~mode ~display_base (result : file_result) =
  let path = Lint_support.Path.display ~base:display_base result.abs_path in
  let lines =
    [
      Printf.sprintf "path: %s" path;
      Printf.sprintf "status: %s" (status_for_mode mode result.changed);
    ]
  in
  let lines =
    if result.applied_rules = [] then lines
    else lines @ ("rules:" :: stringify_applied_rules_text result.applied_rules)
  in
  match result.diff with
  | None -> String.concat "\n" lines
  | Some diff ->
    String.concat "\n" (lines @ ["diff:"; "```diff"; diff; "```"])

let stringify_json_file_result ~mode ~display_base (result : file_result) =
  let path = Lint_support.Path.display ~base:display_base result.abs_path in
  Lint_support.Json.stringify_compact_object
    [
      ("path", Some (Protocol.wrapInQuotes path));
      ("mode", Some (Protocol.wrapInQuotes (mode_name mode)));
      ("changed", Some (string_of_bool result.changed));
      ("applied", Some (stringify_applied_rules_json result.applied_rules));
      ("diff", Protocol.optWrapInQuotes result.diff);
      ("rewritten", Protocol.optWrapInQuotes result.rewritten_contents);
    ]

let render_results ~mode ~json ~display_base results =
  if json then
    "["
    ^ String.concat ","
        (results
        |> List.map (stringify_json_file_result ~mode ~display_base))
    ^ "]"
  else
    let sections =
      results |> List.map (stringify_text_file_result ~mode ~display_base)
    in
    String.concat "\n\n" (sections @ [stringify_summary_text ~mode results])

let write_contents path contents =
  let oc = open_out path in
  Printf.fprintf oc "%s" contents;
  close_out oc

let run ?config_path ?(json = false) ?(mode = Write) target =
  let target =
    if Filename.is_relative target then Filename.concat (Sys.getcwd ()) target
    else target
  in
  if not (Files.exists target) then
    Error ("error: no such file or directory: " ^ target)
  else if
    (not (Lint_support.Path.is_directory target))
    && not (FindFiles.isSourceFile target)
  then
    Error
      "File extension not supported. This command accepts .res and .resi \
       files"
  else
    let target = Unix.realpath target in
    match Lint_config.load ?config_path target with
    | Error _ as error -> error
    | Ok config ->
      let files = collect_files target in
      let display_base = display_base target files in
      let process_one path =
        match rewrite_file ~config path with
        | Error _ as error -> error
        | Ok {changed; source_contents; contents; applied_rules} ->
          (match mode with
          | Write when changed -> write_contents path contents
          | Write | Diff -> ());
          let rel_path = Lint_support.Path.display ~base:display_base path in
          let diff =
            match (mode, changed) with
            | Diff, true ->
              Diff.render ~path:rel_path ~before:source_contents ~after:contents
            | Write, _ | Diff, false -> None
          in
          let rewritten_contents =
            match (mode, changed) with
            | Diff, true -> Some contents
            | Write, _ | Diff, false -> None
          in
          Ok {abs_path = path; changed; applied_rules; diff; rewritten_contents}
      in
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | path :: rest -> (
          match process_one path with
          | Error _ as error -> error
          | Ok result -> collect (result :: acc) rest)
      in
      match collect [] files with
      | Error _ as error -> error
      | Ok results ->
        Ok
          {
            output = render_results ~mode ~json ~display_base results;
            changed_files =
              results
              |> List.fold_left
                   (fun count (result : file_result) ->
                     if result.changed then count + 1 else count)
                   0;
          }
