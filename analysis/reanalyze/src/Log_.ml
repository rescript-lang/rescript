module Color = struct
  let color_enabled = lazy (Unix.isatty Unix.stdout)
  let force_color = ref false
  let get_color_enabled () = !force_color || Lazy.force color_enabled

  type color = Red | Yellow | Magenta | Cyan
  type style = FG of color | Bold | Dim

  let code_of_style = function
    | FG Red -> "31"
    | FG Yellow -> "33"
    | FG Magenta -> "35"
    | FG Cyan -> "36"
    | Bold -> "1"
    | Dim -> "2"

  let get_string_tag s =
    match s with
    | Format.String_tag s -> s
    | _ -> ""

  let style_of_tag s =
    match s |> get_string_tag with
    | "error" -> [Bold; FG Red]
    | "warning" -> [Bold; FG Magenta]
    | "info" -> [Bold; FG Yellow]
    | "dim" -> [Dim]
    | "filename" -> [FG Cyan]
    | _ -> []

  let ansi_of_tag s =
    let l = style_of_tag s in
    let s = String.concat ";" (List.map code_of_style l) in
    "\027[" ^ s ^ "m"

  let reset_lit = "\027[0m"

  let set_open_close_tag open_tag close_tag =
    {
      Format.mark_open_stag = open_tag;
      mark_close_stag = close_tag;
      print_open_stag = (fun _ -> ());
      print_close_stag = (fun _ -> ());
    }

  let color_functions =
    set_open_close_tag
      (fun s -> if get_color_enabled () then ansi_of_tag s else "")
      (fun _ -> if get_color_enabled () then reset_lit else "")

  let setup () =
    Format.pp_set_mark_tags Format.std_formatter true;
    Format.pp_set_formatter_stag_functions Format.std_formatter color_functions;
    if not (get_color_enabled ()) then Misc.Color.setup (Some Never);
    (* Print a dummy thing once in the beginning, as otherwise flushing does not work. *)
    Location.print_loc Format.str_formatter Location.none

  let error ppf s = Format.fprintf ppf "@{<error>%s@}" s
  let info ppf s = Format.fprintf ppf "@{<info>%s@}" s
end

module Loc = struct
  let print_loc ppf (loc : Location.t) =
    (* Change the range so it's on a single line.
       In this way, the line number is clickable in vscode. *)
    let start_char = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let end_char = start_char + loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
    let line = loc.loc_start.pos_lnum in
    let process_pos char (pos : Lexing.position) : Lexing.position =
      {
        pos_lnum = line;
        pos_bol = 0;
        pos_cnum = char;
        pos_fname =
          (let open Filename in
           match is_implicit pos.pos_fname with
           | _ when !Cli.ci -> basename pos.pos_fname
           | true -> concat (Sys.getcwd ()) pos.pos_fname
           | false -> pos.pos_fname);
      }
    in
    Location.print_loc ppf
      {
        loc with
        loc_start = loc.loc_start |> process_pos start_char;
        loc_end = loc.loc_end |> process_pos end_char;
      }

  let print ppf (loc : Location.t) = Format.fprintf ppf "@[%a@]" print_loc loc
end

let log x = Format.fprintf Format.std_formatter x

let item x =
  Format.fprintf Format.std_formatter "  ";
  Format.fprintf Format.std_formatter x

let missing_raise_info_to_text {Issue.missing_annotations; loc_full} =
  let missing_txt =
    Format.asprintf "%a" (Exceptions.pp ~exn_table:None) missing_annotations
  in
  if !Cli.json then
    EmitJson.emit_annotate ~action:"Add @throws annotation"
      ~pos:(EmitJson.loc_to_pos loc_full)
      ~text:(Format.asprintf "@throws(%s)\\n" missing_txt)
  else ""

let log_additional_info ~(description : Issue.description) =
  match description with
  | ExceptionAnalysisMissing missing_raise_info ->
    missing_raise_info_to_text missing_raise_info
  | _ -> ""

let missing_throw_info_to_message
    {Issue.exn_table; exn_name; missing_annotations; throw_set} =
  let throws_txt =
    Format.asprintf "%a" (Exceptions.pp ~exn_table:(Some exn_table)) throw_set
  in
  let missing_txt =
    Format.asprintf "%a" (Exceptions.pp ~exn_table:None) missing_annotations
  in
  Format.asprintf
    "@{<info>%s@} might throw %s and is not annotated with @throws(%s)" exn_name
    throws_txt missing_txt

let description_to_message (description : Issue.description) =
  match description with
  | Circular {message} -> message
  | DeadModule {message} -> message
  | DeadOptional {message} -> message
  | DeadWarning {path; message} ->
    Format.asprintf "@{<info>%s@} %s" path message
  | ExceptionAnalysis {message} -> message
  | ExceptionAnalysisMissing missing_raise_info ->
    missing_throw_info_to_message missing_raise_info
  | Termination {message} -> message

let description_to_name (description : Issue.description) =
  match description with
  | Circular _ -> Issues.warning_dead_analysis_cycle
  | DeadModule _ -> Issues.warning_dead_module
  | DeadOptional {dead_optional = WarningUnusedArgument} ->
    Issues.warning_unused_argument
  | DeadOptional {dead_optional = WarningRedundantOptionalArgument} ->
    Issues.warning_redundant_optional_argument
  | DeadWarning {dead_warning = WarningDeadException} ->
    Issues.warning_dead_exception
  | DeadWarning {dead_warning = WarningDeadType} -> Issues.warning_dead_type
  | DeadWarning {dead_warning = WarningDeadValue} -> Issues.warning_dead_value
  | DeadWarning {dead_warning = WarningDeadValueWithSideEffects} ->
    Issues.warning_dead_value_with_side_effects
  | DeadWarning {dead_warning = IncorrectDeadAnnotation} ->
    Issues.incorrect_dead_annotation
  | ExceptionAnalysis _ -> Issues.exception_analysis
  | ExceptionAnalysisMissing _ -> Issues.exception_analysis
  | Termination {termination = ErrorHygiene} -> Issues.error_hygiene
  | Termination {termination = ErrorNotImplemented} ->
    Issues.error_not_implemented
  | Termination {termination = ErrorTermination} -> Issues.error_termination
  | Termination {termination = TerminationAnalysisInternal} ->
    Issues.termination_analysis_internal

let log_issue ~config ~(issue : Issue.t) =
  let open Format in
  let loc = issue.loc in
  if config.DceConfig.cli.json then
    let file = loc.loc_start.pos_fname in
    let start_line = loc.loc_start.pos_lnum - 1 in
    let start_character = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
    let end_line = loc.loc_end.pos_lnum - 1 in
    let end_character = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
    let message = description_to_message issue.description in
    Format.asprintf "%a%s%s"
      (fun ppf () ->
        EmitJson.emit_item ~ppf ~name:issue.name
          ~kind:
            (match issue.severity with
            | Warning -> "warning"
            | Error -> "error")
          ~file
          ~range:(start_line, start_character, end_line, end_character)
          ~message)
      ()
      (log_additional_info ~description:issue.description)
      (if config.DceConfig.cli.json then EmitJson.emit_close () else "")
  else
    let color =
      match issue.severity with
      | Warning -> Color.info
      | Error -> Color.error
    in
    asprintf "@.  %a@.  %a@.  %s%s@." color issue.name Loc.print issue.loc
      (description_to_message issue.description)
      (log_additional_info ~description:issue.description)

module Stats = struct
  let issues = ref []
  let add_issue (issue : Issue.t) = issues := issue :: !issues
  let clear () = issues := []
  let get_issue_count () = List.length !issues

  let get_sorted_issues () =
    let counters2 = Hashtbl.create 1 in
    !issues
    |> List.iter (fun (issue : Issue.t) ->
           let counter =
             match Hashtbl.find_opt counters2 issue.name with
             | Some counter -> counter
             | None ->
               let counter = ref 0 in
               Hashtbl.add counters2 issue.name counter;
               counter
           in
           incr counter);
    let issues, n_issues =
      Hashtbl.fold
        (fun name cnt (issues, n_issues) ->
          ((name, cnt) :: issues, n_issues + !cnt))
        counters2 ([], 0)
    in
    (issues |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2), n_issues)

  let report ~config =
    !issues |> List.rev
    |> List.iter (fun issue -> log_issue ~config ~issue |> print_string);
    let sorted_issues, n_issues = get_sorted_issues () in
    if not config.DceConfig.cli.json then (
      if sorted_issues <> [] then item "@.";
      item "Analysis reported %d issues%s@." n_issues
        (match sorted_issues with
        | [] -> ""
        | _ :: _ ->
          " ("
          ^ (sorted_issues
            |> List.map (fun (name, cnt) -> name ^ ":" ^ string_of_int !cnt)
            |> String.concat ", ")
          ^ ")"))
end

let log_issue ~for_stats ~severity ~(loc : Location.t) description =
  let name = description_to_name description in
  if Suppress.filter loc.loc_start then
    if for_stats then Stats.add_issue {name; severity; loc; description}

let warning ?(for_stats = true) ~loc description =
  description |> log_issue ~severity:Warning ~for_stats ~loc

let error ~loc description =
  description |> log_issue ~severity:Error ~for_stats:true ~loc
