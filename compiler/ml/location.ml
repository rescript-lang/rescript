(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lexing

type t = Warnings.loc = {
  loc_start: position;
  loc_end: position;
  loc_ghost: bool;
}

let in_file name =
  let loc = {pos_fname = name; pos_lnum = 1; pos_bol = 0; pos_cnum = -1} in
  {loc_start = loc; loc_end = loc; loc_ghost = true}

let none = in_file "_none_"

let input_name = ref "_none_"
let set_input_name name = if name <> "" then input_name := name
(* Terminal info *)

(* Print the location in some way or another *)

open Format

let show_filename file = if file = "_none_" then !input_name else file

let print_filename ppf file = Format.fprintf ppf "%s" (show_filename file)

(* return file, line, char from the given position *)
let get_pos_info pos = (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let setup_colors () =
  Misc.Color.setup !Clflags.color;
  Code_frame.setup !Clflags.color

(* ocaml's reported line/col numbering is horrible and super error-prone
   when being handled programmatically (or humanly for that matter. If you're
   an ocaml contributor reading this: who the heck reads the character count
   starting from the first erroring character?) *)
let normalize_range loc =
  (* TODO: lots of the handlings here aren't needed anymore because the new
     rescript syntax has much stronger invariants regarding positions, e.g.
     no -1 *)
  let _, start_line, start_char = get_pos_info loc.loc_start in
  let _, end_line, end_char = get_pos_info loc.loc_end in
  (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
  (* start_char is inclusive, end_char is exclusive *)
  if start_char == -1 || end_char == -1 then
    (* happens sometimes. Syntax error for example *)
    None
  else if start_line = end_line && start_char >= end_char then
    (* in some errors, starting char and ending char can be the same. But
       since ending char was supposed to be exclusive, here it might end up
       smaller than the starting char if we naively did start_char + 1 to
       just the starting char and forget ending char *)
    let same_char = start_char + 1 in
    Some ((start_line, same_char), (end_line, same_char))
  else
    (* again: end_char is exclusive, so +1-1=0 *)
    Some ((start_line, start_char + 1), (end_line, end_char))

let print_loc ppf (loc : t) =
  setup_colors ();
  let normalized_range = normalize_range loc in
  let dim_loc ppf = function
    | None -> ()
    | Some ((start_line, start_line_start_char), (end_line, end_line_end_char))
      ->
      if start_line = end_line then
        if start_line_start_char = end_line_end_char then
          fprintf ppf ":@{<dim>%i:%i@}" start_line start_line_start_char
        else
          fprintf ppf ":@{<dim>%i:%i-%i@}" start_line start_line_start_char
            end_line_end_char
      else
        fprintf ppf ":@{<dim>%i:%i-%i:%i@}" start_line start_line_start_char
          end_line end_line_end_char
  in
  fprintf ppf "@{<filename>%a@}%a" print_filename loc.loc_start.pos_fname
    dim_loc normalized_range

let print ?(src = None) ~message_kind intro ppf (loc : t) =
  (match message_kind with
  | `warning -> fprintf ppf "@[@{<info>%s@}@]@," intro
  | `warning_as_error ->
    fprintf ppf "@[@{<error>%s@} (configured as error) @]@," intro
  | `error -> fprintf ppf "@[@{<error>%s@}@]@," intro);
  (* ocaml's reported line/col numbering is horrible and super error-prone
     when being handled programmatically (or humanly for that matter. If you're
     an ocaml contributor reading this: who the heck reads the character count
     starting from the first erroring character?) *)
  let file, start_line, start_char = get_pos_info loc.loc_start in
  let _, end_line, end_char = get_pos_info loc.loc_end in
  (* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion *)
  (* start_char is inclusive, end_char is exclusive *)
  let normalized_range =
    (* TODO: lots of the handlings here aren't needed anymore because the new
       rescript syntax has much stronger invariants regarding positions, e.g.
       no -1 *)
    if start_char == -1 || end_char == -1 then
      (* happens sometimes. Syntax error for example *)
      None
    else if start_line = end_line && start_char >= end_char then
      (* in some errors, starting char and ending char can be the same. But
         since ending char was supposed to be exclusive, here it might end up
         smaller than the starting char if we naively did start_char + 1 to
         just the starting char and forget ending char *)
      let same_char = start_char + 1 in
      Some ((start_line, same_char), (end_line, same_char))
    else
      (* again: end_char is exclusive, so +1-1=0 *)
      Some ((start_line, start_char + 1), (end_line, end_char))
  in
  fprintf ppf "  @[%a@]@," print_loc loc;
  match normalized_range with
  | None -> ()
  | Some _ -> (
    try
      (* Print a syntax error that is a list of Res_diagnostics.t.
         Instead of reading file for every error, it uses the source that the parser already has. *)
      let src =
        match src with
        | Some src -> src
        | None -> Ext_io.load_file file
      in
      (* we're putting the line break `@,` here rather than above, because this
         branch might not be reached (aka no inline file content display) so
         we don't wanna end up with two line breaks in the the consequent *)
      fprintf ppf "@,%s"
        (Code_frame.print ~is_warning:(message_kind = `warning) ~src
           ~start_pos:loc.loc_start ~end_pos:loc.loc_end)
    with
    (* this might happen if the file is e.g. "", "_none_" or any of the fake file name placeholders.
       we've already printed the location above, so nothing more to do here. *)
    | Sys_error _ ->
      ())

let default_warning_printer loc ppf w =
  match Warnings.report w with
  | `Inactive -> ()
  | `Active {Warnings.number = _; message = _; is_error; sub_locs = _} ->
    setup_colors ();
    let message_kind = if is_error then `warning_as_error else `warning in
    Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]@."
      (print ~message_kind
         ("Warning number " ^ (Warnings.number w |> string_of_int)))
      loc (Warnings.message w)
(* at this point, you can display sub_locs too, from e.g. https://github.com/ocaml/ocaml/commit/f6d53cc38f87c67fbf49109f5fb79a0334bab17a
   but we won't bother for now *)

let warning_printer = ref default_warning_printer

let print_warning loc ppf w = !warning_printer loc ppf w

let formatter_for_warnings = ref err_formatter
let prerr_warning loc w = print_warning loc !formatter_for_warnings w

type 'a loc = {txt: 'a; loc: t}

let mkloc txt loc = {txt; loc}
let mknoloc txt = mkloc txt none

type error = {
  loc: t;
  msg: string;
  sub: error list;
  if_highlight: string; (* alternative message if locations are highlighted *)
}

let pp_ksprintf ?before k fmt =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  (match before with
  | None -> ()
  | Some f -> f ppf);
  kfprintf
    (fun _ ->
      pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

(* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L354 *)
(* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted *)
let print_phanton_error_prefix ppf =
  (* modified from the original. We use only 2 indentations for error report
     (see super_error_reporter above) *)
  Format.pp_print_as ppf 2 ""

let errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") fmt =
  pp_ksprintf ~before:print_phanton_error_prefix
    (fun msg -> {loc; msg; sub; if_highlight})
    fmt

let error ?(loc = none) ?(sub = []) ?(if_highlight = "") msg =
  {loc; msg; sub; if_highlight}

let error_of_exn : (exn -> error option) list ref = ref []

let register_error_of_exn f = error_of_exn := f :: !error_of_exn

exception Already_displayed_error = Warnings.Errors

let error_of_exn exn =
  match exn with
  | Already_displayed_error -> Some `Already_displayed
  | _ ->
    let rec loop = function
      | [] -> None
      | f :: rest -> (
        match f exn with
        | Some error -> Some (`Ok error)
        | None -> loop rest)
    in
    loop !error_of_exn

(* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 *)
(* This is the error report entry point. We'll replace the default reporter with this one. *)
let rec default_error_reporter ?(custom_intro = None) ?(src = None) ppf
    {loc; msg; sub} =
  setup_colors ();
  (* open a vertical box. Everything in our message is indented 2 spaces *)
  (* If src is given, it will display a syntax error after parsing. *)
  let intro =
    match (custom_intro, src) with
    | Some intro, _ -> intro
    | None, Some _ -> "Syntax error!"
    | None, None -> "We've found a bug for you!"
  in
  Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]"
    (print ~src ~message_kind:`error intro)
    loc msg;
  List.iter
    (Format.fprintf ppf "@,@[%a@]" (default_error_reporter ~custom_intro ~src))
    sub
(* no need to flush here; location's report_exception (which uses this ultimately) flushes *)

let error_reporter = ref default_error_reporter

let report_error ?(custom_intro = None) ?(src = None) ppf err =
  !error_reporter ~custom_intro ~src ppf err

let error_of_printer loc print x = errorf ~loc "%a@?" print x

let error_of_printer_file print x =
  error_of_printer (in_file !input_name) print x

let () =
  register_error_of_exn (function
    | Sys_error msg ->
      Some (errorf ~loc:(in_file !input_name) "I/O error: %s" msg)
    | Misc.HookExnWrapper {error = e; hook_name; hook_info = {Misc.sourcefile}}
      ->
      let sub =
        match error_of_exn e with
        | None | Some `Already_displayed -> error (Printexc.to_string e)
        | Some (`Ok err) -> err
      in
      Some (errorf ~loc:(in_file sourcefile) "In hook %S:" hook_name ~sub:[sub])
    | _ -> None)

external reraise : exn -> 'a = "%reraise"

let rec report_exception_rec n ppf exn =
  try
    match error_of_exn exn with
    | None -> reraise exn
    | Some `Already_displayed -> ()
    | Some (`Ok err) ->
      fprintf ppf "@[%a@]@." (report_error ~custom_intro:None ~src:None) err
  with exn when n > 0 -> report_exception_rec (n - 1) ppf exn

let report_exception ppf exn = report_exception_rec 5 ppf exn

exception Error of error

let () =
  register_error_of_exn (function
    | Error e -> Some e
    | _ -> None)

let raise_errorf ?(loc = none) ?(sub = []) ?(if_highlight = "") =
  pp_ksprintf ~before:print_phanton_error_prefix (fun msg ->
      raise (Error {loc; msg; sub; if_highlight}))

let deprecated ?(def = none) ?(use = none) loc msg =
  prerr_warning loc (Warnings.Deprecated (msg, def, use))

let map_loc f {txt; loc} = {txt = f txt; loc}
