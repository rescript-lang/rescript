(* Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let ninja_ansi_forced =
  lazy (try Sys.getenv "NINJA_ANSI_FORCED" with Not_found -> "")

let color_enabled = lazy (Unix.isatty Unix.stdout)

(* same logic as [ninja.exe] *)
let get_color_enabled () =
  let colorful =
    match ninja_ansi_forced with
    | (lazy "1") -> true
    | (lazy ("0" | "false")) -> false
    | _ -> Lazy.force color_enabled
  in
  colorful

let color_functions : Format.formatter_stag_functions =
  {
    mark_open_stag =
      (fun s ->
        if get_color_enabled () then Ext_color.ansi_of_tag s
        else Ext_string.empty);
    mark_close_stag =
      (fun _ ->
        if get_color_enabled () then Ext_color.reset_lit else Ext_string.empty);
    print_open_stag = (fun _ -> ());
    print_close_stag = (fun _ -> ());
  }

(* let set_color ppf =
   Format.pp_set_formatter_tag_functions ppf color_functions *)

let setup () =
  Format.pp_set_mark_tags Format.std_formatter true;
  Format.pp_set_mark_tags Format.err_formatter true;
  Format.pp_set_formatter_stag_functions Format.std_formatter color_functions;
  Format.pp_set_formatter_stag_functions Format.err_formatter color_functions

type level = Debug | Info | Warn | Error

let int_of_level (x : level) =
  match x with
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3

let log_level = ref Warn

let verbose () = log_level := Debug

let dfprintf level fmt =
  if int_of_level level >= int_of_level !log_level then Format.fprintf fmt
  else Format.ifprintf fmt

type 'a fmt = Format.formatter -> ('a, Format.formatter, unit) format -> 'a

type 'a log = ('a, Format.formatter, unit) format -> 'a

let debug fmt = dfprintf Debug Format.std_formatter fmt

let info fmt = dfprintf Info Format.std_formatter fmt

let warn fmt = dfprintf Warn Format.err_formatter fmt

let error fmt = dfprintf Error Format.err_formatter fmt

let info_args (args : string array) =
  if int_of_level Info >= int_of_level !log_level then (
    for i = 0 to Array.length args - 1 do
      Format.pp_print_string Format.std_formatter (Array.unsafe_get args i);
      Format.pp_print_string Format.std_formatter Ext_string.single_space
    done;
    Format.pp_print_newline Format.std_formatter ())
  else ()
