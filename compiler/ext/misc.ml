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

(* Errors *)

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: ";
  prerr_endline msg;
  raise Fatal_error

let try_finally work cleanup =
  let result =
    try work ()
    with e ->
      cleanup ();
      raise e
  in
  cleanup ();
  result

type ref_and_value = R : 'a ref * 'a -> ref_and_value

let protect_refs =
  let set_refs l = List.iter (fun (R (r, v)) -> r := v) l in
  fun refs f ->
    let backup = List.map (fun (R (r, _)) -> R (r, !r)) refs in
    set_refs refs;
    match f () with
    | x ->
      set_refs backup;
      x
    | exception e ->
      set_refs backup;
      raise e

(* List functions *)

let rec map_end f l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> f hd :: map_end f tl l2

let rec map_left_right f = function
  | [] -> []
  | hd :: tl ->
    let res = f hd in
    res :: map_left_right f tl

let rec for_all2 pred l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | hd1 :: tl1, hd2 :: tl2 -> pred hd1 hd2 && for_all2 pred tl1 tl2
  | _, _ -> false

let rec replicate_list elem n =
  if n <= 0 then [] else elem :: replicate_list elem (n - 1)

let rec list_remove x = function
  | [] -> []
  | hd :: tl -> if hd = x then tl else hd :: list_remove x tl

let rec split_last = function
  | [] -> assert false
  | [x] -> ([], x)
  | hd :: tl ->
    let lst, last = split_last tl in
    (hd :: lst, last)

let may = Stdlib.Option.iter
let may_map = Stdlib.Option.map

(* File functions *)

let find_in_path_uncap path name =
  let uname = String.uncapitalize_ascii name in
  let rec try_dir = function
    | [] -> raise Not_found
    | dir :: rem ->
      let fullname = Filename.concat dir name
      and ufullname = Filename.concat dir uname in
      if Sys.file_exists ufullname then ufullname
      else if Sys.file_exists fullname then fullname
      else try_dir rem
  in
  try_dir path

let remove_file filename =
  try if Sys.file_exists filename then Sys.remove filename
  with Sys_error _msg -> ()

(* Expand a -I option: if it starts with +, make it relative to the standard
   library directory *)

let expand_directory alt s =
  if String.length s > 0 && s.[0] = '+' then
    Filename.concat alt (String.sub s 1 (String.length s - 1))
  else s

(* Hashtable functions *)

let create_hashtable init =
  let size = Array.length init in
  let tbl = Hashtbl.create size in
  Array.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(* File copy *)

let output_to_bin_file_directly filename fn =
  let oc = open_out_bin filename in
  match fn filename oc with
  | v ->
    close_out oc;
    v
  | exception e ->
    close_out oc;
    raise e

let output_to_file_via_temporary ?(mode = [Open_text]) filename fn =
  let temp_filename, oc =
    Filename.open_temp_file ~mode ~perms:0o666
      ~temp_dir:(Filename.dirname filename)
      (Filename.basename filename)
      ".tmp"
  in
  (* The 0o666 permissions will be modified by the umask.  It's just
     like what [open_out] and [open_out_bin] do.
     With temp_dir = dirname filename, we ensure that the returned
     temp file is in the same directory as filename itself, making
     it safe to rename temp_filename to filename later.
     With prefix = basename filename, we are almost certain that
     the first generated name will be unique.  A fixed prefix
     would work too but might generate more collisions if many
     files are being produced simultaneously in the same directory. *)
  match fn temp_filename oc with
  | res -> (
    close_out oc;
    try
      Sys.rename temp_filename filename;
      res
    with exn ->
      remove_file temp_filename;
      raise exn)
  | exception exn ->
    close_out oc;
    remove_file temp_filename;
    raise exn

(* Integer operations *)

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n asr 1)

module Int_literal_converter = struct
  (* To convert integer literals, allowing max_int + 1 (PR#4210) *)
  let cvt_int_aux str neg of_string =
    if String.length str = 0 || str.[0] = '-' then of_string str
    else neg (of_string ("-" ^ str))
  let int s = cvt_int_aux s ( ~- ) int_of_string
  let int32 s = cvt_int_aux s Int32.neg Int32.of_string
  let int64 s = cvt_int_aux s Int64.neg Int64.of_string
end

(* String operations *)

let chop_extensions file =
  let dirname = Filename.dirname file and basename = Filename.basename file in
  try
    let pos = String.index basename '.' in
    let basename = String.sub basename 0 pos in
    if Filename.is_implicit file && dirname = Filename.current_dir_name then
      basename
    else Filename.concat dirname basename
  with Not_found -> file

let get_ref r =
  let v = !r in
  r := [];
  v

let fst3 (x, _, _) = x
let edit_distance a b cutoff =
  let la, lb = (String.length a, String.length b) in
  let cutoff =
    (* using max_int for cutoff would cause overflows in (i + cutoff + 1);
       we bring it back to the (max la lb) worstcase *)
    min (max la lb) cutoff
  in
  if abs (la - lb) > cutoff then None
  else
    (* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. *)
    let m = Array.make_matrix (la + 1) (lb + 1) (cutoff + 1) in
    m.(0).(0) <- 0;
    for i = 1 to la do
      m.(i).(0) <- i
    done;
    for j = 1 to lb do
      m.(0).(j) <- j
    done;
    for i = 1 to la do
      for j = max 1 (i - cutoff - 1) to min lb (i + cutoff + 1) do
        let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
        let best =
          (* insert, delete or substitute *)
          min (1 + min m.(i - 1).(j) m.(i).(j - 1)) (m.(i - 1).(j - 1) + cost)
        in
        let best =
          (* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues *)
          if
            not
              (i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1])
          then best
          else min best (m.(i - 2).(j - 2) + cost)
        in
        m.(i).(j) <- best
      done
    done;
    let result = m.(la).(lb) in
    if result > cutoff then None else Some result

let spellcheck env name =
  let cutoff =
    match String.length name with
    | 1 | 2 -> 0
    | 3 | 4 -> 1
    | 5 | 6 -> 2
    | _ -> 3
  in
  let compare target acc head =
    match edit_distance target head cutoff with
    | None -> acc
    | Some dist ->
      let best_choice, best_dist = acc in
      if dist < best_dist then ([head], dist)
      else if dist = best_dist then (head :: best_choice, dist)
      else acc
  in
  fst (List.fold_left (compare name) ([], max_int) env)

let did_you_mean ppf get_choices =
  (* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. *)
  Format.fprintf ppf "@?";
  match get_choices () with
  | [] -> ()
  | choices ->
    let rest, last = split_last choices in
    Format.fprintf ppf "@\nHint: Did you mean %s%s%s?@?"
      (String.concat ", " rest)
      (if rest = [] then "" else " or ")
      last

module String_set = Set.Make (struct
  type t = string
  let compare = compare
end)
module String_map = Map.Make (struct
  type t = string
  let compare = compare
end)

(* Color handling *)
module Color = struct
  (* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
  type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset
    | Dim

  let ansi_of_color = function
    | Black -> "0"
    | Red -> "1"
    | Green -> "2"
    | Yellow -> "3"
    | Blue -> "4"
    | Magenta -> "5"
    | Cyan -> "6"
    | White -> "7"

  let code_of_style = function
    | FG c -> "3" ^ ansi_of_color c
    | BG c -> "4" ^ ansi_of_color c
    | Bold -> "1"
    | Reset -> "0"
    | Dim -> "2"

  let ansi_of_style_l l =
    let s =
      match l with
      | [] -> code_of_style Reset
      | [s] -> code_of_style s
      | _ -> String.concat ";" (List.map code_of_style l)
    in
    "\x1b[" ^ s ^ "m"

  type styles = {error: style list; warning: style list; loc: style list}

  let default_styles =
    {warning = [Bold; FG Magenta]; error = [Bold; FG Red]; loc = [Bold]}

  let cur_styles = ref default_styles
  let style_of_tag s =
    match s with
    | Format.String_tag "error" -> !cur_styles.error
    | Format.String_tag "warning" -> !cur_styles.warning
    | Format.String_tag "loc" -> !cur_styles.loc
    | Format.String_tag "info" -> [Bold; FG Yellow]
    | Format.String_tag "dim" -> [Dim]
    | Format.String_tag "filename" -> [FG Cyan]
    | _ -> raise Not_found

  let color_enabled = ref true

  (* either prints the tag of [s] or delegates to [or_else] *)
  let mark_open_tag ~or_else s =
    try
      let style = style_of_tag s in
      if !color_enabled then ansi_of_style_l style else ""
    with Not_found -> or_else s

  let mark_close_tag ~or_else s =
    try
      let _ = style_of_tag s in
      if !color_enabled then ansi_of_style_l [Reset] else ""
    with Not_found -> or_else s

  (* add color handling to formatter [ppf] *)
  let set_color_tag_handling ppf =
    let open Format in
    let functions = pp_get_formatter_stag_functions ppf () in
    let functions' =
      {
        functions with
        mark_open_stag = mark_open_tag ~or_else:functions.mark_open_stag;
        mark_close_stag = mark_close_tag ~or_else:functions.mark_close_stag;
      }
    in
    pp_set_mark_tags ppf true;
    (* enable tags *)
    pp_set_formatter_stag_functions ppf functions';
    (* also setup margins *)
    pp_set_margin ppf (pp_get_margin std_formatter ());
    ()

  external isatty : out_channel -> bool = "caml_sys_isatty"

  (* reasonable heuristic on whether colors should be enabled *)
  let should_enable_color () =
    let term = try Sys.getenv "TERM" with Not_found -> "" in
    term <> "dumb" && term <> "" && isatty stderr

  type setting = Auto | Always | Never

  let setup =
    let first = ref true in
    (* initialize only once *)
    let formatter_l =
      [Format.std_formatter; Format.err_formatter; Format.str_formatter]
    in
    fun o ->
      if !first then (
        first := false;
        Format.set_mark_tags true;
        List.iter set_color_tag_handling formatter_l;
        color_enabled :=
          match o with
          | Some Always -> true
          | Some Auto -> should_enable_color ()
          | Some Never -> false
          | None -> should_enable_color ());
      ()
end

let normalise_eol s =
  let b = Buffer.create 80 in
  for i = 0 to String.length s - 1 do
    if s.[i] <> '\r' then Buffer.add_char b s.[i]
  done;
  Buffer.contents b
