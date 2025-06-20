/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

open P

open Lexing

let absname = ref(false)
/* This reference should be in Clflags, but it would create an additional
 dependency and make bootstrapping Camlp4 more difficult. */

type t = Warnings.loc = {loc_start: position, loc_end: position, loc_ghost: bool}

let in_file = name => {
  let loc = {
    pos_fname: name,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: -1,
  }
  {loc_start: loc, loc_end: loc, loc_ghost: true}
}

let none = in_file("_none_")

let curr = lexbuf => {
  loc_start: lexbuf.lex_start_p,
  loc_end: lexbuf.lex_curr_p,
  loc_ghost: false,
}

let init = (lexbuf, fname) =>
  lexbuf.lex_curr_p = {
    pos_fname: fname,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: 0,
  }

let symbol_rloc = () => {
  loc_start: Parsing.symbol_start_pos(),
  loc_end: Parsing.symbol_end_pos(),
  loc_ghost: false,
}

let symbol_gloc = () => {
  loc_start: Parsing.symbol_start_pos(),
  loc_end: Parsing.symbol_end_pos(),
  loc_ghost: true,
}

let rhs_loc = n => {
  loc_start: Parsing.rhs_start_pos(n),
  loc_end: Parsing.rhs_end_pos(n),
  loc_ghost: false,
}

let input_name = ref("_none_")
let input_lexbuf = ref((None: option<lexbuf>))
let set_input_name = name =>
  if name != "" {
    input_name := name
  }
/* Terminal info */

let num_loc_lines = ref(0) /* number of lines already printed after input */

/* Print the location in some way or another */

open Format

let absolute_path = s => {
  /* This function could go into Filename */
  open Filename
  let s = if is_relative(s) {
    concat(Sys.getcwd(), s)
  } else {
    s
  }
  /* Now simplify . and .. components */
  let rec aux = s => {
    let base = basename(s)
    let dir = dirname(s)
    if dir == s {
      dir
    } else if base == current_dir_name {
      aux(dir)
    } else if base == parent_dir_name {
      dirname(aux(dir))
    } else {
      concat(aux(dir), base)
    }
  }

  aux(s)
}

let show_filename = file => {
  let file = if file == "_none_" {
    input_name.contents
  } else {
    file
  }
  if absname.contents {
    absolute_path(file)
  } else {
    file
  }
}

let print_filename = (ppf, file) => Format.fprintf(ppf, "%s", show_filename(file))

let reset = () => num_loc_lines := 0

let (msg_file, msg_line, msg_chars, msg_to, msg_colon) = (
  "File \"",
  "\", line ",
  ", characters ",
  "-",
  ":",
)

/* return file, line, char from the given position */
let get_pos_info = pos => (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

let error_prefix = "Error"
let warning_prefix = "Warning"

let print_compact = (ppf, loc) => {
  let (file, line, startchar) = get_pos_info(loc.loc_start)
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar
  fprintf(ppf, "%a:%i", print_filename, file, line)
  if startchar >= 0 {
    fprintf(ppf, ",%i--%i", startchar, endchar)
  }
}

let echo_eof = () => {
  print_newline()
  incr(num_loc_lines)
}

type loc<'a> = {
  txt: 'a,
  loc: t,
}

let mkloc = (txt, loc) => {txt: txt, loc: loc}
let mknoloc = txt => mkloc(txt, none)

type rec error = {
  loc: t,
  msg: string,
  sub: list<error>,
  if_highlight: string /* alternative message if locations are highlighted */,
}

let pp_ksprintf = (~before=?, k, fmt) => {
  let buf = Buffer.create(64)
  let ppf = Format.formatter_of_buffer(buf)
  Misc.Color.set_color_tag_handling(ppf)
  switch before {
  | None => ()
  | Some(f) => f(ppf)
  }
  kfprintf(_ => {
    pp_print_flush(ppf, ())
    let msg = Buffer.contents(buf)
    k(msg)
  }, ppf, fmt)
}

/* Shift the formatter's offset by the length of the error prefix, which
 is always added by the compiler after the message has been formatted */
let print_phanton_error_prefix = ppf =>
  Format.pp_print_as(ppf, String.length(error_prefix) + 2 /* ": " */, "")

let errorf = (~loc=none, ~sub=list{}, ~if_highlight="", fmt) =>
  pp_ksprintf(
    ~before=print_phanton_error_prefix,
    msg => {loc: loc, msg: msg, sub: sub, if_highlight: if_highlight},
    fmt,
  )

let error = (~loc=none, ~sub=list{}, ~if_highlight="", msg) => {
  loc: loc,
  msg: msg,
  sub: sub,
  if_highlight: if_highlight,
}

let error_of_exn: ref<list<exn => option<error>>> = ref(list{})

let register_error_of_exn = f => error_of_exn := list{f, ...error_of_exn.contents}

exception Already_displayed_error = Warnings.Errors

let error_of_exn = exn =>
  switch exn {
  | Already_displayed_error => Some(#Already_displayed)
  | _ =>
    let rec loop = x =>
      switch x {
      | list{} => None
      | list{f, ...rest} =>
        switch f(exn) {
        | Some(error) => Some(#Ok(error))
        | None => loop(rest)
        }
      }

    loop(error_of_exn.contents)
  }

let error_of_printer = (loc, print, x) => errorf(~loc, "%a@?", print, x)

let error_of_printer_file = (print, x) => error_of_printer(in_file(input_name.contents), print, x)

let () = register_error_of_exn(x =>
  switch x {
  | Sys_error(msg) => Some(errorf(~loc=in_file(input_name.contents), "I/O error: %s", msg))

  | Misc.HookExnWrapper({error: e, hook_name, hook_info: {Misc.sourcefile: sourcefile}}) =>
    let sub = switch error_of_exn(e) {
    | None | Some(#Already_displayed) => error(Printexc.to_string(e))
    | Some(#Ok(err)) => err
    }

    Some(errorf(~loc=in_file(sourcefile), "In hook %S:", hook_name, ~sub=list{sub}))
  | _ => None
  }
)

external reraise: exn => 'a = "%reraise"

exception Error(error)

let () = register_error_of_exn(x =>
  switch x {
  | Error(e) => Some(e)
  | _ => None
  }
)

@raises(Error)
let raise_errorf = (~loc=none, ~sub=list{}, ~if_highlight="") =>
  pp_ksprintf(~before=print_phanton_error_prefix, msg =>
    throw(Error({loc: loc, msg: msg, sub: sub, if_highlight: if_highlight}))
  )
