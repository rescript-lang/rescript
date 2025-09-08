/*
  This file is taken from ReScript's super_code_frame.ml and super_location.ml
  We're copying the look of ReScript's terminal error reporting.
  See https://github.com/rescript-lang/syntax/pull/77 for the rationale.
  A few lines have been commented out and swapped for their tweaked version.
*/

/* ===== super_code_frame.ml */

open P

module Super_code_frame = {
  let digits_count = n => {
    let rec loop = (n, base, count) =>
      if n >= base {
        loop(n, base * 10, count + 1)
      } else {
        count
      }

    loop(abs(n), 1, 0)
  }

  let seek_2_lines_before = (src, pos) => {
    open Lexing
    let original_line = pos.pos_lnum
    let rec loop = (current_line, current_char) =>
      if current_line + 2 >= original_line {
        (current_char, current_line)
      } else {
        loop(
          if @doesNotRaise String.get(src, current_char) == '\n' {
            current_line + 1
          } else {
            current_line
          },
          current_char + 1,
        )
      }

    loop(1, 0)
  }

  let seek_2_lines_after = (src, pos) => {
    open Lexing
    let original_line = pos.pos_lnum
    let rec loop = (current_line, current_char) =>
      if current_char == String.length(src) {
        (current_char, current_line)
      } else {
        switch @doesNotRaise
        String.get(src, current_char) {
        | '\n' if current_line == original_line + 2 => (current_char, current_line)
        | '\n' => loop(current_line + 1, current_char + 1)
        | _ => loop(current_line, current_char + 1)
        }
      }

    loop(original_line, pos.pos_cnum)
  }

  let leading_space_count = str => {
    let rec loop = (i, count) =>
      if i == String.length(str) {
        count
      } else if @doesNotRaise String.get(str, i) !== ' ' {
        count
      } else {
        loop(i + 1, count + 1)
      }

    loop(0, 0)
  }

  let break_long_line = (max_width, line) => {
    let rec loop = (pos, accum) =>
      if pos == String.length(line) {
        accum
      } else {
        let chunk_length = min(max_width, String.length(line) - pos)
        let chunk = (@doesNotRaise String.sub)(line, pos, chunk_length)
        loop(pos + chunk_length, list{chunk, ...accum})
      }

    loop(0, list{}) |> List.rev
  }

  let filter_mapi = (f, l) => {
    let rec loop = (f, l, i, accum) =>
      switch l {
      | list{} => accum
      | list{head, ...rest} =>
        let accum = switch f(i, head) {
        | None => accum
        | Some(result) => list{result, ...accum}
        }

        loop(f, rest, i + 1, accum)
      }

    loop(f, l, 0, list{}) |> List.rev
  }

  /* Spiritual equivalent of
  https://github.com/ocaml/ocaml/blob/414bdec9ae387129b8102cc6bf3c0b6ae173eeb9/utils/misc.ml#L601
*/
  module Color = {
    type color =
      | Dim
      /* | Filename */
      | Err
      | Warn
      | NoColor

    let dim = "\x1b[2m"
    /* let filename = "\x1b[46m" */
    let err = "\x1b[1;31m"
    let warn = "\x1b[1;33m"
    let reset = "\x1b[0m"

    external isatty: out_channel => bool = "caml_sys_isatty"
    /* reasonable heuristic on whether colors should be enabled */
    let should_enable_color = () => {
      let term = try Sys.getenv("TERM") catch {
      | Not_found => ""
      }
      term != "dumb" && (term != "" && isatty(stderr))
    }

    let color_enabled = ref(true)

    let setup = {
      let first = ref(true) /* initialize only once */
      o => {
        if first.contents {
          first := false
          color_enabled :=
            switch o {
            | Some(Misc.Color.Always) => true
            | Some(Auto) => should_enable_color()
            | Some(Never) => false
            | None => should_enable_color()
            }
        }
        ()
      }
    }
  }

  let setup = Color.setup

  type gutter = Number(int) | Elided
  type highlighted_string = {s: string, start: int, end_: int}
  type line = {
    gutter: gutter,
    content: list<highlighted_string>,
  }
  /*
  Features:
  - display a line gutter
  - break long line into multiple for terminal display
  - peek 2 lines before & after for context
  - center snippet when it's heavily indented
  - ellide intermediate lines when the reported range is huge
*/
  let print = (~is_warning, ~src, ~startPos, ~endPos) => {
    open Lexing

    let indent = 2
    let highlight_line_start_line = startPos.pos_lnum
    let highlight_line_end_line = endPos.pos_lnum
    let (start_line_line_offset, first_shown_line) = seek_2_lines_before(src, startPos)
    let (end_line_line_end_offset, last_shown_line) = seek_2_lines_after(src, endPos)

    let more_than_5_highlighted_lines = highlight_line_end_line - highlight_line_start_line + 1 > 5

    let max_line_digits_count = digits_count(last_shown_line)
    /* TODO: change this back to a fixed 100? */
    /* 3 for separator + the 2 spaces around it */
    let line_width = 78 - max_line_digits_count - indent - 3
    let lines =
      (@doesNotRaise String.sub)(
        src,
        start_line_line_offset,
        end_line_line_end_offset - start_line_line_offset,
      )
      |> String.split_on_char('\n')
      |> filter_mapi((i, line) => {
        let line_number = i + first_shown_line
        if more_than_5_highlighted_lines {
          if line_number == highlight_line_start_line + 2 {
            Some(Elided, line)
          } else if (
            line_number > highlight_line_start_line + 2 && line_number < highlight_line_end_line - 1
          ) {
            None
          } else {
            Some(Number(line_number), line)
          }
        } else {
          Some(Number(line_number), line)
        }
      })

    let leading_space_to_cut = lines |> List.fold_left((current_max, (_, line)) => {
      let leading_spaces = leading_space_count(line)
      if String.length(line) == leading_spaces {
        /* the line's nothing but spaces. Doesn't count */
        current_max
      } else {
        min(leading_spaces, current_max)
      }
    }, 99999)

    let separator = if leading_space_to_cut == 0 {
      "│"
    } else {
      "┆"
    }
    let stripped_lines = lines |> List.map(((gutter, line)) => {
      let new_content = if String.length(line) <= leading_space_to_cut {
        list{{s: "", start: 0, end_: 0}}
      } else {
        (@doesNotRaise String.sub)(
          line,
          leading_space_to_cut,
          String.length(line) - leading_space_to_cut,
        )
        |> break_long_line(line_width)
        |> List.mapi((i, line) =>
          switch gutter {
          | Elided => {s: line, start: 0, end_: 0}
          | Number(line_number) =>
            let highlight_line_start_offset = startPos.pos_cnum - startPos.pos_bol
            let highlight_line_end_offset = endPos.pos_cnum - endPos.pos_bol
            let start = if i == 0 && line_number == highlight_line_start_line {
              highlight_line_start_offset - leading_space_to_cut
            } else {
              0
            }

            let end_ = if line_number < highlight_line_start_line {
              0
            } else if (
              line_number == highlight_line_start_line && line_number == highlight_line_end_line
            ) {
              highlight_line_end_offset - leading_space_to_cut
            } else if line_number == highlight_line_start_line {
              String.length(line)
            } else if (
              line_number > highlight_line_start_line && line_number < highlight_line_end_line
            ) {
              String.length(line)
            } else if line_number == highlight_line_end_line {
              highlight_line_end_offset - leading_space_to_cut
            } else {
              0
            }

            {s: line, start: start, end_: end_}
          }
        )
      }

      {gutter: gutter, content: new_content}
    })

    let buf = Buffer.create(100)
    open Color
    let add_ch = {
      let last_color = ref(NoColor)
      (color, ch) =>
        if !Color.color_enabled.contents || last_color.contents == color {
          Buffer.add_char(buf, ch)
        } else {
          let ansi = switch (last_color.contents, color) {
          | (NoColor, Dim) => dim
          /* | NoColor, Filename -> filename */
          | (NoColor, Err) => err
          | (NoColor, Warn) => warn
          | (_, NoColor) => reset
          | (_, Dim) => reset ++ dim
          /* | _, Filename -> reset ^ filename */
          | (_, Err) => reset ++ err
          | (_, Warn) => reset ++ warn
          }

          Buffer.add_string(buf, ansi)
          Buffer.add_char(buf, ch)
          last_color := color
        }
    }

    let draw_gutter = (color, s) => {
      for _i in 1 to max_line_digits_count + indent - String.length(s) {
        add_ch(NoColor, ' ')
      }
      s |> String.iter(add_ch(color))
      add_ch(NoColor, ' ')
      separator |> String.iter(add_ch(Dim))
      add_ch(NoColor, ' ')
    }

    stripped_lines |> List.iter(({gutter, content}) =>
      switch gutter {
      | Elided =>
        draw_gutter(Dim, ".")
        add_ch(Dim, '.')
        add_ch(Dim, '.')
        add_ch(Dim, '.')
        add_ch(NoColor, '\n')
      | Number(line_number) =>
        content |> List.iteri((i, line) => {
          let gutter_content = if i == 0 {
            string_of_int(line_number)
          } else {
            ""
          }
          let gutter_color = if (
            i == 0 &&
              (line_number >= highlight_line_start_line &&
              line_number <= highlight_line_end_line)
          ) {
            if is_warning {
              Warn
            } else {
              Err
            }
          } else {
            NoColor
          }

          draw_gutter(gutter_color, gutter_content)

          line.s |> String.iteri((ii, ch) => {
            let c = if ii >= line.start && ii < line.end_ {
              if is_warning {
                Warn
              } else {
                Err
              }
            } else {
              NoColor
            }
            add_ch(c, ch)
          })
          add_ch(NoColor, '\n')
        })
      }
    )
    Buffer.contents(buf)
  }
}

/* ===== super_location.ml */
module Super_location = {
  let fprintf = Format.fprintf

  let print_filename = Location.print_filename

  /* let print ~message_kind intro ppf (loc : Location.t) = */
  let print = (~message_kind, intro, src, ppf, loc: Location.t) => {
    switch message_kind {
    | #warning => fprintf(ppf, "@[@{<info>%s@}@]@,", intro)
    | #warning_as_error => fprintf(ppf, "@[@{<error>%s@} (configured as error) @]@,", intro)
    | #error => fprintf(ppf, "@[@{<error>%s@}@]@,", intro)
    }
    /* ocaml's reported line/col numbering is horrible and super error-prone
     when being handled programmatically (or humanly for that matter. If you're
     an ocaml contributor reading this: who the heck reads the character count
     starting from the first erroring character?) */
    /* let (file, start_line, start_char) = Location.get_pos_info loc.loc_start in */
    let (_file, start_line, start_char) = Location.get_pos_info(loc.loc_start)
    let (_, end_line, end_char) = Location.get_pos_info(loc.loc_end)
    /* line is 1-indexed, column is 0-indexed. We convert all of them to 1-indexed to avoid confusion */
    /* start_char is inclusive, end_char is exclusive */
    let normalizedRange = /* TODO: lots of the handlings here aren't needed anymore because the new
      rescript syntax has much stronger invariants regarding positions, e.g.
      no -1 */
    if start_char === -1 || end_char === -1 {
      /* happens sometimes. Syntax error for example */
      None
    } else if start_line == end_line && start_char >= end_char {
      /* in some errors, starting char and ending char can be the same. But
         since ending char was supposed to be exclusive, here it might end up
         smaller than the starting char if we naively did start_char + 1 to
         just the starting char and forget ending char */
      let same_char = start_char + 1
      Some((start_line, same_char), (end_line, same_char))
    } else {
      /* again: end_char is exclusive, so +1-1=0 */
      Some((start_line, start_char + 1), (end_line, end_char))
    }

    switch normalizedRange {
    | None => ()
    | Some(_) =>
      try /* let src = Ext_io.load_file file in */
      /* we're putting the line break `@,` here rather than above, because this
           branch might not be reached (aka no inline file content display) so
           we don't wanna end up with two line breaks in the the consequent */
      fprintf(
        ppf,
        "@,%s",
        Super_code_frame.print(
          ~is_warning=message_kind == #warning,
          ~src,
          ~startPos=loc.loc_start,
          ~endPos=loc.loc_end,
        ),
      ) catch {
      /* this might happen if the file is e.g. "", "_none_" or any of the fake file name placeholders.
       we've already printed the location above, so nothing more to do here. */
      | Sys_error(_) => ()
      }
    }
  }

  /* taken from https://github.com/rescript-lang/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/parsing/location.ml#L380 */
  /* This is the error report entry point. We'll replace the default reporter with this one. */
  /* let rec super_error_reporter ppf ({loc; msg; sub} : Location.error) = */
  let super_error_reporter = (ppf, src, {loc, msg}: Location.error) =>
    /* open a vertical box. Everything in our message is indented 2 spaces */
    /* Format.fprintf ppf "@[<v>@,  %a@,  %s@,@]" (print ~message_kind:`error "We've found a bug for you!") src loc msg; */
    Format.fprintf(
      ppf,
      "@[<v>@,  %a@,  %s@,@]",
      print(~message_kind=#error, "Syntax error!", src),
      loc,
      msg,
    )
  /* List.iter (Format.fprintf ppf "@,@[%a@]" super_error_reporter) sub */
  /* no need to flush here; location's report_exception (which uses this ultimately) flushes */
}

