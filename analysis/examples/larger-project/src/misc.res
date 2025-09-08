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

/* Errors */

open P

exception Fatal_error

@raises(Fatal_error)
let fatal_error = msg => {
  print_string(">> Fatal error: ")
  prerr_endline(msg)
  throw(Fatal_error)
}

@raises(Fatal_error)
let fatal_errorf = fmt => Format.kasprintf(fatal_error, fmt)

/* Exceptions */

@raises(genericException)
let try_finally = (work, cleanup) => {
  let result = try work() catch {
  | e =>
    cleanup()
    throw(e)
  }
  cleanup()
  result
}

type rec ref_and_value = R(ref<'a>, 'a): ref_and_value

@raises(genericException)
let protect_refs = {
  let set_refs = l => List.iter((R(r, v)) => r := v, l)
  (refs, f) => {
    let backup = List.map((R(r, _)) => R(r, r.contents), refs)
    set_refs(refs)
    switch f() {
    | x =>
      set_refs(backup)
      x
    | exception e =>
      set_refs(backup)
      throw(e)
    }
  }
}

/* List functions */

let rec map_end = (f, l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{f(hd), ...map_end(f, tl, l2)}
  }

let rec map_left_right = (f, x) =>
  switch x {
  | list{} => list{}
  | list{hd, ...tl} =>
    let res = f(hd)
    list{res, ...map_left_right(f, tl)}
  }

let rec for_all2 = (pred, l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => true
  | (list{hd1, ...tl1}, list{hd2, ...tl2}) => pred(hd1, hd2) && for_all2(pred, tl1, tl2)
  | (_, _) => false
  }

let rec replicate_list = (elem, n) =>
  if n <= 0 {
    list{}
  } else {
    list{elem, ...replicate_list(elem, n - 1)}
  }

let rec list_remove = (x, y) => assert false

let rec split_last = x =>
  switch x {
  | list{} => assert false
  | list{x} => (list{}, x)
  | list{hd, ...tl} =>
    let (lst, last) = split_last(tl)
    (list{hd, ...lst}, last)
  }

module Stdlib = {
  module List = {
    type t<'a> = list<'a>

    let rec compare = (cmp, l1, l2) =>
      switch (l1, l2) {
      | (list{}, list{}) => 0
      | (list{}, list{_, ..._}) => -1
      | (list{_, ..._}, list{}) => 1
      | (list{h1, ...t1}, list{h2, ...t2}) =>
        let c = cmp(h1, h2)
        if c != 0 {
          c
        } else {
          compare(cmp, t1, t2)
        }
      }

    let rec equal = (eq, l1, l2) =>
      switch (l1, l2) {
      | (list{}, list{}) => true
      | (list{hd1, ...tl1}, list{hd2, ...tl2}) => eq(hd1, hd2) && equal(eq, tl1, tl2)
      | (_, _) => false
      }

    @raises(Invalid_argument)
    let filter_map = (f, l) => {
      @raises(Invalid_argument)
      let rec aux = (acc, l) =>
        switch l {
        | list{} => List.rev(acc)
        | list{h, ...t} =>
          switch f(h) {
          | None => aux(acc, t)
          | Some(v) => aux(list{v, ...acc}, t)
          }
        }

      aux(list{}, l)
    }

    @raises(Invalid_argument)
    let map2_prefix = (f, l1, l2) => {
      @raises(Invalid_argument)
      let rec aux = (acc, l1, l2) =>
        switch (l1, l2) {
        | (list{}, _) => (List.rev(acc), l2)
        | (list{_, ..._}, list{}) => throw(Invalid_argument("map2_prefix"))
        | (list{h1, ...t1}, list{h2, ...t2}) =>
          let h = f(h1, h2)
          aux(list{h, ...acc}, t1, t2)
        }

      aux(list{}, l1, l2)
    }

    @raises(Invalid_argument)
    let some_if_all_elements_are_some = l => {
      @raises(Invalid_argument)
      let rec aux = (acc, l) =>
        switch l {
        | list{} => Some(List.rev(acc))
        | list{None, ..._} => None
        | list{Some(h), ...t} => aux(list{h, ...acc}, t)
        }

      aux(list{}, l)
    }

    @raises(Invalid_argument)
    let split_at = (n, l) => {
      @raises(Invalid_argument)
      let rec aux = (n, acc, l) =>
        if n == 0 {
          (List.rev(acc), l)
        } else {
          switch l {
          | list{} => throw(Invalid_argument("split_at"))
          | list{t, ...q} => aux(n - 1, list{t, ...acc}, q)
          }
        }

      aux(n, list{}, l)
    }
  }

  module Option = {
    type t<'a> = option<'a>

    let equal = (eq, o1, o2) =>
      switch (o1, o2) {
      | (None, None) => true
      | (Some(e1), Some(e2)) => eq(e1, e2)
      | (_, _) => false
      }

    let iter = (f, x) =>
      switch x {
      | Some(x) => f(x)
      | None => ()
      }

    let map = (f, x) =>
      switch x {
      | Some(x) => Some(f(x))
      | None => None
      }

    let fold = (f, a, b) =>
      switch a {
      | None => b
      | Some(a) => f(a, b)
      }

    let value_default = (f, ~default, a) =>
      switch a {
      | None => default
      | Some(a) => f(a)
      }
  }

  module Array = {
    @raises(Invalid_argument)
    let exists2 = (p, a1, a2) => {
      let n = Array.length(a1)
      if Array.length(a2) != n {
        invalid_arg("Misc.Stdlib.Array.exists2")
      }
      let rec loop = i =>
        if i == n {
          false
        } else if p(Array.unsafe_get(a1, i), Array.unsafe_get(a2, i)) {
          true
        } else {
          loop(succ(i))
        }
      loop(0)
    }
  }
}

let may = Stdlib.Option.iter
let may_map = Stdlib.Option.map

/* File functions */

@raises(Not_found)
let find_in_path = (path, name) =>
  if !Filename.is_implicit(name) {
    if Sys.file_exists(name) {
      name
    } else {
      throw(Not_found)
    }
  } else {
    @raises(Not_found)
    let rec try_dir = x =>
      switch x {
      | list{} => throw(Not_found)
      | list{dir, ...rem} =>
        let fullname = Filename.concat(dir, name)
        if Sys.file_exists(fullname) {
          fullname
        } else {
          try_dir(rem)
        }
      }
    try_dir(path)
  }

@raises(Not_found)
let find_in_path_rel = (path, name) => {
  let rec simplify = s => {
    open Filename
    let base = basename(s)
    let dir = dirname(s)
    if dir == s {
      dir
    } else if base == current_dir_name {
      simplify(dir)
    } else {
      concat(simplify(dir), base)
    }
  }

  @raises(Not_found)
  let rec try_dir = x =>
    switch x {
    | list{} => throw(Not_found)
    | list{dir, ...rem} =>
      let fullname = simplify(Filename.concat(dir, name))
      if Sys.file_exists(fullname) {
        fullname
      } else {
        try_dir(rem)
      }
    }
  try_dir(path)
}

@raises(Not_found)
let find_in_path_uncap = (path, name) => {
  let uname = String.uncapitalize_ascii(name)

  @raises(Not_found)
  let rec try_dir = x =>
    switch x {
    | list{} => throw(Not_found)
    | list{dir, ...rem} =>
      let fullname = Filename.concat(dir, name)
      and ufullname = Filename.concat(dir, uname)
      if Sys.file_exists(ufullname) {
        ufullname
      } else if Sys.file_exists(fullname) {
        fullname
      } else {
        try_dir(rem)
      }
    }
  try_dir(path)
}


let remove_file = filename =>
  try if Sys.file_exists(filename) {
    Sys.remove(filename)
  } catch {
  | Sys_error(_msg) => ()
  }

/* Expand a -I option: if it starts with +, make it relative to the standard
 library directory */

@raises(Invalid_argument)
let expand_directory = (alt, s) =>
  if String.length(s) > 0 && String.get(s, 0) == '+' {
    Filename.concat(alt, String.sub(s, 1, String.length(s) - 1))
  } else {
    s
  }

/* Hashtable functions */

let create_hashtable = (size, init) => {
  let tbl = Hashtbl.create(size)
  List.iter(((key, data)) => Hashtbl.add(tbl, key, data), init)
  tbl
}

/* File copy */

@raises(Invalid_argument)
let copy_file = (ic, oc) => {
  let buff = Bytes.create(0x1000)

  @raises(Invalid_argument)
  let rec copy = () => {
    let n = input(ic, buff, 0, 0x1000)
    if n == 0 {
      ()
    } else {
      output(oc, buff, 0, n)
      copy()
    }
  }
  copy()
}

@raises(Invalid_argument)
let copy_file_chunk = (ic, oc, len) => {
  let buff = Bytes.create(0x1000)

  @raises([End_of_file, Invalid_argument])
  let rec copy = n =>
    if n <= 0 {
      ()
    } else {
      let r = input(ic, buff, 0, min(n, 0x1000))
      if r == 0 {
        throw(End_of_file)
      } else {
        output(oc, buff, 0, r)
        copy(n - r)
      }
    }
  copy(len)
}

@raises(Invalid_argument)
let string_of_file = ic => {
  let b = Buffer.create(0x10000)
  let buff = Bytes.create(0x1000)

  @raises(Invalid_argument)
  let rec copy = () => {
    let n = input(ic, buff, 0, 0x1000)
    if n == 0 {
      Buffer.contents(b)
    } else {
      Buffer.add_subbytes(b, buff, 0, n)
      copy()
    }
  }
  copy()
}

@raises([Sys_error, genericException])
let output_to_file_via_temporary = (~mode=list{Open_text}, filename, fn) => {
  let (temp_filename, oc) = open_temp_file(
    ~mode,
    ~perms=0o666,
    ~temp_dir=Filename.dirname(filename),
    Filename.basename(filename),
    ".tmp",
  )
  /* The 0o666 permissions will be modified by the umask.  It's just
       like what [open_out] and [open_out_bin] do.
       With temp_dir = dirname filename, we ensure that the returned
       temp file is in the same directory as filename itself, making
       it safe to rename temp_filename to filename later.
       With prefix = basename filename, we are almost certain that
       the first generated name will be unique.  A fixed prefix
       would work too but might generate more collisions if many
       files are being produced simultaneously in the same directory. */
  switch fn(temp_filename, oc) {
  | res =>
    close_out(oc)
    try {
      Sys.rename(temp_filename, filename)
      res
    } catch {
    | exn =>
      remove_file(temp_filename)
      throw(exn)
    }
  | exception exn =>
    close_out(oc)
    remove_file(temp_filename)
    throw(exn)
  }
}

/* Integer operations */

let rec log2 = n =>
  if n <= 1 {
    0
  } else {
    1 + log2(asr(n, 1))
  }

let align = (n, a) =>
  if n >= 0 {
    land(n + a - 1, -a)
  } else {
    land(n, -a)
  }

let no_overflow_add = (a, b) => lor(lxor(a, b), lxor(a, lnot(a + b))) < 0

let no_overflow_sub = (a, b) => lor(lxor(a, lnot(b)), lxor(b, a - b)) < 0

@raises(Division_by_zero)
let no_overflow_mul = (a, b) => b != 0 && a * b / b == a

let no_overflow_lsl = (a, k) =>
  0 <= k && (k < Sys.word_size && (asr(min_int, k) <= a && a <= asr(max_int, k)))

module Int_literal_converter = {
  /* To convert integer literals, allowing max_int + 1 (PR#4210) */
  @raises(Invalid_argument)
  let cvt_int_aux = (str, neg, of_string) =>
    if String.length(str) == 0 || String.get(str, 0) == '-' {
      of_string(str)
    } else {
      neg(of_string("-" ++ str))
    }
  @raises([Failure, Invalid_argument])
  let int = s => cvt_int_aux(s, \"~-", int_of_string)
  @raises(Invalid_argument)
  let int32 = s => cvt_int_aux(s, Int32.neg, Int32.of_string)
  @raises(Invalid_argument)
  let int64 = s => cvt_int_aux(s, Int64.neg, Int64.of_string)
  @raises(Invalid_argument)
  let nativeint = s => cvt_int_aux(s, Nativeint.neg, Nativeint.of_string)
}

/* String operations */

@raises(Invalid_argument)
let chop_extensions = file => {
  let dirname = Filename.dirname(file) and basename = Filename.basename(file)
  try {
    let pos = String.index(basename, '.')
    let basename = String.sub(basename, 0, pos)
    if Filename.is_implicit(file) && dirname == Filename.current_dir_name {
      basename
    } else {
      Filename.concat(dirname, basename)
    }
  } catch {
  | Not_found => file
  }
}

@raises(Invalid_argument)
let search_substring = (pat, str, start) => {
  @raises([Invalid_argument, Not_found])
  let rec search = (i, j) =>
    if j >= String.length(pat) {
      i
    } else if i + j >= String.length(str) {
      throw(Not_found)
    } else if String.get(str, i + j) == String.get(pat, j) {
      search(i, j + 1)
    } else {
      search(i + 1, 0)
    }
  search(start, 0)
}

@raises(Invalid_argument)
let replace_substring = (~before, ~after, str) => {
  @raises(Invalid_argument)
  let rec search = (acc, curr) =>
    switch search_substring(before, str, curr) {
    | next =>
      let prefix = String.sub(str, curr, next - curr)
      search(list{prefix, ...acc}, next + String.length(before))
    | exception Not_found =>
      let suffix = String.sub(str, curr, String.length(str) - curr)
      List.rev(list{suffix, ...acc})
    }
  String.concat(after, search(list{}, 0))
}

@raises(Invalid_argument)
let rev_split_words = s => {
  @raises(Invalid_argument)
  let rec split1 = (res, i) =>
    if i >= String.length(s) {
      res
    } else {
      switch String.get(s, i) {
      | ' ' | '\t' | '\r' | '\n' => split1(res, i + 1)
      | _ => split2(res, i, i + 1)
      }
    }
  @raises(Invalid_argument)
  and split2 = (res, i, j) =>
    if j >= String.length(s) {
      list{String.sub(s, i, j - i), ...res}
    } else {
      switch String.get(s, j) {
      | ' ' | '\t' | '\r' | '\n' => split1(list{String.sub(s, i, j - i), ...res}, j + 1)
      | _ => split2(res, i, j + 1)
      }
    }
  split1(list{}, 0)
}

let get_ref = r => {
  let v = r.contents
  r := list{}
  v
}

let fst3 = ((x, _, _)) => x
let snd3 = ((_, x, _)) => x
let thd3 = ((_, _, x)) => x

let fst4 = ((x, _, _, _)) => x
let snd4 = ((_, x, _, _)) => x
let thd4 = ((_, _, x, _)) => x
let for4 = ((_, _, _, x)) => x

module LongString = {
  type t = array<bytes>

  @raises([Division_by_zero, Invalid_argument])
  let create = str_size => {
    let tbl_size = str_size / Sys.max_string_length + 1
    let tbl = Array.make(tbl_size, Bytes.empty)
    for i in 0 to tbl_size - 2 {
      tbl[i] = Bytes.create(Sys.max_string_length)
    }
    tbl[tbl_size - 1] = Bytes.create(mod(str_size, Sys.max_string_length))
    tbl
  }

  @raises(Invalid_argument)
  let length = tbl => {
    let tbl_size = Array.length(tbl)
    Sys.max_string_length * (tbl_size - 1) + Bytes.length(tbl[tbl_size - 1])
  }

  @raises([Division_by_zero, Invalid_argument])
  let get = (tbl, ind) =>
    Bytes.get(tbl[ind / Sys.max_string_length], mod(ind, Sys.max_string_length))

  @raises([Division_by_zero, Invalid_argument])
  let set = (tbl, ind, c) =>
    Bytes.set(tbl[ind / Sys.max_string_length], mod(ind, Sys.max_string_length), c)

  @raises([Division_by_zero, Invalid_argument])
  let blit = (src, srcoff, dst, dstoff, len) =>
    for i in 0 to len - 1 {
      set(dst, dstoff + i, get(src, srcoff + i))
    }

  @raises([Division_by_zero, Invalid_argument])
  let output = (oc, tbl, pos, len) =>
    for i in pos to pos + len - 1 {
      output_char(oc, get(tbl, i))
    }

  @raises([Division_by_zero, Invalid_argument])
  let unsafe_blit_to_bytes = (src, srcoff, dst, dstoff, len) =>
    for i in 0 to len - 1 {
      Bytes.unsafe_set(dst, dstoff + i, get(src, srcoff + i))
    }

  @raises([Division_by_zero, End_of_file, Invalid_argument])
  let input_bytes = (ic, len) => {
    let tbl = create(len)
    Array.iter(str => really_input(ic, str, 0, Bytes.length(str)), tbl)
    tbl
  }
}

@raises(Invalid_argument)
let edit_distance = (a, b, cutoff) => {
  let (la, lb) = (String.length(a), String.length(b))
  let cutoff = /* using max_int for cutoff would cause overflows in (i + cutoff + 1);
   we bring it back to the (max la lb) worstcase */
  min(max(la, lb), cutoff)
  if abs(la - lb) > cutoff {
    None
  } else {
    /* initialize with 'cutoff + 1' so that not-yet-written-to cases have
       the worst possible cost; this is useful when computing the cost of
       a case just at the boundary of the cutoff diagonal. */
    let m = Array.make_matrix(la + 1, lb + 1, cutoff + 1)
    m[0][0] = 0
    for i in 1 to la {
      m[i][0] = i
    }
    for j in 1 to lb {
      m[0][j] = j
    }
    for i in 1 to la {
      for j in max(1, i - cutoff - 1) to min(lb, i + cutoff + 1) {
        let cost = if String.get(a, i - 1) == String.get(b, j - 1) {
          0
        } else {
          1
        }
        let best = /* insert, delete or substitute */
        min(1 + min(m[i - 1][j], m[i][j - 1]), m[i - 1][j - 1] + cost)

        let best = /* swap two adjacent letters; we use "cost" again in case of
             a swap between two identical letters; this is slightly
             redundant as this is a double-substitution case, but it
             was done this way in most online implementations and
             imitation has its virtues */
        if (
          !(
            i > 1 &&
              (j > 1 &&
              (String.get(a, i - 1) == String.get(b, j - 2) &&
                String.get(a, i - 2) == String.get(b, j - 1)))
          )
        ) {
          best
        } else {
          min(best, m[i - 2][j - 2] + cost)
        }

        m[i][j] = best
      }
    }
    let result = m[la][lb]
    if result > cutoff {
      None
    } else {
      Some(result)
    }
  }
}

@raises(Invalid_argument)
let spellcheck = (env, name) => {
  let cutoff = switch String.length(name) {
  | 1 | 2 => 0
  | 3 | 4 => 1
  | 5 | 6 => 2
  | _ => 3
  }

  @raises(Invalid_argument)
  let compare = (target, acc, head) =>
    switch edit_distance(target, head, cutoff) {
    | None => acc
    | Some(dist) =>
      let (best_choice, best_dist) = acc
      if dist < best_dist {
        (list{head}, dist)
      } else if dist == best_dist {
        (list{head, ...best_choice}, dist)
      } else {
        acc
      }
    }

  fst(List.fold_left(compare(name), (list{}, max_int), env))
}

let did_you_mean = (ppf, get_choices) => {
  /* flush now to get the error report early, in the (unheard of) case
     where the search in the get_choices function would take a bit of
     time; in the worst case, the user has seen the error, she can
     interrupt the process before the spell-checking terminates. */
  Format.fprintf(ppf, "@?")
  switch get_choices() {
  | list{} => ()
  | choices =>
    let (rest, last) = split_last(choices)
    Format.fprintf(
      ppf,
      "@\nHint: Did you mean %s%s%s?@?",
      String.concat(", ", rest),
      if rest == list{} {
        ""
      } else {
        " or "
      },
      last,
    )
  }
}

@raises([Invalid_argument, Not_found])
let cut_at = (s, c) => {
  let pos = String.index(s, c)
  (String.sub(s, 0, pos), String.sub(s, pos + 1, String.length(s) - pos - 1))
}

module StringSet = Set.Make({
  type t = string
  let compare = compare
})
module StringMap = Map.Make({
  type t = string
  let compare = compare
})

/* Color handling */
module Color = {
  /* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code */
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type style =
    | FG(color) /* foreground */
    | BG(color) /* background */
    | Bold
    | Reset

  let ansi_of_color = x =>
    switch x {
    | Black => "0"
    | Red => "1"
    | Green => "2"
    | Yellow => "3"
    | Blue => "4"
    | Magenta => "5"
    | Cyan => "6"
    | White => "7"
    }

  let code_of_style = x =>
    switch x {
    | FG(c) => "3" ++ ansi_of_color(c)
    | BG(c) => "4" ++ ansi_of_color(c)
    | Bold => "1"
    | Reset => "0"
    }

  let ansi_of_style_l = l => {
    let s = switch l {
    | list{} => code_of_style(Reset)
    | list{s} => code_of_style(s)
    | _ => String.concat(";", List.map(code_of_style, l))
    }

    "\x1b[" ++ (s ++ "m")
  }

  type styles = {
    error: list<style>,
    warning: list<style>,
    loc: list<style>,
  }

  let default_styles = {
    warning: list{Bold, FG(Magenta)},
    error: list{Bold, FG(Red)},
    loc: list{Bold},
  }

  let cur_styles = ref(default_styles)
  let get_styles = () => cur_styles.contents
  let set_styles = s => cur_styles := s

  /* map a tag to a style, if the tag is known.
   @raise Not_found otherwise */
  @raises(Not_found)
  let style_of_tag = s =>
    switch s {
    | "error" => cur_styles.contents.error
    | "warning" => cur_styles.contents.warning
    | "loc" => cur_styles.contents.loc
    | _ => throw(Not_found)
    }

  let color_enabled = ref(true)

  /* either prints the tag of [s] or delegates to [or_else] */
  let mark_open_tag = (~or_else, s) =>
    try {
      let style = style_of_tag(s)
      if color_enabled.contents {
        ansi_of_style_l(style)
      } else {
        ""
      }
    } catch {
    | Not_found => or_else(s)
    }

  let mark_close_tag = (~or_else, s) =>
    try {
      let _ = style_of_tag(s)
      if color_enabled.contents {
        ansi_of_style_l(list{Reset})
      } else {
        ""
      }
    } catch {
    | Not_found => or_else(s)
    }

  /* add color handling to formatter [ppf] */
  let set_color_tag_handling = ppf => {
    assert false
  }

  external isatty: out_channel => bool = "caml_sys_isatty"

  /* reasonable heuristic on whether colors should be enabled */
  let should_enable_color = () => {
    let term = try Sys.getenv("TERM") catch {
    | Not_found => ""
    }
    term != "dumb" && (term != "" && isatty(stderr))
  }

  type setting = Auto | Always | Never

  let setup = {
    let first = ref(true) /* initialize only once */
    let formatter_l = list{Format.std_formatter, Format.err_formatter, Format.str_formatter}

    o => {
      if first.contents {
        first := false
        Format.set_mark_tags(true)
        List.iter(set_color_tag_handling, formatter_l)
        color_enabled :=
          switch o {
          | Some(Always) => true
          | Some(Auto) => should_enable_color()
          | Some(Never) => false
          | None => should_enable_color()
          }
      }
      ()
    }
  }
}

@raises(Invalid_argument)
let normalise_eol = s => {
  let b = Buffer.create(80)
  for i in 0 to String.length(s) - 1 {
    if String.get(s, i) != '\r' {
      Buffer.add_char(b, String.get(s, i))
    }
  }
  Buffer.contents(b)
}

@raises(Invalid_argument)
let delete_eol_spaces = src => {
  let len_src = String.length(src)
  let dst = Bytes.create(len_src)

  @raises(Invalid_argument)
  let rec loop = (i_src, i_dst) =>
    if i_src == len_src {
      i_dst
    } else {
      switch String.get(src, i_src) {
      | ' ' | '\t' => loop_spaces(1, i_src + 1, i_dst)
      | c =>
        Bytes.set(dst, i_dst, c)
        loop(i_src + 1, i_dst + 1)
      }
    }
  @raises(Invalid_argument)
  and loop_spaces = (spaces, i_src, i_dst) =>
    if i_src == len_src {
      i_dst
    } else {
      switch String.get(src, i_src) {
      | ' ' | '\t' => loop_spaces(spaces + 1, i_src + 1, i_dst)
      | '\n' =>
        Bytes.set(dst, i_dst, '\n')
        loop(i_src + 1, i_dst + 1)
      | _ =>
        for n in 0 to spaces {
          Bytes.set(dst, i_dst + n, String.get(src, i_src - spaces + n))
        }
        loop(i_src + 1, i_dst + spaces + 1)
      }
    }

  let stop = loop(0, 0)
  Bytes.sub_string(dst, 0, stop)
}

type hook_info = {sourcefile: string}

exception HookExnWrapper({error: exn, hook_name: string, hook_info: hook_info})

exception HookExn(exn)

@raises(HookExn)
let raise_direct_hook_exn = e => throw(HookExn(e))

@raises([HookExnWrapper, genericException])
let fold_hooks = (list, hook_info, ast) =>
  List.fold_left(
    (ast, (hook_name, f)) =>
      try f(hook_info, ast) catch {
      | HookExn(e) => throw(e)
      | error => throw(HookExnWrapper({error: error, hook_name: hook_name, hook_info: hook_info}))
      },
    /* when explicit reraise with backtrace will be available,
     it should be used here */
    ast,
    List.sort(compare, list),
  )

module type HookSig = {
  type t

  let add_hook: (string, (hook_info, t) => t) => unit
  let apply_hooks: (hook_info, t) => t
}

module MakeHooks = (
  M: {
    type t
  },
): (HookSig with type t = M.t) => {
  type t = M.t

  let hooks = ref(list{})
  let add_hook = (name, f) => hooks := list{(name, f), ...hooks.contents}
  @raises([HookExnWrapper, genericException])
  let apply_hooks = (sourcefile, intf) => fold_hooks(hooks.contents, sourcefile, intf)
}
