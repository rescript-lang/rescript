type style = SingleLine | MultiLine | DocComment | ModuleComment

let style_to_string s =
  match s with
  | SingleLine -> "SingleLine"
  | MultiLine -> "MultiLine"
  | DocComment -> "DocComment"
  | ModuleComment -> "ModuleComment"

type t = {
  txt: string;
  style: style;
  loc: Location.t;
  mutable prev_tok_end_pos: Lexing.position;
}

let loc t = t.loc
let txt t = t.txt
let prev_tok_end_pos t = t.prev_tok_end_pos

let set_prev_tok_end_pos t pos = t.prev_tok_end_pos <- pos

let is_single_line_comment t = t.style = SingleLine

let is_doc_comment t = t.style = DocComment

let is_module_comment t = t.style = ModuleComment

let to_string t =
  let {Location.loc_start; loc_end} = t.loc in
  Format.sprintf "(txt: %s\nstyle: %s\nlocation: %d,%d-%d,%d)" t.txt
    (style_to_string t.style) loc_start.pos_lnum
    (loc_start.pos_cnum - loc_start.pos_bol)
    loc_end.pos_lnum
    (loc_end.pos_cnum - loc_end.pos_bol)

let make_single_line_comment ~loc txt =
  {txt; loc; style = SingleLine; prev_tok_end_pos = Lexing.dummy_pos}

let make_multi_line_comment ~loc ~doc_comment ~standalone txt =
  {
    txt;
    loc;
    style =
      (if doc_comment then if standalone then ModuleComment else DocComment
       else MultiLine);
    prev_tok_end_pos = Lexing.dummy_pos;
  }

let trim_spaces s =
  let len = String.length s in
  if len = 0 then s
  else if String.unsafe_get s 0 = ' ' || String.unsafe_get s (len - 1) = ' '
  then (
    let i = ref 0 in
    while !i < len && String.unsafe_get s !i = ' ' do
      incr i
    done;
    let j = ref (len - 1) in
    while !j >= !i && String.unsafe_get s !j = ' ' do
      decr j
    done;
    if !j >= !i then (String.sub [@doesNotRaise]) s !i (!j - !i + 1) else "")
  else s
