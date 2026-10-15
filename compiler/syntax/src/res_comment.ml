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
  mutable prev_tok_is_question: bool;
  (* Checkpoints restore comment lists, not this field. A speculative JSX parse
     must reproduce this marking on reparse; otherwise defer it until commit or
     restore it on rollback. *)
  mutable jsx_container: Location.t option;
}

let loc t = t.loc
let txt t = t.txt
let jsx_container t = t.jsx_container
let set_jsx_container t loc = t.jsx_container <- Some loc
let prev_tok_end_pos t = t.prev_tok_end_pos

let set_prev_tok_end_pos t pos = t.prev_tok_end_pos <- pos

let prev_tok_is_question t = t.prev_tok_is_question

let set_prev_tok_is_question t b = t.prev_tok_is_question <- b

let is_single_line_comment t = t.style = SingleLine

let to_block_if_safe t =
  let rec contains_delimiter i =
    i + 1 < String.length t.txt
    && ((t.txt.[i] = '/' && t.txt.[i + 1] = '*')
       || (t.txt.[i] = '*' && t.txt.[i + 1] = '/')
       || contains_delimiter (i + 1))
  in
  if is_single_line_comment t && not (contains_delimiter 0) then
    {t with style = MultiLine}
  else t

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
  {
    txt;
    loc;
    style = SingleLine;
    prev_tok_end_pos = Lexing.dummy_pos;
    prev_tok_is_question = false;
    jsx_container = None;
  }

let make_multi_line_comment ~loc ~doc_comment ~standalone txt =
  {
    txt;
    loc;
    style =
      (if doc_comment then if standalone then ModuleComment else DocComment
       else MultiLine);
    prev_tok_end_pos = Lexing.dummy_pos;
    prev_tok_is_question = false;
    jsx_container = None;
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
