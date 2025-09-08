module MiniBuffer = Res_minibuffer

type mode = Break | Flat

type line_style =
  | Classic (* fits? -> replace with space *)
  | Soft (* fits? -> replaced with nothing *)
  | Hard
  (* always included, forces breaks in parents *)
  (* always included, forces breaks in parents, but doesn't increase indentation
     use case: template literals, multiline string content *)
  | Literal

type t =
  | Nil
  | Text of string
  | Concat of t list
  | Indent of t
  | IfBreaks of {yes: t; no: t; mutable broken: bool}
    (* when broken is true, treat as the yes branch *)
  | LineSuffix of t
  | LineBreak of line_style
  | Group of {mutable should_break: bool; doc: t}
  | CustomLayout of t list
  | BreakParent

let nil = Nil
let line = LineBreak Classic
let hard_line = LineBreak Hard
let soft_line = LineBreak Soft
let literal_line = LineBreak Literal
let text s = Text s

(* Optimization. We eagerly collapse and reduce whatever allocation we can *)
let rec _concat acc l =
  match l with
  | Text s1 :: Text s2 :: rest -> Text (s1 ^ s2) :: _concat acc rest
  | Nil :: rest -> _concat acc rest
  | Concat l2 :: rest ->
    _concat (_concat acc rest) l2 (* notice the order here *)
  | x :: rest ->
    let rest1 = _concat acc rest in
    if rest1 == rest then l else x :: rest1
  | [] -> acc

let concat l = Concat (_concat [] l)

let indent d = Indent d
let if_breaks t f = IfBreaks {yes = t; no = f; broken = false}
let line_suffix d = LineSuffix d
let group d = Group {should_break = false; doc = d}
let breakable_group ~force_break d = Group {should_break = force_break; doc = d}
let custom_layout gs = CustomLayout gs
let break_parent = BreakParent

let space = Text " "
let comma = Text ","
let dot = Text "."
let dotdot = Text ".."
let dotdotdot = Text "..."
let less_than = Text "<"
let greater_than = Text ">"
let lbrace = Text "{"
let rbrace = Text "}"
let lparen = Text "("
let rparen = Text ")"
let lbracket = Text "["
let rbracket = Text "]"
let question = Text "?"
let tilde = Text "~"
let equal = Text "="
let trailing_comma = if_breaks comma nil
let double_quote = Text "\""

let propagate_forced_breaks doc =
  let rec walk doc =
    match doc with
    | Text _ | Nil | LineSuffix _ -> false
    | BreakParent -> true
    | LineBreak (Hard | Literal) -> true
    | LineBreak (Classic | Soft) -> false
    | Indent children ->
      let child_forces_break = walk children in
      child_forces_break
    | IfBreaks ({yes = true_doc; no = false_doc} as ib) ->
      let false_force_break = walk false_doc in
      if false_force_break then (
        let _ = walk true_doc in
        ib.broken <- true;
        true)
      else
        let force_break = walk true_doc in
        force_break
    | Group ({should_break = force_break; doc = children} as gr) ->
      let child_forces_break = walk children in
      let should_break = force_break || child_forces_break in
      gr.should_break <- should_break;
      should_break
    | Concat children ->
      List.fold_left
        (fun force_break child ->
          let child_forces_break = walk child in
          force_break || child_forces_break)
        false children
    | CustomLayout children ->
      (* When using CustomLayout, we don't want to propagate forced breaks
       * from the children up. By definition it picks the first layout that fits
       * otherwise it takes the last of the list.
       * However we do want to propagate forced breaks in the sublayouts. They
       * might need to be broken. We just don't propagate them any higher here *)
      let _ = walk (Concat children) in
      false
  in
  let _ = walk doc in
  ()

(* See documentation in interface file *)
let rec will_break doc =
  match doc with
  | LineBreak (Hard | Literal) | BreakParent | Group {should_break = true} ->
    true
  | Group {doc} | Indent doc | CustomLayout (doc :: _) -> will_break doc
  | Concat docs -> List.exists will_break docs
  | IfBreaks {yes; no} -> will_break yes || will_break no
  | _ -> false

let join ~sep docs =
  let rec loop acc sep docs =
    match docs with
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: xs -> loop (sep :: x :: acc) sep xs
  in
  concat (loop [] sep docs)

let join_with_sep docs_with_sep =
  let rec loop acc docs =
    match docs with
    | [] -> List.rev acc
    | [(x, _sep)] -> List.rev (x :: acc)
    | (x, sep) :: xs -> loop (sep :: x :: acc) xs
  in
  concat (loop [] docs_with_sep)

let fits w stack =
  let width = ref w in
  let result = ref None in

  let rec calculate indent mode doc =
    match (mode, doc) with
    | _ when result.contents != None -> ()
    | _ when width.contents < 0 -> result := Some false
    | _, Nil | _, LineSuffix _ | _, BreakParent -> ()
    | _, Text txt -> width := width.contents - String.length txt
    | _, Indent doc -> calculate (indent + 2) mode doc
    | Flat, LineBreak Hard | Flat, LineBreak Literal -> result := Some true
    | Flat, LineBreak Classic -> width := width.contents - 1
    | Flat, LineBreak Soft -> ()
    | Break, LineBreak _ -> result := Some true
    | _, Group {should_break = true; doc} -> calculate indent Break doc
    | _, Group {doc} -> calculate indent mode doc
    | _, IfBreaks {yes = break_doc; broken = true} ->
      calculate indent mode break_doc
    | Break, IfBreaks {yes = break_doc} -> calculate indent mode break_doc
    | Flat, IfBreaks {no = flat_doc} -> calculate indent mode flat_doc
    | _, Concat docs -> calculate_concat indent mode docs
    | _, CustomLayout (hd :: _) ->
      (* TODO: if we have nested custom layouts, what we should do here? *)
      calculate indent mode hd
    | _, CustomLayout [] -> ()
  and calculate_concat indent mode docs =
    if result.contents == None then
      match docs with
      | [] -> ()
      | doc :: rest ->
        calculate indent mode doc;
        calculate_concat indent mode rest
  in
  let rec calculate_all stack =
    match (result.contents, stack) with
    | Some r, _ -> r
    | None, [] -> !width >= 0
    | None, (indent, mode, doc) :: rest ->
      calculate indent mode doc;
      calculate_all rest
  in
  calculate_all stack

let to_string ~width doc =
  propagate_forced_breaks doc;
  let buffer = MiniBuffer.create 1000 in

  let rec process ~pos line_suffices stack =
    match stack with
    | ((ind, mode, doc) as cmd) :: rest -> (
      match doc with
      | Nil | BreakParent -> process ~pos line_suffices rest
      | Text txt ->
        MiniBuffer.add_string buffer txt;
        process ~pos:(String.length txt + pos) line_suffices rest
      | LineSuffix doc -> process ~pos ((ind, mode, doc) :: line_suffices) rest
      | Concat docs ->
        let ops = List.map (fun doc -> (ind, mode, doc)) docs in
        process ~pos line_suffices (List.append ops rest)
      | Indent doc -> process ~pos line_suffices ((ind + 2, mode, doc) :: rest)
      | IfBreaks {yes = break_doc; broken = true} ->
        process ~pos line_suffices ((ind, mode, break_doc) :: rest)
      | IfBreaks {yes = break_doc; no = flat_doc} ->
        if mode = Break then
          process ~pos line_suffices ((ind, mode, break_doc) :: rest)
        else process ~pos line_suffices ((ind, mode, flat_doc) :: rest)
      | LineBreak line_style ->
        if mode = Break then
          match line_suffices with
          | [] ->
            if line_style = Literal then (
              MiniBuffer.add_char buffer '\n';
              process ~pos:0 [] rest)
            else (
              MiniBuffer.flush_newline buffer;
              MiniBuffer.add_string buffer (String.make ind ' ' [@doesNotRaise]);
              process ~pos:ind [] rest)
          | _docs ->
            process ~pos:ind []
              (List.concat [List.rev line_suffices; cmd :: rest])
        else
          (* mode = Flat *)
          let pos =
            match line_style with
            | Classic ->
              MiniBuffer.add_string buffer " ";
              pos + 1
            | Hard ->
              MiniBuffer.flush_newline buffer;
              0
            | Literal ->
              MiniBuffer.add_char buffer '\n';
              0
            | Soft -> pos
          in
          process ~pos line_suffices rest
      | Group {should_break; doc} ->
        if should_break || not (fits (width - pos) ((ind, Flat, doc) :: rest))
        then process ~pos line_suffices ((ind, Break, doc) :: rest)
        else process ~pos line_suffices ((ind, Flat, doc) :: rest)
      | CustomLayout docs ->
        let rec find_group_that_fits groups =
          match groups with
          | [] -> Nil
          | [last_group] -> last_group
          | doc :: docs ->
            if fits (width - pos) ((ind, Flat, doc) :: rest) then doc
            else find_group_that_fits docs
        in
        let doc = find_group_that_fits docs in
        process ~pos line_suffices ((ind, Flat, doc) :: rest))
    | [] -> (
      match line_suffices with
      | [] -> ()
      | suffices -> process ~pos:0 [] (List.rev suffices))
  in
  process ~pos:0 [] [(0, Flat, doc)];
  MiniBuffer.contents buffer

let debug t =
  let rec to_doc = function
    | Nil -> text "nil"
    | BreakParent -> text "breakparent"
    | Text txt -> text ("text(\"" ^ txt ^ "\")")
    | LineSuffix doc ->
      group
        (concat
           [
             text "linesuffix(";
             indent (concat [line; to_doc doc]);
             line;
             text ")";
           ])
    | Concat [] -> text "concat()"
    | Concat docs ->
      group
        (concat
           [
             text "concat(";
             indent
               (concat
                  [
                    line;
                    join ~sep:(concat [text ","; line]) (List.map to_doc docs);
                  ]);
             line;
             text ")";
           ])
    | CustomLayout docs ->
      group
        (concat
           [
             text "customLayout(";
             indent
               (concat
                  [
                    line;
                    join ~sep:(concat [text ","; line]) (List.map to_doc docs);
                  ]);
             line;
             text ")";
           ])
    | Indent doc ->
      concat [text "indent("; soft_line; to_doc doc; soft_line; text ")"]
    | IfBreaks {yes = true_doc; broken = true} -> to_doc true_doc
    | IfBreaks {yes = true_doc; no = false_doc} ->
      group
        (concat
           [
             text "ifBreaks(";
             indent
               (concat
                  [
                    line;
                    to_doc true_doc;
                    concat [text ","; line];
                    to_doc false_doc;
                  ]);
             line;
             text ")";
           ])
    | LineBreak break ->
      let break_txt =
        match break with
        | Classic -> "Classic"
        | Soft -> "Soft"
        | Hard -> "Hard"
        | Literal -> "Liteal"
      in
      text ("LineBreak(" ^ break_txt ^ ")")
    | Group {should_break; doc} ->
      group
        (concat
           [
             text "Group(";
             indent
               (concat
                  [
                    line;
                    text ("{shouldBreak: " ^ string_of_bool should_break ^ "}");
                    concat [text ","; line];
                    to_doc doc;
                  ]);
             line;
             text ")";
           ])
  in
  let doc = to_doc t in
  to_string ~width:10 doc |> print_endline
[@@live]
