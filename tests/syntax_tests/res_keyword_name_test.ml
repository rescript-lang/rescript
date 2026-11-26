(* Property test for reserved words as names.

   A reserved word in a label position (a record field, an argument label, a
   JSX prop), as a polymorphic variant tag or as a lowercase JSX tag name means
   the same as its escaped spelling, which every earlier version of the parser
   accepts: [r.type] is [r.\"type"], [#type] is [#"type"].

   Random programs are generated in both spellings. For each program:
   - both spellings parse without diagnostics;
   - both give the same parse tree;
   - formatting is idempotent, and parsing the formatted program gives the
     same parse tree again;
   - formatting the escaped spelling gives the same text as formatting the
     bare one;
   - formatting adds no escaped reserved word: the escape is only needed for a
     variable, and the bare spelling already writes every other one bare.
   A shorthand with a bare reserved word ([{type}], [~type], [<a type />])
   stands for a variable of that name and must be rejected.

   The generator is seeded, so a failure reproduces; the seed and the program
   are printed. *)

let reserved_words =
  [
    "and";
    "as";
    "assert";
    "await";
    "break";
    "constraint";
    "continue";
    "else";
    "exception";
    "external";
    "false";
    "for";
    "if";
    "in";
    "include";
    "let";
    "module";
    "mutable";
    "of";
    "open";
    "private";
    "rec";
    "switch";
    "true";
    "try";
    "type";
    "when";
    "while";
  ]

(* Names that are not reserved, some of which look like they could be *)
let other_names =
  ["x"; "kind"; "value"; "type_"; "async"; "list"; "dict"; "catch"; "unpack"]

let is_reserved name = List.mem name reserved_words

(* A piece of source text in both spellings *)
type spelled = {bare: string; escaped: string}

let same s = {bare = s; escaped = s}
let ( ++ ) a b = {bare = a.bare ^ b.bare; escaped = a.escaped ^ b.escaped}

let concat sep = function
  | [] -> same ""
  | first :: rest ->
    List.fold_left (fun acc x -> acc ++ same sep ++ x) first rest

let escape name = "\\\"" ^ name ^ "\""

(* A name where the grammar expects a label or a lowercase JSX tag name *)
let label name =
  if is_reserved name then {bare = name; escaped = escape name} else same name

(* A polymorphic variant tag *)
let tag name =
  if is_reserved name then {bare = name; escaped = "\"" ^ name ^ "\""}
  else same name

(* A variable: a reserved word needs its escape in both spellings *)
let variable name = same (if is_reserved name then escape name else name)

(* [data-type]: a hyphenated JSX name, one exotic name when escaped *)
let hyphenated prefix name =
  {bare = prefix ^ "-" ^ name; escaped = escape (prefix ^ "-" ^ name)}

let pick rng list = List.nth list (Random.State.int rng (List.length list))
let chance rng n = Random.State.int rng n = 0
let name rng =
  if chance rng 4 then pick rng other_names else pick rng reserved_words

let list rng ~min ~max f =
  List.init (min + Random.State.int rng (max - min + 1)) (fun _ -> f ())

let rec expr rng depth =
  let atom () =
    match Random.State.int rng 4 with
    | 0 -> same (string_of_int (Random.State.int rng 100))
    | 1 -> same "\"s\""
    | 2 -> variable (name rng)
    | _ -> same "r." ++ label (name rng)
  in
  if depth = 0 then atom ()
  else
    match Random.State.int rng 6 with
    | 0 -> record_expr rng (depth - 1)
    | 1 -> same "#" ++ tag (name rng)
    | 2 ->
      same "#" ++ tag (name rng) ++ same "(" ++ expr rng (depth - 1) ++ same ")"
    | 3 ->
      same "r."
      ++ label (name rng)
      ++ same "."
      ++ label (name rng)
      ++ same " + "
      ++ expr rng (depth - 1)
    | _ -> atom ()

and record_expr rng depth =
  let row () =
    match Random.State.int rng 4 with
    | 0 -> label (name rng) ++ same ": ?" ++ expr rng depth
    | 1 -> same "M." ++ label (name rng) ++ same ": " ++ expr rng depth
    | _ -> label (name rng) ++ same ": " ++ expr rng depth
  in
  let spread = if chance rng 3 then same "...r, " else same "" in
  let rows = list rng ~min:1 ~max:4 row in
  (* A shorthand names a variable, which keeps its escape *)
  let rows =
    if List.length rows >= 2 && chance rng 2 then
      rows @ [variable (pick rng reserved_words)]
    else rows
  in
  same "{" ++ spread ++ concat ", " rows ++ same "}"

let pattern rng =
  match Random.State.int rng 3 with
  | 0 -> same "_"
  | 1 -> same (string_of_int (Random.State.int rng 10))
  | _ -> variable (name rng)

let record_pattern rng =
  let row () =
    match Random.State.int rng 3 with
    | 0 -> label (name rng) ++ same ": ?" ++ pattern rng
    | 1 -> same "M." ++ label (name rng) ++ same ": " ++ pattern rng
    | _ -> label (name rng) ++ same ": " ++ pattern rng
  in
  same "{" ++ concat ", " (list rng ~min:1 ~max:3 row) ++ same "}"

let jsx_tag_name rng =
  if chance rng 3 then same (pick rng ["div"; "input"; "svg"])
  else label (pick rng reserved_words)

let rec jsx rng depth =
  let prop () =
    match Random.State.int rng 4 with
    | 0 -> label (name rng) ++ same "=\"s\""
    | 1 -> label (name rng) ++ same "=?" ++ variable (name rng)
    | 2 -> hyphenated (pick rng ["data"; "aria"]) (name rng) ++ same "=\"s\""
    | _ -> label (name rng) ++ same "={" ++ expr rng 1 ++ same "}"
  in
  let tag_name = jsx_tag_name rng in
  let props = concat " " (list rng ~min:0 ~max:3 prop) in
  if depth = 0 || chance rng 2 then
    same "<" ++ tag_name ++ same " " ++ props ++ same " />"
  else
    same "<" ++ tag_name ++ same " " ++ props ++ same "> "
    ++ concat " " (list rng ~min:1 ~max:2 (fun () -> jsx rng (depth - 1)))
    ++ same " </" ++ tag_name ++ same ">"

let item rng i =
  let v = same (Printf.sprintf "let v%d = " i) in
  match Random.State.int rng 12 with
  | 0 ->
    let field () =
      (if chance rng 4 then same "@as(\"z\") " else same "")
      ++ (if chance rng 3 then same "mutable " else same "")
      ++ label (name rng)
      ++ (if chance rng 3 then same "?" else same "")
      ++ same ": int"
    in
    same (Printf.sprintf "type t%d = {" i)
    ++ concat ", " (list rng ~min:1 ~max:4 field)
    ++ same "}"
  | 1 -> v ++ record_expr rng 2
  | 2 ->
    v ++ same "r."
    ++ label (name rng)
    ++ same "."
    ++ label (name rng)
    ++ same " + r.M."
    ++ label (name rng)
  | 3 -> same "r." ++ label (name rng) ++ same " = " ++ expr rng 1
  | 4 -> same "let " ++ record_pattern rng ++ same " = r"
  | 5 ->
    same "let v"
    ++ same (string_of_int i)
    ++ same " = switch r { | " ++ record_pattern rng ++ same " => 1 | #"
    ++ tag (name rng)
    ++ same " => 2 | #"
    ++ tag (name rng)
    ++ same "(_) => 3 | _ => 4 }"
  | 6 ->
    let param () =
      same "~"
      ++ label (name rng)
      ++ same " as "
      ++ variable (name rng)
      ++ pick rng [same ""; same "=?"; same "=1"]
    in
    same (Printf.sprintf "let f%d = (" i)
    ++ concat ", " (list rng ~min:1 ~max:3 param)
    ++ same ", ()) => 1"
  | 7 ->
    let arg () =
      same "~"
      ++ label (name rng)
      ++ pick rng [same "="; same "=?"]
      ++ expr rng 1
    in
    v ++ same "f(" ++ concat ", " (list rng ~min:1 ~max:3 arg) ++ same ", ())"
  | 8 ->
    let param () =
      same "~"
      ++ label (name rng)
      ++ same ": int"
      ++ if chance rng 2 then same "=?" else same ""
    in
    same (Printf.sprintf "type fn%d = (" i)
    ++ concat ", " (list rng ~min:1 ~max:3 param)
    ++ same ") => unit"
  | 9 ->
    let row () =
      same "#"
      ++ tag (name rng)
      ++ if chance rng 3 then same "(int)" else same ""
    in
    same (Printf.sprintf "type pv%d = [" i)
    ++ pick rng [same ""; same "> "]
    ++ concat " | " (list rng ~min:1 ~max:4 row)
    ++ same "]"
  | 10 ->
    (* [#let?] and [r.let?] are scanned as [let?]; here they are a tag or a
       field followed by the [?] of a ternary *)
    v ++ same "a == #"
    ++ tag (name rng)
    ++ same "? 1 : r."
    ++ label (name rng)
    ++ same "? 2 : 3"
  | _ -> v ++ jsx rng 2

let program rng =
  let items = list rng ~min:1 ~max:8 (fun () -> ()) in
  concat "\n" (List.mapi (fun i () -> item rng i) items) ++ same "\n"

let parse source =
  Res_driver.parse_implementation_from_source ~display_filename:"keyword.res"
    ~source

(* The parse tree without locations *)
let tree (result : _ Res_driver.parse_result) =
  Format.asprintf "%a" Pprintast.structure result.parsetree

let format (result : _ Res_driver.parse_result) =
  Res_printer.print_implementation result.parsetree ~comments:result.comments

let fail ~seed ~what ~source details =
  Printf.printf "❌ reserved words as names (seed %d): %s\n\nsource:\n%s\n%s\n"
    seed what source details;
  exit 1

(* The number of escaped reserved words, such as [\"type"], in [text] *)
let escapes text =
  List.fold_left
    (fun count word ->
      let needle = escape word in
      let n = String.length needle in
      let rec loop i count =
        if i + n > String.length text then count
        else if String.sub text i n = needle then loop (i + n) (count + 1)
        else loop (i + 1) count
      in
      loop 0 count)
    0 reserved_words

let check_program ~seed source =
  let bare = parse source.bare in
  let escaped = parse source.escaped in
  let check_valid spelling (result : _ Res_driver.parse_result) =
    if result.invalid then
      fail ~seed
        ~what:(spelling ^ " spelling has a syntax error")
        ~source:result.source ""
  in
  check_valid "bare" bare;
  check_valid "escaped" escaped;
  let bare_tree = tree bare in
  if bare_tree <> tree escaped then
    fail ~seed ~what:"the two spellings give different parse trees"
      ~source:source.bare
      ("bare:\n" ^ bare_tree ^ "\nescaped:\n" ^ tree escaped);
  let formatted = format bare in
  let reparsed = parse formatted in
  check_valid "formatted" reparsed;
  if tree reparsed <> bare_tree then
    fail ~seed ~what:"formatting changes the parse tree" ~source:source.bare
      ("formatted:\n" ^ formatted);
  if format reparsed <> formatted then
    fail ~seed ~what:"formatting is not idempotent" ~source:source.bare
      ("formatted:\n" ^ formatted ^ "\nformatted again:\n" ^ format reparsed);
  if escapes formatted > escapes source.bare then
    fail ~seed ~what:"formatting escapes a reserved word that needs no escape"
      ~source:source.bare
      ("formatted:\n" ^ formatted);
  if format escaped <> formatted then
    fail ~seed ~what:"the two spellings format differently" ~source:source.bare
      ("bare formatted:\n" ^ formatted ^ "\nescaped formatted:\n"
     ^ format escaped)

(* Each shorthand needs a variable named by the reserved word *)
let shorthands word =
  [
    Printf.sprintf "let v = {x: 1, %s}" word;
    Printf.sprintf "let v = {x: 1, ?%s}" word;
    Printf.sprintf "let {x, %s} = r" word;
    Printf.sprintf "let f = (~%s) => 1" word;
    Printf.sprintf "let f = (~%s: int) => 1" word;
    Printf.sprintf "let v = f(~%s)" word;
    Printf.sprintf "let v = f(~%s?)" word;
    Printf.sprintf "let v = <div %s />" word;
    Printf.sprintf "let v = <div ?%s />" word;
    Printf.sprintf "type t = {x: int, %s}" word;
  ]

(* A row that follows another without a comma *)
let missing_commas word =
  [
    Printf.sprintf "let {a %s: _} = r" word;
    Printf.sprintf "let {a: _ %s: _} = r" word;
    Printf.sprintf "let {a\n%s: _} = r" word;
    Printf.sprintf "let v = {a: 1 %s: 2}" word;
    Printf.sprintf "let v = {...r, a: 1\n%s: 2}" word;
    Printf.sprintf "type t = {a: int %s: int}" word;
  ]

let run () =
  let programs = 2000 in
  for seed = 1 to programs do
    let rng = Random.State.make [|seed|] in
    check_program ~seed (program rng)
  done;
  List.iter
    (fun word ->
      List.iter
        (fun source ->
          if not (parse source).invalid then
            fail ~seed:0 ~what:"a shorthand with a reserved word is accepted"
              ~source "")
        (shorthands word);
      List.iter
        (fun source ->
          if not (parse source).invalid then
            fail ~seed:0 ~what:"a missing comma before a field is accepted"
              ~source "")
        (missing_commas word))
    reserved_words;
  Printf.printf "✅ reserved words as names (%d random programs)\n" programs
