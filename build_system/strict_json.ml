(* Yojson also accepts comments, unquoted keys, NaN, and Infinity. Other tools
   that read rescript.json, such as the editor extension, use standard JSON
   parsers, so a configuration the build accepted would fail there. This
   validator rejects anything outside RFC 8259 before Yojson parses it. *)

type error = {line: int; column: int; message: string}

exception Invalid of int * string

let error_at contents index message =
  let line = ref 1 and column = ref 1 in
  for position = 0 to min index (String.length contents) - 1 do
    match contents.[position] with
    | '\n' ->
      incr line;
      column := 1
    (* Count characters rather than bytes: skip UTF-8 continuation bytes. *)
    | character when Char.code character land 0xC0 = 0x80 -> ()
    | _ -> incr column
  done;
  {line = !line; column = !column; message}

let check contents =
  let length = String.length contents in
  let fail index message = raise (Invalid (index, message)) in
  let peek index = if index < length then Some contents.[index] else None in
  let rec skip_whitespace index =
    match peek index with
    | Some (' ' | '\t' | '\n' | '\r') -> skip_whitespace (index + 1)
    | Some '/' -> fail index "comments are not allowed in JSON"
    | Some _ | None -> index
  in
  let expect_literal index literal =
    let literal_length = String.length literal in
    if
      index + literal_length <= length
      && String.sub contents index literal_length = literal
    then index + literal_length
    else fail index "expected a value"
  in
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let rec digits index =
    match peek index with
    | Some character when is_digit character -> digits (index + 1)
    | Some _ | None -> index
  in
  let one_or_more_digits index =
    match peek index with
    | Some character when is_digit character -> digits index
    | Some _ | None -> fail index "invalid number"
  in
  let number index =
    let index = if peek index = Some '-' then index + 1 else index in
    let index =
      match peek index with
      | Some '0' -> index + 1
      | Some '1' .. '9' -> digits index
      | Some _ | None -> fail index "invalid number"
    in
    let index =
      if peek index = Some '.' then one_or_more_digits (index + 1) else index
    in
    match peek index with
    | Some ('e' | 'E') ->
      let index = index + 1 in
      let index =
        match peek index with
        | Some ('+' | '-') -> index + 1
        | Some _ | None -> index
      in
      one_or_more_digits index
    | Some _ | None -> index
  in
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let rec string_body index =
    match peek index with
    | None -> fail index "unterminated string"
    | Some '"' -> index + 1
    | Some '\\' -> (
      match peek (index + 1) with
      | Some ('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') ->
        string_body (index + 2)
      | Some 'u' ->
        for offset = 2 to 5 do
          match peek (index + offset) with
          | Some character when is_hex character -> ()
          | Some _ | None -> fail index "invalid \\u escape"
        done;
        string_body (index + 6)
      | Some _ | None -> fail index "invalid escape in string")
    | Some character when Char.code character < 0x20 ->
      fail index "control characters must be escaped in strings"
    | Some _ -> string_body (index + 1)
  in
  let string index =
    match peek index with
    | Some '"' -> string_body (index + 1)
    | Some _ | None -> fail index "expected a string key"
  in
  let rec value index =
    let index = skip_whitespace index in
    match peek index with
    | Some '{' -> members (index + 1) ~first:true
    | Some '[' -> elements (index + 1) ~first:true
    | Some '"' -> string_body (index + 1)
    | Some ('-' | '0' .. '9') -> number index
    | Some 't' -> expect_literal index "true"
    | Some 'f' -> expect_literal index "false"
    | Some 'n' -> expect_literal index "null"
    | Some _ | None -> fail index "expected a value"
  and members index ~first =
    let index = skip_whitespace index in
    match peek index with
    | Some '}' when first -> index + 1
    | Some '}' -> fail index "trailing comma"
    | Some _ | None -> (
      let index = skip_whitespace (string index) in
      if peek index <> Some ':' then fail index "expected ':'";
      let index = skip_whitespace (value (index + 1)) in
      match peek index with
      | Some ',' -> members (index + 1) ~first:false
      | Some '}' -> index + 1
      | Some _ | None -> fail index "expected ',' or '}'")
  and elements index ~first =
    let index = skip_whitespace index in
    match peek index with
    | Some ']' when first -> index + 1
    | Some ']' -> fail index "trailing comma"
    | Some _ | None -> (
      let index = skip_whitespace (value index) in
      match peek index with
      | Some ',' -> elements (index + 1) ~first:false
      | Some ']' -> index + 1
      | Some _ | None -> fail index "expected ',' or ']'")
  in
  try
    if not (String.is_valid_utf_8 contents) then fail 0 "invalid UTF-8";
    if String.starts_with ~prefix:"\xEF\xBB\xBF" contents then
      fail 0 "byte order mark is not allowed";
    let index = skip_whitespace (value 0) in
    if index < length then fail index "unexpected content after the value";
    Ok ()
  with Invalid (index, message) -> Error (error_at contents index message)
