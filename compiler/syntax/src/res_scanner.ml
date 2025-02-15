module Diagnostics = Res_diagnostics
module Token = Res_token
module Comment = Res_comment

type mode = Jsx | Diamond

(* We hide the implementation detail of the scanner reading character. Our char
   will also contain the special -1 value to indicate end-of-file. This isn't
   ideal; we should clean this up *)
let hacky_eof_char = Char.unsafe_chr (-1)
type char_encoding = Char.t

type t = {
  filename: string;
  src: string;
  mutable err:
    start_pos:Lexing.position ->
    end_pos:Lexing.position ->
    Diagnostics.category ->
    unit;
  mutable ch: char_encoding; (* current character *)
  mutable offset: int; (* current byte offset *)
  mutable offset16: int;
      (* current number of utf16 code units since line start *)
  mutable line_offset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
  mutable mode: mode list;
}

let set_diamond_mode scanner = scanner.mode <- Diamond :: scanner.mode

let set_jsx_mode scanner = scanner.mode <- Jsx :: scanner.mode

let pop_mode scanner mode =
  match scanner.mode with
  | m :: ms when m = mode -> scanner.mode <- ms
  | _ -> ()

let in_diamond_mode scanner =
  match scanner.mode with
  | Diamond :: _ -> true
  | _ -> false

let in_jsx_mode scanner =
  match scanner.mode with
  | Jsx :: _ -> true
  | _ -> false

let position scanner =
  Lexing.
    {
      pos_fname = scanner.filename;
      (* line number *)
      pos_lnum = scanner.lnum;
      (* offset of the beginning of the line (number
         of bytes between the beginning of the scanner and the beginning
         of the line) *)
      pos_bol = scanner.line_offset;
      (* [pos_cnum - pos_bol]  is the number of utf16 code units since line start *)
      pos_cnum = scanner.line_offset + scanner.offset16;
    }

(* Small debugging util
   ❯ echo 'let msg = "hello"' | ./lib/rescript.exe
   let msg = "hello"
   ^-^ let 0-3
   let msg = "hello"
       ^-^ msg 4-7
   let msg = "hello"
           ^ = 8-9
   let msg = "hello"
             ^-----^ string "hello" 10-17
   let msg = "hello"
                     ^ eof 18-18
   let msg = "hello"
*)
let _printDebug ~start_pos ~end_pos scanner token =
  let open Lexing in
  print_string scanner.src;
  print_string ((String.make [@doesNotRaise]) start_pos.pos_cnum ' ');
  print_char '^';
  (match end_pos.pos_cnum - start_pos.pos_cnum with
  | 0 -> if token = Token.Eof then () else assert false
  | 1 -> ()
  | n ->
    print_string ((String.make [@doesNotRaise]) (n - 2) '-');
    print_char '^');
  print_char ' ';
  print_string (Res_token.to_string token);
  print_char ' ';
  print_int start_pos.pos_cnum;
  print_char '-';
  print_int end_pos.pos_cnum;
  print_endline ""
[@@live]

let next scanner =
  let next_offset = scanner.offset + 1 in
  let utf16len =
    match Ext_utf8.classify scanner.ch with
    | Single _ | Invalid -> 1
    | Leading (n, _) -> ( (((n + 1) / 2) [@doesNotRaise]))
    | Cont _ -> 0
  in
  let newline =
    scanner.ch = '\n'
    (* What about CRLF (\r + \n) on windows?
       \r\n will always be terminated by a \n
       -> we can just bump the line count on \n *)
  in
  if newline then (
    scanner.line_offset <- next_offset;
    scanner.offset16 <- 0;
    scanner.lnum <- scanner.lnum + 1)
  else scanner.offset16 <- scanner.offset16 + utf16len;
  if next_offset < String.length scanner.src then (
    scanner.offset <- next_offset;
    scanner.ch <- String.unsafe_get scanner.src next_offset)
  else (
    scanner.offset <- String.length scanner.src;
    scanner.offset16 <- scanner.offset - scanner.line_offset;
    scanner.ch <- hacky_eof_char)

let next2 scanner =
  next scanner;
  next scanner

let next3 scanner =
  next scanner;
  next scanner;
  next scanner

let peek scanner =
  if scanner.offset + 1 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 1)
  else hacky_eof_char

let peek2 scanner =
  if scanner.offset + 2 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 2)
  else hacky_eof_char

let peek3 scanner =
  if scanner.offset + 3 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 3)
  else hacky_eof_char

let make ~filename src =
  {
    filename;
    src;
    err = (fun ~start_pos:_ ~end_pos:_ _ -> ());
    ch = (if src = "" then hacky_eof_char else String.unsafe_get src 0);
    offset = 0;
    offset16 = 0;
    line_offset = 0;
    lnum = 1;
    mode = [];
  }

(* generic helpers *)

let is_whitespace ch =
  match ch with
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let rec skip_whitespace scanner =
  if is_whitespace scanner.ch then (
    next scanner;
    skip_whitespace scanner)

let digit_value ch =
  match ch with
  | '0' .. '9' -> Char.code ch - 48
  | 'a' .. 'f' -> Char.code ch - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code ch + 32 - Char.code 'a' + 10
  | _ -> 16 (* larger than any legal value *)

(* scanning helpers *)

let scan_identifier scanner =
  let start_off = scanner.offset in
  let rec skip_good_chars scanner =
    match (scanner.ch, in_jsx_mode scanner) with
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\''), false ->
      next scanner;
      skip_good_chars scanner
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' | '-'), true ->
      next scanner;
      skip_good_chars scanner
    | _ -> ()
  in
  skip_good_chars scanner;
  let str =
    (String.sub [@doesNotRaise]) scanner.src start_off
      (scanner.offset - start_off)
  in
  match (scanner, str) with
  | {ch = '{'}, "list" ->
    next scanner;
    (* TODO: this isn't great *)
    Token.lookup_keyword "list{"
  | {ch = '{'}, "dict" ->
    next scanner;
    (* TODO: this isn't great *)
    Token.lookup_keyword "dict{"
  | _ -> Token.lookup_keyword str

let scan_digits scanner ~base =
  if base <= 10 then
    let rec loop scanner found_digits =
      match scanner.ch with
      | '0' .. '9' ->
        next scanner;
        loop scanner true
      | '_' ->
        next scanner;
        loop scanner false
      | _ -> found_digits
    in
    loop scanner false
  else
    let rec loop scanner found_digits =
      match scanner.ch with
      (* hex *)
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' ->
        next scanner;
        loop scanner true
      | '_' ->
        next scanner;
        loop scanner false
      | _ -> found_digits
    in
    loop scanner false

(* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
let scan_number scanner =
  let start_off = scanner.offset in

  (* integer part *)
  let base =
    match scanner.ch with
    | '0' -> (
      match peek scanner with
      | 'x' | 'X' ->
        next2 scanner;
        16
      | 'o' | 'O' ->
        next2 scanner;
        8
      | 'b' | 'B' ->
        next2 scanner;
        2
      | _ ->
        next scanner;
        8)
    | _ -> 10
  in
  let _ : bool = scan_digits scanner ~base in

  (* *)
  let is_float =
    if '.' == scanner.ch then (
      next scanner;
      let _ : bool = scan_digits scanner ~base in
      true)
    else false
  in

  (* exponent part *)
  let is_float =
    let start_pos = position scanner in
    match scanner.ch with
    | 'e' | 'E' | 'p' | 'P' ->
      (match peek scanner with
      | '+' | '-' -> next2 scanner
      | _ -> next scanner);
      let end_pos = position scanner in
      let found_digits = scan_digits scanner ~base in
      if not found_digits then
        scanner.err ~start_pos ~end_pos
          (Diagnostics.message "Expected digits after exponential notation.");
      true
    | _ -> is_float
  in
  let literal =
    (String.sub [@doesNotRaise]) scanner.src start_off
      (scanner.offset - start_off)
  in

  (* suffix *)
  let suffix =
    match scanner.ch with
    | ('g' .. 'z' | 'G' .. 'Z') as ch ->
      next scanner;
      Some ch
    | _ -> None
  in
  if is_float then Token.Float {f = literal; suffix}
  else Token.Int {i = literal; suffix}

let scan_exotic_identifier scanner =
  let start_pos = position scanner in
  let start_off = scanner.offset in

  next2 scanner;

  let rec scan () =
    match scanner.ch with
    | '"' -> next scanner
    | '\n' | '\r' ->
      (* line break *)
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos
        (Diagnostics.message "A quoted identifier can't contain line breaks.");
      next scanner
    | ch when ch == hacky_eof_char ->
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos
        (Diagnostics.message "Did you forget a \" here?")
    | _ ->
      next scanner;
      scan ()
  in
  scan ();

  let ident =
    (String.sub [@doesNotRaise]) scanner.src start_off
      (scanner.offset - start_off)
  in
  let name = Ext_ident.unwrap_uppercase_exotic ident in
  if name = String.empty then (
    let end_pos = position scanner in
    scanner.err ~start_pos ~end_pos
      (Diagnostics.message "A quoted identifier can't be empty string.");
    Token.Lident ident)
  else if Ext_ident.is_uident name then Token.Lident ident
    (* Exotic ident with uppercase letter should be encoded to avoid confusing in OCaml parsetree *)
  else Token.Lident name

let scan_string_escape_sequence ~start_pos scanner =
  let scan ~n ~base ~max =
    let rec loop n x =
      if n == 0 then x
      else
        let d = digit_value scanner.ch in
        if d >= base then (
          let pos = position scanner in
          let msg =
            if scanner.ch == hacky_eof_char then "unclosed escape sequence"
            else "unknown escape sequence"
          in
          scanner.err ~start_pos ~end_pos:pos (Diagnostics.message msg);
          -1)
        else
          let () = next scanner in
          loop (n - 1) ((x * base) + d)
    in
    let x = loop n 0 in
    if x > max || (0xD800 <= x && x < 0xE000) then
      let pos = position scanner in
      let msg = "escape sequence is invalid unicode code point" in
      scanner.err ~start_pos ~end_pos:pos (Diagnostics.message msg)
  in
  match scanner.ch with
  (* \ already consumed *)
  | 'n' | 't' | 'b' | 'r' | '\\' | ' ' | '\'' | '"' -> next scanner
  | '0'
    when let c = peek scanner in
         c < '0' || c > '9' ->
    (* Allow \0 *)
    next scanner
  | '0' .. '9' -> scan ~n:3 ~base:10 ~max:255
  | 'x' ->
    (* hex *)
    next scanner;
    scan ~n:2 ~base:16 ~max:255
  | 'u' -> (
    next scanner;
    match scanner.ch with
    | '{' -> (
      (* unicode code point escape sequence: '\u{7A}', one or more hex digits *)
      next scanner;
      let x = ref 0 in
      while
        match scanner.ch with
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false
      do
        x := (!x * 16) + digit_value scanner.ch;
        next scanner
      done;
      (* consume '}' in '\u{7A}' *)
      match scanner.ch with
      | '}' -> next scanner
      | _ -> ())
    | _ -> scan ~n:4 ~base:16 ~max:Res_utf8.max)
  | _ ->
    (* unknown escape sequence
     * TODO: we should warn the user here. Let's not make it a hard error for now, for reason compat *)
    (*
      let pos = position scanner in
      let msg =
        if ch == -1 then "unclosed escape sequence"
        else "unknown escape sequence"
      in
      scanner.err ~startPos ~endPos:pos (Diagnostics.message msg)
     *)
    ()

let scan_string scanner =
  (* assumption: we've just matched a quote *)
  let start_pos_with_quote = position scanner in
  next scanner;

  (* If the text needs changing, a buffer is used *)
  let buf = Buffer.create 0 in
  let first_char_offset = scanner.offset in
  let last_offset_in_buf = ref first_char_offset in

  let bring_buf_up_to_date ~start_offset =
    let str_up_to_now =
      (String.sub scanner.src !last_offset_in_buf
         (start_offset - !last_offset_in_buf) [@doesNotRaise])
    in
    Buffer.add_string buf str_up_to_now;
    last_offset_in_buf := start_offset
  in

  let result ~first_char_offset ~last_char_offset =
    if Buffer.length buf = 0 then
      (String.sub [@doesNotRaise]) scanner.src first_char_offset
        (last_char_offset - first_char_offset)
    else (
      bring_buf_up_to_date ~start_offset:last_char_offset;
      Buffer.contents buf)
  in

  let rec scan () =
    match scanner.ch with
    | '"' ->
      let last_char_offset = scanner.offset in
      next scanner;
      result ~first_char_offset ~last_char_offset
    | '\\' ->
      let start_pos = position scanner in
      let start_offset = scanner.offset + 1 in
      next scanner;
      scan_string_escape_sequence ~start_pos scanner;
      let end_offset = scanner.offset in
      convert_octal_to_hex ~start_offset ~end_offset
    | ch when ch == hacky_eof_char ->
      let end_pos = position scanner in
      scanner.err ~start_pos:start_pos_with_quote ~end_pos
        Diagnostics.unclosed_string;
      let last_char_offset = scanner.offset in
      result ~first_char_offset ~last_char_offset
    | _ ->
      next scanner;
      scan ()
  and convert_octal_to_hex ~start_offset ~end_offset =
    let len = end_offset - start_offset in
    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    in
    let txt = scanner.src in
    let is_numeric_escape =
      len = 3
      && (is_digit txt.[start_offset] [@doesNotRaise])
      && (is_digit txt.[start_offset + 1] [@doesNotRaise])
      && (is_digit txt.[start_offset + 2] [@doesNotRaise])
    in
    if is_numeric_escape then (
      let str_decimal = (String.sub txt start_offset 3 [@doesNotRaise]) in
      bring_buf_up_to_date ~start_offset;
      let str_hex = Res_string.convert_decimal_to_hex ~str_decimal in
      last_offset_in_buf := start_offset + 3;
      Buffer.add_string buf str_hex;
      scan ())
    else scan ()
  in
  Token.String (scan ())

let scan_escape scanner =
  (* '\' consumed *)
  let offset = scanner.offset - 1 in
  let convert_number scanner ~n ~base =
    let x = ref 0 in
    for _ = n downto 1 do
      let d = digit_value scanner.ch in
      x := (!x * base) + d;
      next scanner
    done;
    let c = !x in
    if Res_utf8.is_valid_code_point c then c else Res_utf8.repl
  in
  let codepoint =
    match scanner.ch with
    | '0' .. '9' -> convert_number scanner ~n:3 ~base:10
    | 'b' ->
      next scanner;
      8
    | 'n' ->
      next scanner;
      10
    | 'r' ->
      next scanner;
      13
    | 't' ->
      next scanner;
      009
    | 'x' ->
      next scanner;
      convert_number scanner ~n:2 ~base:16
    | 'o' ->
      next scanner;
      convert_number scanner ~n:3 ~base:8
    | 'u' -> (
      next scanner;
      match scanner.ch with
      | '{' ->
        (* unicode code point escape sequence: '\u{7A}', one or more hex digits *)
        next scanner;
        let x = ref 0 in
        while
          match scanner.ch with
          | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
          | _ -> false
        do
          x := (!x * 16) + digit_value scanner.ch;
          next scanner
        done;
        (* consume '}' in '\u{7A}' *)
        (match scanner.ch with
        | '}' -> next scanner
        | _ -> ());
        let c = !x in
        if Res_utf8.is_valid_code_point c then c else Res_utf8.repl
      | _ ->
        (* unicode escape sequence: '\u007A', exactly 4 hex digits *)
        convert_number scanner ~n:4 ~base:16)
    | ch ->
      next scanner;
      Char.code ch
  in
  let contents =
    (String.sub [@doesNotRaise]) scanner.src offset (scanner.offset - offset)
  in
  next scanner;
  (* Consume \' *)
  (* TODO: do we know it's \' ? *)
  Token.Codepoint {c = codepoint; original = contents}

let scan_regex scanner =
  let start_pos = position scanner in
  let buf = Buffer.create 0 in
  let first_char_offset = scanner.offset in
  let last_offset_in_buf = ref first_char_offset in

  let bring_buf_up_to_date ~start_offset =
    let str_up_to_now =
      (String.sub scanner.src !last_offset_in_buf
         (start_offset - !last_offset_in_buf) [@doesNotRaise])
    in
    Buffer.add_string buf str_up_to_now;
    last_offset_in_buf := start_offset
  in

  let result ~first_char_offset ~last_char_offset =
    if Buffer.length buf = 0 then
      (String.sub [@doesNotRaise]) scanner.src first_char_offset
        (last_char_offset - first_char_offset)
    else (
      bring_buf_up_to_date ~start_offset:last_char_offset;
      Buffer.contents buf)
  in
  let rec scan () =
    match scanner.ch with
    | '/' ->
      let last_char_offset = scanner.offset in
      next scanner;
      let pattern = result ~first_char_offset ~last_char_offset in
      let flags =
        let flags_buf = Buffer.create 0 in
        let rec scan_flags () =
          match scanner.ch with
          | 'd' | 'g' | 'i' | 'm' | 's' | 'u' | 'v' | 'y' ->
            Buffer.add_char flags_buf scanner.ch;
            next scanner;
            scan_flags ()
          | _ -> Buffer.contents flags_buf
        in
        scan_flags ()
      in
      (pattern, flags)
    | ch when ch == '\n' || ch == hacky_eof_char ->
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos (Diagnostics.message "unterminated regex");
      ("", "")
    | '\\' ->
      next scanner;
      next scanner;
      scan ()
    | _ ->
      next scanner;
      scan ()
  in
  let pattern, flags = scan () in
  let end_pos = position scanner in
  (start_pos, end_pos, Token.Regex (pattern, flags))

let scan_single_line_comment scanner =
  let start_off = scanner.offset in
  let start_pos = position scanner in
  let rec skip scanner =
    match scanner.ch with
    | '\n' | '\r' -> ()
    | ch when ch == hacky_eof_char -> ()
    | _ ->
      next scanner;
      skip scanner
  in
  skip scanner;
  let end_pos = position scanner in
  Token.Comment
    (Comment.make_single_line_comment
       ~loc:
         Location.{loc_start = start_pos; loc_end = end_pos; loc_ghost = false}
       ((String.sub [@doesNotRaise]) scanner.src start_off
          (scanner.offset - start_off)))

let scan_multi_line_comment scanner =
  (* assumption: we're only ever using this helper in `scan` after detecting a comment *)
  let doc_comment =
    peek2 scanner = '*' && peek3 scanner <> '/'
    (* no /**/ *)
  in
  let standalone =
    doc_comment && peek3 scanner = '*'
    (* /*** *)
  in
  let content_start_off =
    scanner.offset + if doc_comment then if standalone then 4 else 3 else 2
  in
  let start_pos = position scanner in
  let rec scan ~depth =
    (* invariant: depth > 0 right after this match. See assumption *)
    match (scanner.ch, peek scanner) with
    | '/', '*' ->
      next2 scanner;
      scan ~depth:(depth + 1)
    | '*', '/' ->
      next2 scanner;
      if depth > 1 then scan ~depth:(depth - 1)
    | ch, _ when ch == hacky_eof_char ->
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos Diagnostics.unclosed_comment
    | _ ->
      next scanner;
      scan ~depth
  in
  scan ~depth:0;
  let length = scanner.offset - 2 - content_start_off in
  let length = if length < 0 (* in case of EOF *) then 0 else length in
  Token.Comment
    (Comment.make_multi_line_comment ~doc_comment ~standalone
       ~loc:
         Location.
           {
             loc_start = start_pos;
             loc_end = position scanner;
             loc_ghost = false;
           }
       ((String.sub [@doesNotRaise]) scanner.src content_start_off length))

let scan_template_literal_token scanner =
  let start_off = scanner.offset in

  (* if starting } here, consume it *)
  if scanner.ch == '}' then next scanner;

  let start_pos = position scanner in

  let rec scan () =
    let last_pos = position scanner in
    match scanner.ch with
    | '`' ->
      next scanner;
      let contents =
        (String.sub [@doesNotRaise]) scanner.src start_off
          (scanner.offset - 1 - start_off)
      in
      Token.TemplateTail (contents, last_pos)
    | '$' -> (
      match peek scanner with
      | '{' ->
        next2 scanner;
        let contents =
          (String.sub [@doesNotRaise]) scanner.src start_off
            (scanner.offset - 2 - start_off)
        in
        Token.TemplatePart (contents, last_pos)
      | _ ->
        next scanner;
        scan ())
    | '\\' -> (
      match peek scanner with
      | '`' | '\\' | '$' | '\n' | '\r' ->
        (* line break *)
        next2 scanner;
        scan ()
      | _ ->
        next scanner;
        scan ())
    | ch when ch = hacky_eof_char ->
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos Diagnostics.unclosed_template;
      let contents =
        (String.sub [@doesNotRaise]) scanner.src start_off
          (max (scanner.offset - 1 - start_off) 0)
      in
      Token.TemplateTail (contents, last_pos)
    | _ ->
      next scanner;
      scan ()
  in
  let token = scan () in
  let end_pos = position scanner in
  (start_pos, end_pos, token)

let rec scan scanner =
  skip_whitespace scanner;
  let start_pos = position scanner in

  let token =
    match scanner.ch with
    (* peeking 0 char *)
    | 'A' .. 'Z' | 'a' .. 'z' -> scan_identifier scanner
    | '0' .. '9' -> scan_number scanner
    | '`' ->
      next scanner;
      Token.Backtick
    | '~' ->
      next scanner;
      Token.Tilde
    | '?' ->
      next scanner;
      Token.Question
    | ';' ->
      next scanner;
      Token.Semicolon
    | '(' ->
      next scanner;
      Token.Lparen
    | ')' ->
      next scanner;
      Token.Rparen
    | '[' ->
      next scanner;
      Token.Lbracket
    | ']' ->
      next scanner;
      Token.Rbracket
    | '{' ->
      next scanner;
      Token.Lbrace
    | '}' ->
      next scanner;
      Token.Rbrace
    | ',' ->
      next scanner;
      Token.Comma
    | '"' -> scan_string scanner
    (* peeking 1 char *)
    | '_' -> (
      match peek scanner with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> scan_identifier scanner
      | _ ->
        next scanner;
        Token.Underscore)
    | '#' -> (
      match peek scanner with
      | '=' ->
        next2 scanner;
        Token.HashEqual
      | _ ->
        next scanner;
        Token.Hash)
    | '*' -> (
      match peek scanner with
      | '*' ->
        next2 scanner;
        Token.Exponentiation
      | '.' ->
        next2 scanner;
        Token.AsteriskDot
      | _ ->
        next scanner;
        Token.Asterisk)
    | '@' -> (
      match peek scanner with
      | '@' ->
        next2 scanner;
        Token.AtAt
      | _ ->
        next scanner;
        Token.At)
    | '%' -> (
      match peek scanner with
      | '%' ->
        next2 scanner;
        Token.PercentPercent
      | _ ->
        next scanner;
        Token.Percent)
    | '|' -> (
      match peek scanner with
      | '|' ->
        next2 scanner;
        Token.Lor
      | '>' ->
        next2 scanner;
        Token.BarGreater
      | _ ->
        next scanner;
        Token.Bar)
    | '&' -> (
      match peek scanner with
      | '&' ->
        next2 scanner;
        Token.Land
      | _ ->
        next scanner;
        Token.Band)
    | ':' -> (
      match peek scanner with
      | '=' ->
        next2 scanner;
        Token.ColonEqual
      | '>' ->
        next2 scanner;
        Token.ColonGreaterThan
      | _ ->
        next scanner;
        Token.Colon)
    | '\\' -> scan_exotic_identifier scanner
    | '/' -> (
      match peek scanner with
      | '/' ->
        next2 scanner;
        scan_single_line_comment scanner
      | '*' -> scan_multi_line_comment scanner
      | '.' ->
        next2 scanner;
        Token.ForwardslashDot
      | _ ->
        next scanner;
        Token.Forwardslash)
    | '-' -> (
      match peek scanner with
      | '.' ->
        next2 scanner;
        Token.MinusDot
      | '>' ->
        next2 scanner;
        Token.MinusGreater
      | _ ->
        next scanner;
        Token.Minus)
    | '+' -> (
      match peek scanner with
      | '.' ->
        next2 scanner;
        Token.PlusDot
      | '+' ->
        next2 scanner;
        Token.PlusPlus
      | '=' ->
        next2 scanner;
        Token.PlusEqual
      | _ ->
        next scanner;
        Token.Plus)
    | '>' -> (
      match peek scanner with
      | '=' when not (in_diamond_mode scanner) ->
        next2 scanner;
        Token.GreaterEqual
      | _ ->
        next scanner;
        Token.GreaterThan)
    | '<' when not (in_jsx_mode scanner) -> (
      match peek scanner with
      | '=' ->
        next2 scanner;
        Token.LessEqual
      | _ ->
        next scanner;
        Token.LessThan)
    (* special handling for JSX < *)
    | '<' -> (
      (* Imagine the following: <div><
       * < indicates the start of a new jsx-element, the parser expects
       * the name of a new element after the <
       * Example: <div> <div
       * But what if we have a / here: example </ in  <div></div>
       * This signals a closing element. To simulate the two-token lookahead,
       * the </ is emitted as a single new token LessThanSlash *)
      next scanner;
      skip_whitespace scanner;
      match scanner.ch with
      | '/' ->
        next scanner;
        Token.LessThanSlash
      | '=' ->
        next scanner;
        Token.LessEqual
      | _ -> Token.LessThan)
    (* peeking 2 chars *)
    | '.' -> (
      match (peek scanner, peek2 scanner) with
      | '.', '.' ->
        next3 scanner;
        Token.DotDotDot
      | '.', _ ->
        next2 scanner;
        Token.DotDot
      | _ ->
        next scanner;
        Token.Dot)
    | '\'' -> (
      match (peek scanner, peek2 scanner) with
      | '\\', '"' ->
        (* careful with this one! We're next-ing _once_ (not twice),
           then relying on matching on the quote *)
        next scanner;
        SingleQuote
      | '\\', _ ->
        next2 scanner;
        scan_escape scanner
      | ch, '\'' ->
        let offset = scanner.offset + 1 in
        next3 scanner;
        Token.Codepoint
          {
            c = Char.code ch;
            original = (String.sub [@doesNotRaise]) scanner.src offset 1;
          }
      | ch, _ ->
        next scanner;
        let offset = scanner.offset in
        let offset16 = scanner.offset16 in
        let codepoint, length =
          Res_utf8.decode_code_point scanner.offset scanner.src
            (String.length scanner.src)
        in
        for _ = 0 to length - 1 do
          next scanner
        done;
        if scanner.ch = '\'' then (
          let contents =
            (String.sub [@doesNotRaise]) scanner.src offset length
          in
          next scanner;
          Token.Codepoint {c = codepoint; original = contents})
        else (
          scanner.ch <- ch;
          scanner.offset <- offset;
          scanner.offset16 <- offset16;
          SingleQuote))
    | '!' -> (
      match (peek scanner, peek2 scanner) with
      | '=', '=' ->
        next3 scanner;
        Token.BangEqualEqual
      | '=', _ ->
        next2 scanner;
        Token.BangEqual
      | _ ->
        next scanner;
        Token.Bang)
    | '=' -> (
      match (peek scanner, peek2 scanner) with
      | '=', '=' ->
        next3 scanner;
        Token.EqualEqualEqual
      | '=', _ ->
        next2 scanner;
        Token.EqualEqual
      | '>', _ ->
        next2 scanner;
        Token.EqualGreater
      | _ ->
        next scanner;
        Token.Equal)
    (* special cases *)
    | ch when ch == hacky_eof_char ->
      next scanner;
      Token.Eof
    | ch ->
      (* if we arrive here, we're dealing with an unknown character,
       * report the error and continue scanning… *)
      next scanner;
      let end_pos = position scanner in
      scanner.err ~start_pos ~end_pos (Diagnostics.unknown_uchar ch);
      let _, _, token = scan scanner in
      token
  in
  let end_pos = position scanner in
  (* _printDebug ~startPos ~endPos scanner token; *)
  (start_pos, end_pos, token)

(* misc helpers used elsewhere *)

(* Imagine: <div> <Navbar /> <
 * is `<` the start of a jsx-child? <div …
 * or is it the start of a closing tag?  </div>
 * reconsiderLessThan peeks at the next token and
 * determines the correct token to disambiguate *)
let reconsider_less_than scanner =
  (* < consumed *)
  skip_whitespace scanner;
  if scanner.ch == '/' then
    let () = next scanner in
    Token.LessThanSlash
  else Token.LessThan

(* If an operator has whitespace around both sides, it's a binary operator *)
(* TODO: this helper seems out of place *)
let is_binary_op src start_cnum end_cnum =
  if start_cnum == 0 then false
  else (
    (* we're gonna put some assertions and invariant checks here because this is
       used outside of the scanner's normal invariant assumptions *)
    assert (end_cnum >= 0);
    assert (start_cnum > 0 && start_cnum < String.length src);
    let left_ok = is_whitespace (String.unsafe_get src (start_cnum - 1)) in
    (* we need some stronger confidence that endCnum is ok *)
    let right_ok =
      end_cnum >= String.length src
      || is_whitespace (String.unsafe_get src end_cnum)
    in
    left_ok && right_ok)
