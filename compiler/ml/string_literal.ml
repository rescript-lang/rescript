type string_kind
type template_kind

(* [source] is the original literal body and [semantic] is exactly what
   JavaScript evaluates that body to. [Invalid_source] is confined by the
   interface to ordinary-string parser recovery. *)
type 'kind payload =
  | Valid of {source: string; semantic: string}
  | Invalid_source of string
type string_literal = string_kind payload
type template_segment = template_kind payload

let source = function
  | Valid {source} | Invalid_source source -> source
let semantic = function
  | Valid {semantic} -> semantic
  | Invalid_source _ -> ""

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let is_high_surrogate codepoint = codepoint >= 0xd800 && codepoint <= 0xdbff
let is_low_surrogate codepoint = codepoint >= 0xdc00 && codepoint <= 0xdfff

let combine_surrogate_pair high low =
  0x10000 + ((high - 0xd800) lsl 10) + (low - 0xdc00)

let is_valid_utf8 = String.is_valid_utf_8

let replace_invalid_utf8 s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec loop index =
    if index < len then
      let decoded = String.get_utf_8_uchar s index in
      let decoded_len = Uchar.utf_decode_length decoded in
      if Uchar.utf_decode_is_valid decoded then (
        Buffer.add_substring buf s index decoded_len;
        loop (index + decoded_len))
      else (
        Buffer.add_utf_8_uchar buf Uchar.rep;
        loop (index + decoded_len))
  in
  loop 0;
  Buffer.contents buf

let normalize_semantic semantic =
  (* Valid compiler-generated strings need no copy. For malformed input, map
     each byte independently to preserve the value previously emitted as a
     JavaScript [\xHH] escape. *)
  if is_valid_utf8 semantic then semantic
  else
    let length = String.length semantic in
    let buffer = Buffer.create length in
    let rec loop index =
      if index < length then
        let decoded = String.get_utf_8_uchar semantic index in
        if Uchar.utf_decode_is_valid decoded then (
          let decoded_length = Uchar.utf_decode_length decoded in
          Buffer.add_substring buffer semantic index decoded_length;
          loop (index + decoded_length))
        else (
          Buffer.add_string buffer
            (Ext_utf8.encode_codepoint
               (Char.code (String.unsafe_get semantic index)));
          loop (index + 1))
    in
    loop 0;
    Buffer.contents buffer

let decode_js_escapes_with ~normalize_template_line_endings s =
  let len = String.length s in
  let buf = Buffer.create len in
  let add_codepoint codepoint =
    if Uchar.is_valid codepoint then (
      Buffer.add_utf_8_uchar buf (Uchar.of_int codepoint);
      true)
    else false
  in
  let decode_fixed_hex start count =
    let rec loop index remaining value =
      if remaining = 0 then Some value
      else if index >= len then None
      else
        let digit = hex_value s.[index] in
        if digit < 0 then None
        else loop (index + 1) (remaining - 1) ((value * 16) + digit)
    in
    loop start count 0
  in
  let decode_braced_hex start =
    let rec loop index value has_digit =
      if index >= len then None
      else
        match s.[index] with
        | '}' when has_digit -> Some (value, index + 1)
        | c ->
          let digit = hex_value c in
          if digit < 0 || value > (0x10ffff - digit) / 16 then None
          else loop (index + 1) ((value * 16) + digit) true
    in
    loop start 0 false
  in
  let copy_utf8 index =
    let decoded = String.get_utf_8_uchar s index in
    if Uchar.utf_decode_is_valid decoded then (
      let length = Uchar.utf_decode_length decoded in
      Buffer.add_substring buf s index length;
      Some (index + length))
    else None
  in
  let rec loop index =
    if index = len then Some (Buffer.contents buf)
    else
      match s.[index] with
      | '\\' when index + 1 >= len -> None
      | '\\'
        when index + 3 < len
             && s.[index + 1] = '\226'
             && s.[index + 2] = '\128'
             && (s.[index + 3] = '\168' || s.[index + 3] = '\169') ->
        (* U+2028 LINE SEPARATOR and U+2029 PARAGRAPH SEPARATOR are JavaScript
           line terminators, so a preceding backslash makes them line
           continuations just like LF and CR. *)
        loop (index + 4)
      | '\\' -> (
        match s.[index + 1] with
        | 'b' ->
          Buffer.add_char buf '\b';
          loop (index + 2)
        | 'f' ->
          Buffer.add_char buf '\012';
          loop (index + 2)
        | 'n' ->
          Buffer.add_char buf '\n';
          loop (index + 2)
        | 'r' ->
          Buffer.add_char buf '\r';
          loop (index + 2)
        | 't' ->
          Buffer.add_char buf '\t';
          loop (index + 2)
        | 'v' ->
          Buffer.add_char buf '\011';
          loop (index + 2)
        | '0'
          when normalize_template_line_endings
               && index + 2 < len
               && s.[index + 2] >= '0'
               && s.[index + 2] <= '9' ->
          None
        | '0' ->
          Buffer.add_char buf '\000';
          loop (index + 2)
        | '1' .. '9' when normalize_template_line_endings -> None
        | '\n' -> loop (index + 2)
        | '\r' ->
          if index + 2 < len && s.[index + 2] = '\n' then loop (index + 3)
          else loop (index + 2)
        | 'x' -> (
          match decode_fixed_hex (index + 2) 2 with
          | Some codepoint when add_codepoint codepoint -> loop (index + 4)
          | Some _ | None -> None)
        | 'u' when index + 2 < len && s.[index + 2] = '{' -> (
          match decode_braced_hex (index + 3) with
          | Some (codepoint, next) when add_codepoint codepoint -> loop next
          | Some _ | None -> None)
        | 'u' -> (
          match decode_fixed_hex (index + 2) 4 with
          | Some high when is_high_surrogate high ->
            if index + 7 < len && s.[index + 6] = '\\' && s.[index + 7] = 'u'
            then
              match decode_fixed_hex (index + 8) 4 with
              | Some low when is_low_surrogate low ->
                let codepoint = combine_surrogate_pair high low in
                if add_codepoint codepoint then loop (index + 12) else None
              | Some _ | None -> None
            else None
          | Some codepoint when add_codepoint codepoint -> loop (index + 6)
          | Some _ | None -> None)
        | _ -> (
          (* JavaScript non-escape characters, such as [\a], evaluate to the
             character following the backslash. This also handles escaped
             quotes, backslashes, dollars, backticks, and spaces. *)
          match copy_utf8 (index + 1) with
          | Some next -> loop next
          | None -> None))
      | '\r' when normalize_template_line_endings ->
        (* JavaScript's template value normalizes literal CR and CRLF source
           line endings to LF. This branch deliberately runs after escape
           handling, so an explicit [\r] escape still decodes to CR, and the
           ordinary string decoder continues to preserve literal line endings. *)
        Buffer.add_char buf '\n';
        if index + 1 < len && s.[index + 1] = '\n' then loop (index + 2)
        else loop (index + 1)
      | '$'
        when normalize_template_line_endings
             && index + 1 < len
             && s.[index + 1] = '{' ->
        (* An unescaped interpolation opener cannot occur inside one template
           segment. Treat it as invalid so callers can safely validate joined
           segment sources with this decoder. *)
        None
      | _ -> (
        match copy_utf8 index with
        | Some next -> loop next
        | None -> None)
  in
  loop 0

let decode_js_escapes =
  decode_js_escapes_with ~normalize_template_line_endings:false

let decode_js_template_escapes =
  decode_js_escapes_with ~normalize_template_line_endings:true

type encode_js_mode = String | Template

let encode_js mode s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\b' -> Buffer.add_string buf {e|\b|e}
      | '\012' -> Buffer.add_string buf {e|\f|e}
      | '\n' -> Buffer.add_string buf {e|\n|e}
      | '\r' -> Buffer.add_string buf {e|\r|e}
      | '\t' -> Buffer.add_string buf {e|\t|e}
      | '\011' -> Buffer.add_string buf {e|\v|e}
      | '\\' -> Buffer.add_string buf {e|\\|e}
      | ('\000' .. '\031' | '\127') as c ->
        Buffer.add_string buf (Printf.sprintf {e|\x%02X|e} (Char.code c))
      | '"' as c -> (
        match mode with
        | String -> Buffer.add_string buf {e|\"|e}
        | Template -> Buffer.add_char buf c)
      | ('`' | '$') as c -> (
        match mode with
        | String -> Buffer.add_char buf c
        | Template ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c)
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let encode_js_string = encode_js String

let encode_js_template = encode_js Template

let string_from_source source : string_literal option =
  match decode_js_escapes source with
  | Some semantic -> Some (Valid {source; semantic})
  | None -> None

let string_from_semantic semantic : string_literal =
  let semantic = normalize_semantic semantic in
  Valid {source = encode_js_string semantic; semantic}

let invalid_string_for_recovery source : string_literal = Invalid_source source

let template_from_source source : template_segment option =
  match decode_js_template_escapes source with
  | Some semantic -> Some (Valid {source; semantic})
  | None -> None

let template_from_semantic semantic : template_segment =
  let semantic = normalize_semantic semantic in
  Valid {source = encode_js_template semantic; semantic}

let concat_template segments =
  let source = String.concat "" (List.map source segments) in
  let semantic = String.concat "" (List.map semantic segments) in
  (* Joining individually valid sources can change their interpretation at a
     boundary, most notably by creating an unescaped [${]. Preserve the joined
     spelling only if decoding it still produces the joined semantic value. *)
  match decode_js_template_escapes source with
  | Some decoded when decoded = semantic -> Valid {source; semantic}
  | _ -> template_from_semantic semantic

let encode_char_source codepoint =
  match codepoint with
  | 0x08 -> {e|\b|e}
  | 0x09 -> {e|\t|e}
  | 0x0a -> {e|\n|e}
  | 0x0d -> {e|\r|e}
  | 0x27 -> {e|\'|e}
  | 0x5c -> {e|\\|e}
  | codepoint when (codepoint >= 0x00 && codepoint <= 0x1f) || codepoint = 0x7f
    ->
    Printf.sprintf {e|\x%02X|e} codepoint
  | codepoint when codepoint >= 0x20 && codepoint <= 0x7e ->
    String.make 1 (Char.unsafe_chr codepoint)
  | codepoint when Uchar.is_valid codepoint ->
    Ext_utf8.encode_codepoint codepoint
  | codepoint -> Printf.sprintf {e|\u{%X}|e} codepoint

let decode_utf8_uchar_exn s index =
  let decoded = String.get_utf_8_uchar s index in
  if Uchar.utf_decode_is_valid decoded then decoded
  else raise (Ext_utf8.Invalid_utf8 "Invalid UTF-8 sequence")

let utf16_length s =
  let len = String.length s in
  let rec loop length index =
    if index = len then length
    else
      let decoded = decode_utf8_uchar_exn s index in
      let codepoint = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
      loop
        (length + if codepoint > 0xffff then 2 else 1)
        (index + Uchar.utf_decode_length decoded)
  in
  loop 0 0

let code_point_at_utf16_index s index =
  if index < 0 then None
  else
    let len = String.length s in
    let rec loop utf16_index byte_index =
      if byte_index = len then None
      else
        let decoded = decode_utf8_uchar_exn s byte_index in
        let codepoint = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
        if utf16_index = index then Some codepoint
        else if codepoint > 0xffff && utf16_index + 1 = index then
          Some (0xdc00 + ((codepoint - 0x10000) land 0x3ff))
        else
          loop
            (utf16_index + if codepoint > 0xffff then 2 else 1)
            (byte_index + Uchar.utf_decode_length decoded)
    in
    loop 0 0
