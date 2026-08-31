let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let is_high_surrogate codepoint = codepoint >= 0xd800 && codepoint <= 0xdbff
let is_low_surrogate codepoint = codepoint >= 0xdc00 && codepoint <= 0xdfff

let combine_surrogate_pair high low =
  0x10000 + ((high - 0xd800) lsl 10) + (low - 0xdc00)

let decode_js_escapes s =
  let len = String.length s in
  let buf = Buffer.create len in
  let add_codepoint codepoint =
    if codepoint > 0x10ffff || (codepoint >= 0xd800 && codepoint <= 0xdfff) then
      false
    else (
      Buffer.add_string buf (Ext_utf8.encode_codepoint codepoint);
      true)
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
  let rec loop index =
    if index = len then Some (Buffer.contents buf)
    else
      match s.[index] with
      | '\\' when index + 1 >= len -> None
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
        | '0' ->
          Buffer.add_char buf '\000';
          loop (index + 2)
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
        | c ->
          (* JavaScript non-escape characters, such as [\a], evaluate to the
             character following the backslash. This also handles escaped
             quotes, backslashes, dollars, backticks, and spaces. *)
          Buffer.add_char buf c;
          loop (index + 2))
      | c ->
        Buffer.add_char buf c;
        loop (index + 1)
  in
  loop 0

let runtime_value s delim =
  match delim with
  | Some ("*j" | "bq") -> (
    match decode_js_escapes s with
    | Some decoded -> decoded
    | None -> s)
  | None | Some _ -> s

let compare (s1, delim1) (s2, delim2) =
  String.compare (runtime_value s1 delim1) (runtime_value s2 delim2)
