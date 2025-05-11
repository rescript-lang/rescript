(** # Json parser
 *
 * Works with bucklescript and bsb-native
 *
 * ## Basics
 *
 * ```
 * open Json.Infix; /* for the nice infix operators */
 * let raw = {|{"hello": "folks"}|};
 * let who = Json.parse(raw) |> Json.get("hello") |?> Json.string;
 * Js.log(who);
 * ```
 *
 * ## Parse & stringify
 *
 * @doc parse, stringify
 *
 * ## Accessing descendents
 *
 * @doc get, nth, getPath
 *
 * ## Coercing to types
 *
 * @doc string, number, array, obj, bool, null
 *
 * ## The JSON type
 *
 * @doc t
 *
 * ## Infix operators for easier working
 *
 * @doc Infix
 *)

type t =
  | String of string
  | Number of float
  | Array of t list
  | Object of (string * t) list
  | True
  | False
  | Null

let string_of_number f =
  let s = string_of_float f in
  if s.[String.length s - 1] = '.' then String.sub s 0 (String.length s - 1)
  else s

(**
 * This module is provided for easier working with optional values.
 *)
module Infix = struct
  (** The "force unwrap" operator
   *
   * If you're sure there's a value, you can force it.
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |! "Expected this to be present";
   * Js.log(x);
   * ```
   *
   * But you gotta be sure, otherwise it will throw.
   * ```reason;raises
   * open Json.Infix;
   * let x: int = None |! "This will throw";
   * ```
   *)
  let ( |! ) o d =
    match o with
    | None -> failwith d
    | Some v -> v

  (** The "upwrap with default" operator
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |? 4;
   * let y: int = None |? 5;
   * Js.log2(x, y);
   * ```
   *)
  let ( |? ) o d =
    match o with
    | None -> d
    | Some v -> v

  (** The "transform contents into new optional" operator
   * ```
   * open Json.Infix;
   * let maybeInc = x => x > 5 ? Some(x + 1) : None;
   * let x: option(int) = Some(14) |?> maybeInc;
   * let y: option(int) = None |?> maybeInc;
   * ```
   *)
  let ( |?> ) o fn =
    match o with
    | None -> None
    | Some v -> fn v

  (** The "transform contents into new value & then re-wrap" operator
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: option(int) = Some(7) |?>> inc;
   * let y: option(int) = None |?>> inc;
   * Js.log2(x, y);
   * ```
   *)
  let ( |?>> ) o fn =
    match o with
    | None -> None
    | Some v -> Some (fn v)

  (** "handle the value if present, otherwise here's the default"
   *
   * It's called fold because that's what people call it :?. It's the same as "transform contents to new value" + "unwrap with default".
   *
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: int = fold(Some(4), 10, inc);
   * let y: int = fold(None, 2, inc);
   * Js.log2(x, y);
   * ```
   *)
  let fold o d f =
    match o with
    | None -> d
    | Some v -> f v
end

let escape text =
  let ln = String.length text in
  let buf = Buffer.create ln in
  let rec loop i =
    if i < ln then (
      (match text.[i] with
      | '\012' -> Buffer.add_string buf "\\f"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c ->
        let code = Char.code c in
        if code < 0x20 then Printf.bprintf buf "\\u%04x" code
        else Buffer.add_char buf c);
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

(**
 * ```
 * let text = {|{"hello": "folks", "aa": [2, 3, "four"]}|};
 * let result = Json.stringify(Json.parse(text));
 * Js.log(result);
 * assert(text == result);
 * ```
 *)
let rec stringify t =
  match t with
  | String value -> "\"" ^ escape value ^ "\""
  | Number num -> string_of_number num
  | Array items -> "[" ^ String.concat ", " (List.map stringify items) ^ "]"
  | Object items ->
    "{"
    ^ String.concat ", "
        (List.map
           (fun (k, v) -> "\"" ^ String.escaped k ^ "\": " ^ stringify v)
           items)
    ^ "}"
  | True -> "true"
  | False -> "false"
  | Null -> "null"

let white n =
  let buffer = Buffer.create n in
  for i = 0 to n - 1 do
    Buffer.add_char buffer ' '
  done;
  Buffer.contents buffer

let rec stringifyPretty ?(indent = 0) t =
  match t with
  | String value -> "\"" ^ escape value ^ "\""
  | Number num -> string_of_number num
  | Array [] -> "[]"
  | Array [(String _ as contents)] -> "[" ^ stringifyPretty contents ^ "]"
  | Array items ->
    "[\n" ^ white indent
    ^ String.concat
        (",\n" ^ white indent)
        (List.map (stringifyPretty ~indent:(indent + 2)) items)
    ^ "\n"
    ^ white (indent - 2)
    ^ "]"
  | Object [] -> "{}"
  | Object items ->
    "{\n" ^ white indent
    ^ String.concat
        (",\n" ^ white indent)
        (List.map
           (fun (k, v) ->
             "\"" ^ String.escaped k ^ "\": "
             ^ stringifyPretty ~indent:(indent + 2) v)
           items)
    ^ "\n"
    ^ white (indent - 2)
    ^ "}"
  | True -> "true"
  | False -> "false"
  | Null -> "null"

let unwrap message t =
  match t with
  | Some v -> v
  | None -> failwith message

module Parser = struct
  let split_by ?(keep_empty = false) is_delim str =
    let len = String.length str in
    let rec loop acc last_pos pos =
      if pos = -1 then
        if last_pos = 0 && not keep_empty then acc
        else String.sub str 0 last_pos :: acc
      else if is_delim str.[pos] then
        let new_len = last_pos - pos - 1 in
        if new_len <> 0 || keep_empty then
          let v = String.sub str (pos + 1) new_len in
          loop (v :: acc) pos (pos - 1)
        else loop acc pos (pos - 1)
      else loop acc last_pos (pos - 1)
    in
    loop [] len (len - 1)

  let fail text pos message =
    let pre = String.sub text 0 pos in
    let lines = split_by (fun c -> c = '\n') pre in
    let count = List.length lines in
    let last =
      match count > 0 with
      | true -> List.nth lines (count - 1)
      | false -> ""
    in
    let col = String.length last + 1 in
    let line = List.length lines in
    let string =
      Printf.sprintf "Error \"%s\" at %d:%d -> %s\n" message line col last
    in
    failwith string

  let rec skipToNewline text pos =
    if pos >= String.length text then pos
    else if text.[pos] = '\n' then pos + 1
    else skipToNewline text (pos + 1)

  let stringTail text =
    let len = String.length text in
    if len > 1 then String.sub text 1 (len - 1) else ""

  let rec skipToCloseMultilineComment text pos =
    if pos + 1 >= String.length text then failwith "Unterminated comment"
    else if text.[pos] = '*' && text.[pos + 1] = '/' then pos + 2
    else skipToCloseMultilineComment text (pos + 1)

  let rec skipWhite text pos =
    if
      pos < String.length text
      && (text.[pos] = ' '
         || text.[pos] = '\t'
         || text.[pos] = '\n'
         || text.[pos] = '\r')
    then skipWhite text (pos + 1)
    else pos

  (* from https://stackoverflow.com/a/42431362 *)
  let utf8encode s =
    let prefs = [|0; 192; 224|] in
    let s1 n = String.make 1 (Char.chr n) in
    let rec ienc k sofar resid =
      let bct = if k = 0 then 7 else 6 - k in
      if resid < 1 lsl bct then s1 (prefs.(k) + resid) ^ sofar
      else ienc (k + 1) (s1 (128 + (resid mod 64)) ^ sofar) (resid / 64)
    in
    ienc 0 "" (int_of_string ("0x" ^ s))

  let parseString text pos =
    (* let i = ref(pos); *)
    let buffer = Buffer.create (String.length text) in
    let ln = String.length text in
    let rec loop i =
      match i >= ln with
      | true -> fail text i "Unterminated string"
      | false -> (
        match text.[i] with
        | '"' -> i + 1
        | '\\' -> (
          match i + 1 >= ln with
          | true -> fail text i "Unterminated string"
          | false -> (
            match text.[i + 1] with
            | '/' ->
              Buffer.add_char buffer '/';
              loop (i + 2)
            | 'f' ->
              Buffer.add_char buffer '\012';
              loop (i + 2)
            | 'u' when i + 6 < ln ->
              Buffer.add_string buffer (utf8encode (String.sub text (i + 2) 4));
              loop (i + 7)
            | _ ->
              Buffer.add_string buffer (Scanf.unescaped (String.sub text i 2));
              loop (i + 2)))
        | c ->
          Buffer.add_char buffer c;
          loop (i + 1))
    in
    let final = loop pos in
    (Buffer.contents buffer, final)

  let parseDigits text pos =
    let len = String.length text in
    let rec loop i =
      if i >= len then i
      else
        match text.[i] with
        | '0' .. '9' -> loop (i + 1)
        | _ -> i
    in
    loop (pos + 1)

  let parseWithDecimal text pos =
    let pos = parseDigits text pos in
    if pos < String.length text && text.[pos] = '.' then
      let pos = parseDigits text (pos + 1) in
      pos
    else pos

  let parseNumber text pos =
    let pos = parseWithDecimal text pos in
    let ln = String.length text in
    if pos < ln - 1 && (text.[pos] = 'E' || text.[pos] = 'e') then
      let pos =
        match text.[pos + 1] with
        | '-' | '+' -> pos + 2
        | _ -> pos + 1
      in
      parseDigits text pos
    else pos

  let parseNegativeNumber text pos =
    let final =
      if text.[pos] = '-' then parseNumber text (pos + 1)
      else parseNumber text pos
    in
    (Number (float_of_string (String.sub text pos (final - pos))), final)

  let expect char text pos message =
    if text.[pos] <> char then fail text pos ("Expected: " ^ message)
    else pos + 1

  let parseComment : 'a. string -> int -> (string -> int -> 'a) -> 'a =
   fun text pos next ->
    if text.[pos] <> '/' then
      if text.[pos] = '*' then
        next text (skipToCloseMultilineComment text (pos + 1))
      else failwith "Invalid syntax"
    else next text (skipToNewline text (pos + 1))

  let maybeSkipComment text pos =
    if pos < String.length text && text.[pos] = '/' then
      if pos + 1 < String.length text && text.[pos + 1] = '/' then
        skipToNewline text (pos + 1)
      else if pos + 1 < String.length text && text.[pos + 1] = '*' then
        skipToCloseMultilineComment text (pos + 1)
      else fail text pos "Invalid synatx"
    else pos

  let rec skip text pos =
    if pos = String.length text then pos
    else
      let n = skipWhite text pos |> maybeSkipComment text in
      if n > pos then skip text n else n

  let rec parse text pos =
    if pos >= String.length text then
      fail text pos "Reached end of file without being done parsing"
    else
      match text.[pos] with
      | '/' -> parseComment text (pos + 1) parse
      | '[' -> parseArray text (pos + 1)
      | '{' -> parseObject text (pos + 1)
      | 'n' ->
        if String.sub text pos 4 = "null" then (Null, pos + 4)
        else fail text pos "unexpected character"
      | 't' ->
        if String.sub text pos 4 = "true" then (True, pos + 4)
        else fail text pos "unexpected character"
      | 'f' ->
        if String.sub text pos 5 = "false" then (False, pos + 5)
        else fail text pos "unexpected character"
      | '\n' | '\t' | ' ' | '\r' -> parse text (skipWhite text pos)
      | '"' ->
        let s, pos = parseString text (pos + 1) in
        (String s, pos)
      | '-' | '0' .. '9' -> parseNegativeNumber text pos
      | _ -> fail text pos "unexpected character"

  and parseArrayValue text pos =
    let pos = skip text pos in
    let value, pos = parse text pos in
    let pos = skip text pos in
    match text.[pos] with
    | ',' ->
      let pos = skip text (pos + 1) in
      if text.[pos] = ']' then ([value], pos + 1)
      else
        let rest, pos = parseArrayValue text pos in
        (value :: rest, pos)
    | ']' -> ([value], pos + 1)
    | _ -> fail text pos "unexpected character"

  and parseArray text pos =
    let pos = skip text pos in
    match text.[pos] with
    | ']' -> (Array [], pos + 1)
    | _ ->
      let items, pos = parseArrayValue text pos in
      (Array items, pos)

  and parseObjectValue text pos =
    let pos = skip text pos in
    if text.[pos] <> '"' then fail text pos "Expected string"
    else
      let key, pos = parseString text (pos + 1) in
      let pos = skip text pos in
      let pos = expect ':' text pos "Colon" in
      let value, pos = parse text pos in
      let pos = skip text pos in
      match text.[pos] with
      | ',' ->
        let pos = skip text (pos + 1) in
        if text.[pos] = '}' then ([(key, value)], pos + 1)
        else
          let rest, pos = parseObjectValue text pos in
          ((key, value) :: rest, pos)
      | '}' -> ([(key, value)], pos + 1)
      | _ ->
        let rest, pos = parseObjectValue text pos in
        ((key, value) :: rest, pos)

  and parseObject text pos =
    let pos = skip text pos in
    if text.[pos] = '}' then (Object [], pos + 1)
    else
      let pairs, pos = parseObjectValue text pos in
      (Object pairs, pos)
end
[@@nodoc]

(** Turns some text into a json object. throws on failure *)
let parse text =
  try
    let item, pos = Parser.parse text 0 in
    let pos = Parser.skip text pos in
    if pos < String.length text then
      (* failwith
         ("Extra data after parse finished: "
         ^ String.sub text pos (String.length text - pos)) *)
      None
    else Some item
  with Invalid_argument _ | Failure _ -> None

(* Accessor helpers *)
let bind v fn =
  match v with
  | None -> None
  | Some v -> fn v

(** If `t` is an object, get the value associated with the given string key *)
let get key t =
  match t with
  | Object items -> ( try Some (List.assoc key items) with Not_found -> None)
  | _ -> None

(** If `t` is an array, get the value associated with the given index *)
let nth n t =
  match t with
  | Array items ->
    if n < List.length items then Some (List.nth items n) else None
  | _ -> None

let string t =
  match t with
  | String s -> Some s
  | _ -> None
let number t =
  match t with
  | Number s -> Some s
  | _ -> None
let array t =
  match t with
  | Array s -> Some s
  | _ -> None
let obj t =
  match t with
  | Object s -> Some s
  | _ -> None
let bool t =
  match t with
  | True -> Some true
  | False -> Some false
  | _ -> None
let null t =
  match t with
  | Null -> Some ()
  | _ -> None

let rec parsePath keyList t =
  match keyList with
  | [] -> Some t
  | head :: rest -> (
    match get head t with
    | None -> None
    | Some value -> parsePath rest value)

(** Get a deeply nested value from an object `t`.
 * ```
 * open Json.Infix;
 * let json = Json.parse({|{"a": {"b": {"c": 2}}}|});
 * let num = Json.getPath("a.b.c", json) |?> Json.number;
 * assert(num == Some(2.))
 * ```
 *)
let getPath path t =
  let keys = Parser.split_by (fun c -> c = '.') path in
  parsePath keys t
