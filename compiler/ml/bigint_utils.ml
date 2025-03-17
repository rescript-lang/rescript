let is_neg s = String.length s > 0 && s.[0] = '-'
let is_pos s = String.length s > 0 && s.[0] = '+'

let to_string sign s = (if sign then "" else "-") ^ s

let remove_leading_sign str : bool * string =
  let len = String.length str in
  if len = 0 then (false, str)
  else if is_neg str || is_pos str then
    (not (is_neg str), String.sub str 1 (len - 1))
  else (true, str)

(*
   Removes leading zeros from the string only if the first non-zero character
   encountered is a digit. Unlike int and float, bigint cannot be of_string, so
   This function removes only leading 0s. Instead, values like 00x1 are not converted
   and are intended to be syntax errors.

   000n -> 0n
   001n -> 1n
   01_000_000n -> 1000000n
   -00100n -> -100n

   The following values are syntax errors

   00o1n -> 00o1n
   00x1_000_000n -> 00x1000000n
*)
let remove_leading_zeros str =
  let aux str =
    let len = String.length str in
    if len = 0 then ""
    else
      let is_digit c = c >= '0' && c <= '9' in
      let idx = ref 0 in
      while !idx < len && str.[!idx] = '0' do
        incr idx
      done;
      if !idx >= len then "0"
        (* If the string contains only '0's, return '0'. *)
      else if is_digit str.[!idx] then String.sub str !idx (len - !idx)
        (* Remove leading zeros and return the rest of the string. *)
      else str
  in
  (* Replace the delimiters '_' inside number *)
  let str = String.concat "" (String.split_on_char '_' str) in
  (* Check if negative *)
  let starts_with_minus = str <> "" && str.[0] = '-' in
  let str =
    if is_neg str || is_pos str then String.sub str 1 (String.length str - 1)
    else str
  in
  let processed_str = aux str in
  if starts_with_minus then "-" ^ processed_str else processed_str

let parse_bigint s =
  let sign, i = remove_leading_sign s in
  (sign, remove_leading_zeros i)

let is_valid s =
  let len = String.length s in
  if len = 0 then false
  else
    let is_digit c = (c >= '0' && c <= '9') || c = '_' in
    let first_char = s.[0] in
    if first_char <> '-' && first_char <> '+' && not (is_digit first_char) then
      false
    else
      let rec check idx =
        if idx >= len then true
        else
          let c = s.[idx] in
          if is_digit c then check (idx + 1) else false
      in
      check 1

let compare (p0, s0) (p1, s1) =
  match (p0, p1) with
  | false, true -> -1 (* If only s1 is positive, s0 is smaller. *)
  | true, false -> 1 (* If only s0 is positive, s0 is larger. *)
  | _ ->
    (* If both numbers are either negative or positive, compare their lengths. *)
    let len0, len1 = (String.length s0, String.length s1) in
    if len0 = len1 then
      if p0 then String.compare s0 s1
      else
        String.compare s1
          s0 (* If lengths are equal, compare the strings directly. *)
    else if len0 > len1 then
      if p0 then 1
      else -1 (* A longer s0 means it's larger unless it's negative. *)
    else if
      (* len0 < len1 *)
      p0
    then -1
    else 1 (* A longer s1 means s0 is smaller unless s1 is negative. *)
