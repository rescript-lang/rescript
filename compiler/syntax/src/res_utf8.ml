(* https://tools.ietf.org/html/rfc3629#section-10 *)
(* let bom = 0xFEFF *)

let repl = 0xFFFD

(* let min = 0x0000 *)
let max = 0x10FFFF

let surrogate_min = 0xD800
let surrogate_max = 0xDFFF

(*
 * Char. number range  |        UTF-8 octet sequence
 *       (hexadecimal)    |              (binary)
 *    --------------------+---------------------------------------------
 *    0000 0000-0000 007F | 0xxxxxxx
 *    0000 0080-0000 07FF | 110xxxxx 10xxxxxx
 *    0000 0800-0000 FFFF | 1110xxxx 10xxxxxx 10xxxxxx
 *    0001 0000-0010 FFFF | 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 *)
let h2 = 0b1100_0000
let h3 = 0b1110_0000
let h4 = 0b1111_0000

let cont_mask = 0b0011_1111

type category = {low: int; high: int; size: int}

let locb = 0b1000_0000
let hicb = 0b1011_1111

let category_table = [|
  (* 0 *) {low = -1; high= -1; size= 1}; (* invalid *)
  (* 1 *) {low = 1; high= -1; size= 1}; (* ascii *)
  (* 2 *) {low = locb; high= hicb; size= 2};
  (* 3 *) {low = 0xA0; high= hicb; size= 3};
  (* 4 *) {low = locb; high= hicb; size= 3};
  (* 5 *) {low = locb; high= 0x9F; size= 3};
  (* 6 *) {low = 0x90; high= hicb;  size= 4};
  (* 7 *) {low = locb; high= hicb; size= 4};
  (* 8 *) {low = locb; high= 0x8F; size= 4};
|] [@@ocamlformat "disable"]

let categories = [|
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;
  1; 1; 1; 1; 1; 1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1 ;1;

  0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;
  0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;
  0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;
  0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;0; 0; 0; 0;
  (* surrogate range U+D800 - U+DFFFF = 55296 - 917503 *)
  0; 0; 2; 2;2; 2; 2; 2;2; 2; 2; 2;2; 2; 2; 2;
   2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2;
   3; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 4; 5; 4; 4;
  6; 7; 7 ;7; 8; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
|] [@@ocamlformat "disable"]

let decode_code_point i s len =
  if len < 1 then (repl, 1)
  else
    let first = int_of_char (String.unsafe_get s i) in
    if first < 128 then (first, 1)
    else
      let index = Array.unsafe_get categories first in
      if index = 0 then (repl, 1)
      else
        let cat = Array.unsafe_get category_table index in
        if len < i + cat.size then (repl, 1)
        else if cat.size == 2 then
          let c1 = int_of_char (String.unsafe_get s (i + 1)) in
          if c1 < cat.low || cat.high < c1 then (repl, 1)
          else
            let i1 = c1 land 0b00111111 in
            let i0 = (first land 0b00011111) lsl 6 in
            let uc = i0 lor i1 in
            (uc, 2)
        else if cat.size == 3 then
          let c1 = int_of_char (String.unsafe_get s (i + 1)) in
          let c2 = int_of_char (String.unsafe_get s (i + 2)) in
          if c1 < cat.low || cat.high < c1 || c2 < locb || hicb < c2 then
            (repl, 1)
          else
            let i0 = (first land 0b00001111) lsl 12 in
            let i1 = (c1 land 0b00111111) lsl 6 in
            let i2 = c2 land 0b00111111 in
            let uc = i0 lor i1 lor i2 in
            (uc, 3)
        else
          let c1 = int_of_char (String.unsafe_get s (i + 1)) in
          let c2 = int_of_char (String.unsafe_get s (i + 2)) in
          let c3 = int_of_char (String.unsafe_get s (i + 3)) in
          if
            c1 < cat.low || cat.high < c1 || c2 < locb || hicb < c2 || c3 < locb
            || hicb < c3
          then (repl, 1)
          else
            let i1 = (c1 land 0x3f) lsl 12 in
            let i2 = (c2 land 0x3f) lsl 6 in
            let i3 = c3 land 0x3f in
            let i0 = (first land 0x07) lsl 18 in
            let uc = i0 lor i3 lor i2 lor i1 in
            (uc, 4)

let encode_code_point c =
  if c <= 127 then (
    let bytes = (Bytes.create [@doesNotRaise]) 1 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr c);
    Bytes.unsafe_to_string bytes)
  else if c <= 2047 then (
    let bytes = (Bytes.create [@doesNotRaise]) 2 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h2 lor (c lsr 6)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes)
  else if c <= 65535 then (
    let bytes = (Bytes.create [@doesNotRaise]) 3 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h3 lor (c lsr 12)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 6) land cont_mask)));
    Bytes.unsafe_set bytes 2
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes)
  else
    (* if c <= max then *)
    let bytes = (Bytes.create [@doesNotRaise]) 4 in
    Bytes.unsafe_set bytes 0 (Char.unsafe_chr (h4 lor (c lsr 18)));
    Bytes.unsafe_set bytes 1
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 12) land cont_mask)));
    Bytes.unsafe_set bytes 2
      (Char.unsafe_chr (0b1000_0000 lor ((c lsr 6) land cont_mask)));
    Bytes.unsafe_set bytes 3
      (Char.unsafe_chr (0b1000_0000 lor (c land cont_mask)));
    Bytes.unsafe_to_string bytes

let is_valid_code_point c =
  (0 <= c && c < surrogate_min) || (surrogate_max < c && c <= max)
