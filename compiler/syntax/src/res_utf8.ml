(* https://tools.ietf.org/html/rfc3629#section-10 *)
(* let bom = 0xFEFF *)

let repl = Uchar.to_int Uchar.rep
let max = Uchar.to_int Uchar.max

let decode_code_point i s len =
  if i < 0 || i >= len || len > String.length s then (repl, 1)
  else
    let decoded = String.get_utf_8_uchar s i in
    let size = Uchar.utf_decode_length decoded in
    if Uchar.utf_decode_is_valid decoded && i + size <= len then
      (Uchar.to_int (Uchar.utf_decode_uchar decoded), size)
    else (repl, 1)

let encode_code_point c =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf (Uchar.of_int c);
  Buffer.contents buf

let is_valid_code_point = Uchar.is_valid
