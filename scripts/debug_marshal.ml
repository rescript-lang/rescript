(* debug_marshal.ml - Debug utility to decode OCaml Marshal binary format

   Usage: ocaml debug_marshal.ml <file.ast>

   This tool decodes the binary AST format and shows:
   - Magic headers
   - Marshal structure (blocks, strings, ints, shared refs)
   - Object indices for understanding sharing patterns

   Object counter rules (matching OCaml Marshal):
   - Blocks, strings increment counter
   - Ints, shared refs do NOT increment counter
*)

let read_u8 ic = input_byte ic

let read_u16 ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  (b0 lsl 8) lor b1

let read_u32 ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3

let read_string ic len =
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  Bytes.to_string buf

(* Marshal format constants *)
let code_int8 = 0x00
let code_int16 = 0x01
let code_int32 = 0x02
let code_int64 = 0x03
let code_shared8 = 0x04
let code_shared16 = 0x05
let code_shared32 = 0x06
let code_block32 = 0x08
let code_block64 = 0x13
let code_string8 = 0x09
let code_string32 = 0x0A
let code_double_big = 0x0B
let code_double_little = 0x0C
let code_double_array8_big = 0x0D
let code_double_array8_little = 0x0E
let code_double_array32_big = 0x0F
let code_double_array32_little = 0x10
let code_codepointer = 0x11
let code_infixpointer = 0x12
let code_custom = 0x18
let code_custom_len = 0x19
let code_custom_fixed = 0x1A

(* Decode a marshal value and return obj_count_delta.
   obj_counter is the index that will be assigned to the NEXT block/string.
   Ints and shared refs don't get object indices. *)
let rec decode_value ic obj_counter indent =
  let byte = read_u8 ic in
  let prefix = String.make (indent * 2) ' ' in

  if byte >= 0x40 && byte <= 0x7F then begin
    (* Small positive integer: 0..63 - NO object index *)
    let n = byte - 0x40 in
    Printf.printf "%s    INT %d\n" prefix n;
    0  (* Ints don't increment counter *)
  end
  else if byte >= 0x80 && byte <= 0xFF then begin
    (* Small block: 0x80-0xFF - GETS object index *)
    let code = byte - 0x80 in
    let tag = code land 0x0F in
    let size = (code lsr 4) land 0x07 in
    Printf.printf "%s[%d] BLOCK tag=%d size=%d {\n" prefix obj_counter tag size;
    let next_counter = ref (obj_counter + 1) in
    for _ = 1 to size do
      let delta = decode_value ic !next_counter (indent + 1) in
      next_counter := !next_counter + delta
    done;
    Printf.printf "%s}\n" prefix;
    !next_counter - obj_counter  (* Return total objects created *)
  end
  else if byte >= 0x20 && byte <= 0x3F then begin
    (* Small string: length 0..31 - GETS object index *)
    let len = byte - 0x20 in
    let s = read_string ic len in
    Printf.printf "%s[%d] STRING[%d] %S\n" prefix obj_counter len s;
    1
  end
  else begin
    (* Extended codes *)
    match byte with
    | 0x00 -> (* INT8 - NO object index *)
      let n = read_u8 ic in
      let n = if n >= 128 then n - 256 else n in
      Printf.printf "%s    INT8 %d\n" prefix n;
      0
    | 0x01 -> (* INT16 - NO object index *)
      let n = read_u16 ic in
      let n = if n >= 32768 then n - 65536 else n in
      Printf.printf "%s    INT16 %d\n" prefix n;
      0
    | 0x02 -> (* INT32 - NO object index *)
      let n = read_u32 ic in
      Printf.printf "%s    INT32 %d\n" prefix n;
      0
    | 0x04 -> (* SHARED8 - NO object index *)
      let offset = read_u8 ic in
      let target = obj_counter - offset in
      Printf.printf "%s    SHARED -> [%d] (offset=%d)\n" prefix target offset;
      0
    | 0x05 -> (* SHARED16 - NO object index *)
      let offset = read_u16 ic in
      let target = obj_counter - offset in
      Printf.printf "%s    SHARED -> [%d] (offset=%d)\n" prefix target offset;
      0
    | 0x06 -> (* SHARED32 - NO object index *)
      let offset = read_u32 ic in
      let target = obj_counter - offset in
      Printf.printf "%s    SHARED -> [%d] (offset=%d)\n" prefix target offset;
      0
    | 0x08 -> (* BLOCK32 - GETS object index *)
      let header = read_u32 ic in
      let tag = header land 0xFF in
      let size = header lsr 10 in
      Printf.printf "%s[%d] BLOCK32 tag=%d size=%d {\n" prefix obj_counter tag size;
      let next_counter = ref (obj_counter + 1) in
      for _ = 1 to size do
        let delta = decode_value ic !next_counter (indent + 1) in
        next_counter := !next_counter + delta
      done;
      Printf.printf "%s}\n" prefix;
      !next_counter - obj_counter
    | 0x09 -> (* STRING8 - GETS object index *)
      let len = read_u8 ic in
      let s = read_string ic len in
      Printf.printf "%s[%d] STRING8[%d] %S\n" prefix obj_counter len s;
      1
    | 0x0A -> (* STRING32 - GETS object index *)
      let len = read_u32 ic in
      let s = read_string ic len in
      Printf.printf "%s[%d] STRING32[%d] %S\n" prefix obj_counter len s;
      1
    | _ ->
      Printf.printf "%s    UNKNOWN code=0x%02X\n" prefix byte;
      0
  end

let decode_marshal_block ic name =
  (* Read magic *)
  let magic = read_string ic 4 in
  if magic <> "\x84\x95\xa6\xbe" then begin
    Printf.printf "ERROR: Invalid marshal magic for %s: %S\n" name magic;
    exit 1
  end;

  (* Read header *)
  let data_len = read_u32 ic in
  let num_objects = read_u32 ic in
  let size_32 = read_u32 ic in
  let size_64 = read_u32 ic in

  Printf.printf "\n=== %s ===\n" name;
  Printf.printf "  data_len=%d num_objects=%d size_32=%d size_64=%d\n"
    data_len num_objects size_32 size_64;
  Printf.printf "  Content:\n";

  (* Decode the value *)
  let _ = decode_value ic 0 2 in
  ()

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: ocaml %s <file.ast>\n" Sys.argv.(0);
    Printf.printf "\nDecodes and displays the structure of an OCaml Marshal binary file.\n";
    Printf.printf "Shows object indices to help debug sharing patterns.\n";
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let ic = open_in_bin filename in

  (* Read file magic *)
  let file_magic = read_string ic 12 in
  Printf.printf "File magic: %S\n" file_magic;

  if file_magic = "Caml1999M022" then
    Printf.printf "  (Implementation AST)\n"
  else if file_magic = "Caml1999N022" then
    Printf.printf "  (Interface AST)\n"
  else begin
    Printf.printf "  (Unknown format)\n";
    exit 1
  end;

  (* Decode filename marshal block *)
  decode_marshal_block ic "Filename";

  (* Decode AST marshal block *)
  decode_marshal_block ic "AST";

  close_in ic;
  Printf.printf "\nDone.\n"
