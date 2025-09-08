(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*    Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt    *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Extensible buffers *)

type t = {mutable buffer: bytes; mutable position: int; mutable length: int}

let create n =
  let n = if n < 1 then 1 else n in
  let s = Bytes.create n in
  {buffer = s; position = 0; length = n}

let contents b = Bytes.sub_string b.buffer 0 b.position
(* let to_bytes b = Bytes.sub b.buffer 0 b.position  *)

(* let sub b ofs len =
   if ofs < 0 || len < 0 || ofs > b.position - len
   then invalid_arg "Ext_buffer.sub"
   else Bytes.sub_string b.buffer ofs len *)

(* let blit src srcoff dst dstoff len =
   if len < 0 || srcoff < 0 || srcoff > src.position - len
             || dstoff < 0 || dstoff > (Bytes.length dst) - len
   then invalid_arg "Ext_buffer.blit"
   else
    Bytes.unsafe_blit src.buffer srcoff dst dstoff len *)

let length b = b.position

let is_empty b = b.position = 0

let clear b = b.position <- 0

(* let reset b =
   b.position <- 0; b.buffer <- b.initial_buffer;
   b.length <- Bytes.length b.buffer *)

let resize b more =
  let len = b.length in
  let new_len = ref len in
  while b.position + more > !new_len do
    new_len := 2 * !new_len
  done;
  let new_buffer = Bytes.create !new_len in
  (* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
     this tricky function that is slow anyway. *)
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer;
  b.length <- !new_len;
  assert (b.position + more <= b.length)

let[@inline] add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

(* let add_substring b s offset len =
   if offset < 0 || len < 0 || offset > String.length s - len
   then invalid_arg "Ext_buffer.add_substring/add_subbytes";
   let new_position = b.position + len in
   if new_position > b.length then resize b len;
   Ext_bytes.unsafe_blit_string s offset b.buffer b.position len;
   b.position <- new_position *)

(* let add_subbytes b s offset len =
   add_substring b (Bytes.unsafe_to_string s) offset len *)

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Ext_bytes.unsafe_blit_string s 0 b.buffer b.position len;
  b.position <- new_position

(* TODO: micro-optimzie *)
let add_string_char b s c =
  let s_len = String.length s in
  let len = s_len + 1 in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in
  Ext_bytes.unsafe_blit_string s 0 b_buffer b.position s_len;
  Bytes.unsafe_set b_buffer (new_position - 1) c;
  b.position <- new_position

let add_char_string b c s =
  let s_len = String.length s in
  let len = s_len + 1 in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in
  let b_position = b.position in
  Bytes.unsafe_set b_buffer b_position c;
  Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 1) s_len;
  b.position <- new_position

(* equivalent to add_char " "; add_char "$"; add_string s  *)
let add_ninja_prefix_var b s =
  let s_len = String.length s in
  let len = s_len + 2 in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  let b_buffer = b.buffer in
  let b_position = b.position in
  Bytes.unsafe_set b_buffer b_position ' ';
  Bytes.unsafe_set b_buffer (b_position + 1) '$';
  Ext_bytes.unsafe_blit_string s 0 b_buffer (b_position + 2) s_len;
  b.position <- new_position

(* let add_bytes b s = add_string b (Bytes.unsafe_to_string s)

   let add_buffer b bs =
   add_subbytes b bs.buffer 0 bs.position *)

(* let add_channel b ic len =
   if len < 0
    || len > Sys.max_string_length
    then   (* PR#5004 *)
    invalid_arg "Ext_buffer.add_channel";
   if b.position + len > b.length then resize b len;
   really_input ic b.buffer b.position len;
   b.position <- b.position + len *)

let output_buffer oc b = output oc b.buffer 0 b.position

external unsafe_string : bytes -> int -> int -> Digest.t = "caml_md5_string"

let digest b = unsafe_string b.buffer 0 b.position

let rec not_equal_aux (b : bytes) (s : string) i len =
  if i >= len then false
  else
    Bytes.unsafe_get b i <> String.unsafe_get s i
    || not_equal_aux b s (i + 1) len

(** avoid a large copy *)
let not_equal (b : t) (s : string) =
  let b_len = b.position in
  let s_len = String.length s in
  b_len <> s_len || not_equal_aux b.buffer s 0 s_len

(**
   It could be one byte, two bytes, three bytes and four bytes 
   TODO: inline for better performance
*)
let add_int_1 (b : t) (x : int) =
  let c = Char.unsafe_chr (x land 0xff) in
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

let add_int_2 (b : t) (x : int) =
  let c1 = Char.unsafe_chr (x land 0xff) in
  let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
  let pos = b.position in
  if pos + 1 >= b.length then resize b 2;
  let b_buffer = b.buffer in
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  b.position <- pos + 2

let add_int_3 (b : t) (x : int) =
  let c1 = Char.unsafe_chr (x land 0xff) in
  let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
  let c3 = Char.unsafe_chr ((x lsr 16) land 0xff) in
  let pos = b.position in
  if pos + 2 >= b.length then resize b 3;
  let b_buffer = b.buffer in
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  b.position <- pos + 3

let add_int_4 (b : t) (x : int) =
  let c1 = Char.unsafe_chr (x land 0xff) in
  let c2 = Char.unsafe_chr ((x lsr 8) land 0xff) in
  let c3 = Char.unsafe_chr ((x lsr 16) land 0xff) in
  let c4 = Char.unsafe_chr ((x lsr 24) land 0xff) in
  let pos = b.position in
  if pos + 3 >= b.length then resize b 4;
  let b_buffer = b.buffer in
  Bytes.unsafe_set b_buffer pos c1;
  Bytes.unsafe_set b_buffer (pos + 1) c2;
  Bytes.unsafe_set b_buffer (pos + 2) c3;
  Bytes.unsafe_set b_buffer (pos + 3) c4;
  b.position <- pos + 4
