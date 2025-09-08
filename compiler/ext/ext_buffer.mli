(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Extensible buffers.

    This module implements buffers that automatically expand
    as necessary.  It provides accumulative concatenation of strings
    in quasi-linear time (instead of quadratic time when strings are
    concatenated pairwise).
*)

(* ReScript customization: customized for efficient digest *)

type t
(** The abstract type of buffers. *)

val create : int -> t
(** [create n] returns a fresh buffer, initially empty.
    The [n] parameter is the initial size of the internal byte sequence
    that holds the buffer contents. That byte sequence is automatically
    reallocated when more than [n] characters are stored in the buffer,
    but shrinks back to [n] characters when [reset] is called.
    For best performance, [n] should be of the same order of magnitude
    as the number of characters that are expected to be stored in
    the buffer (for instance, 80 for a buffer that holds one output
    line).  Nothing bad will happen if the buffer grows beyond that
    limit, however. In doubt, take [n = 16] for instance.
    If [n] is not between 1 and {!Sys.max_string_length}, it will
    be clipped to that interval. *)

val contents : t -> string
(** Return a copy of the current contents of the buffer.
    The buffer itself is unchanged. *)

val length : t -> int
(** Return the number of characters currently contained in the buffer. *)

val is_empty : t -> bool

val clear : t -> unit
(** Empty the buffer. *)

val add_char : t -> char -> unit
(** [add_char b c] appends the character [c] at the end of the buffer [b]. *)

val add_string : t -> string -> unit
(** [add_string b s] appends the string [s] at the end of the buffer [b]. *)

(* val add_bytes : t -> bytes -> unit *)
(** [add_string b s] appends the string [s] at the end of the buffer [b].
    @since 4.02 *)

(* val add_substring : t -> string -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in string [s] and appends them at the end of the buffer [b]. *)

(* val add_subbytes : t -> bytes -> int -> int -> unit *)
(** [add_substring b s ofs len] takes [len] characters from offset
    [ofs] in byte sequence [s] and appends them at the end of the buffer [b].
    @since 4.02 *)

(* val add_buffer : t -> t -> unit *)
(** [add_buffer b1 b2] appends the current contents of buffer [b2]
    at the end of buffer [b1].  [b2] is not modified. *)

(* val add_channel : t -> in_channel -> int -> unit *)
(** [add_channel b ic n] reads exactly [n] character from the
    input channel [ic] and stores them at the end of buffer [b].
    Raise [End_of_file] if the channel contains fewer than [n]
    characters. *)

val output_buffer : out_channel -> t -> unit
(** [output_buffer oc b] writes the current contents of buffer [b]
    on the output channel [oc]. *)

val digest : t -> Digest.t

val not_equal : t -> string -> bool

val add_int_1 : t -> int -> unit

val add_int_2 : t -> int -> unit

val add_int_3 : t -> int -> unit

val add_int_4 : t -> int -> unit

val add_string_char : t -> string -> char -> unit

val add_ninja_prefix_var : t -> string -> unit

val add_char_string : t -> char -> string -> unit
