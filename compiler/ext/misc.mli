(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Miscellaneous useful types and functions *)

val fatal_error : string -> 'a
exception Fatal_error

val try_finally : (unit -> 'a) -> (unit -> unit) -> 'a

val map_end : ('a -> 'b) -> 'a list -> 'b list -> 'b list
(* [map_end f l t] is [map f l @ t], just more efficient. *)

val replicate_list : 'a -> int -> 'a list
(* [replicate_list elem n] is the list with [n] elements
   all identical to [elem]. *)

val may : ('a -> unit) -> 'a option -> unit
val may_map : ('a -> 'b) -> 'a option -> 'b option

type ref_and_value = R : 'a ref * 'a -> ref_and_value

val protect_refs : ref_and_value list -> (unit -> 'a) -> 'a
(** [protect_refs l f] temporarily sets [r] to [v] for each [R (r, v)] in [l]
    while executing [f]. The previous contents of the references is restored
    even if [f] raises an exception. *)

val find_in_path_uncap : string list -> string -> string
(* Same, but search also for uncapitalized name, i.e.
   if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
   to match. *)

val remove_file : string -> unit
(* Delete the given file if it exists. Never raise an error. *)

val expand_directory : string -> string -> string
(* [expand_directory alt file] eventually expands a [+] at the
   beginning of file into [alt] (an alternate root directory) *)

val create_hashtable : ('a * 'b) array -> ('a, 'b) Hashtbl.t
(* Create a hashtable of the given size and fills it with the
   given bindings. *)

val output_to_bin_file_directly : string -> (string -> out_channel -> 'a) -> 'a

module Int_literal_converter : sig
  val int : string -> int
end

val get_ref : 'a list ref -> 'a list
(* [get_ref lr] returns the content of the list reference [lr] and reset
   its content to the empty list. *)

val edit_distance : string -> string -> int -> int option
(** [edit_distance a b cutoff] computes the edit distance between
    strings [a] and [b]. To help efficiency, it uses a cutoff: if the
    distance [d] is smaller than [cutoff], it returns [Some d], else
    [None].

    The distance algorithm currently used is Damerau-Levenshtein: it
    computes the number of insertion, deletion, substitution of
    letters, or swapping of adjacent letters to go from one word to the
    other. The particular algorithm may change in the future.
*)

val spellcheck : string list -> string -> string list
(** [spellcheck env name] takes a list of names [env] that exist in
    the current environment and an erroneous [name], and returns a
    list of suggestions taken from [env], that are close enough to
    [name] that it may be a typo for one of them. *)

val did_you_mean : Format.formatter -> (unit -> string list) -> unit
(** [did_you_mean ppf get_choices] hints that the user may have meant
    one of the option returned by calling [get_choices]. It does nothing
    if the returned list is empty.

    The [unit -> ...] thunking is meant to delay any potentially-slow
    computation (typically computing edit-distance with many things
    from the current environment) to when the hint message is to be
    printed. You should print an understandable error message before
    calling [did_you_mean], so that users get a clear notification of
    the failure even if producing the hint is slow.
*)

module String_set : Set.S with type elt = string
module String_map : Map.S with type key = string
(* TODO: replace all custom instantiations of StringSet/StringMap in various
   compiler modules with this one. *)

(* Color handling *)
module Color : sig
  type color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

  type style =
    | FG of color (* foreground *)
    | BG of color (* background *)
    | Bold
    | Reset
    | Dim

  val ansi_of_style_l : style list -> string
  (* ANSI escape sequence for the given style *)

  type styles = {error: style list; warning: style list; loc: style list}

  val default_styles : styles
  type setting = Auto | Always | Never

  val setup : setting option -> unit
  (* [setup opt] will enable or disable color handling on standard formatters
     according to the value of color setting [opt].
     Only the first call to this function has an effect. *)

  val set_color_tag_handling : Format.formatter -> unit
  (* adds functions to support color tags to the given formatter. *)
end

val normalise_eol : string -> string
(** [normalise_eol s] returns a fresh copy of [s] with any '\r' characters
   removed. Intended for pre-processing text which will subsequently be printed
   on a channel which performs EOL transformations (i.e. Windows) *)
