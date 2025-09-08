(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
(* Adapted for Javascript backend : Hongbo Zhang,  *)

type used_info = {
  mutable times: int;
  mutable captured: bool;
      (* captured in functon or loop,
         inline in such cases should be careful
         1. can not inline mutable values
         2. avoid re-computation
      *)
}

type occ_tbl = used_info Hash_ident.t

val dummy_info : unit -> used_info

val collect_occurs : Lam.t -> occ_tbl

val pp_occ_tbl : Format.formatter -> occ_tbl -> unit
