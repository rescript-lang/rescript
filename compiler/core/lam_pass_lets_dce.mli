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
(* Adapted for Javascript backend: Hongbo Zhang                        *)

val simplify_lets : Lam.t -> Lam.t
(**
   This pass would do beta reduction, and dead code elimination (adapted from compiler's built-in [Simplif] module )

   1. beta reduction -> Llet (Strict )

   2. The global table [occ] associates to each let-bound identifier
   the number of its uses (as a reference):
   - 0 if never used
   - 1 if used exactly once in and *not under a lambda or within a loop
   - > 1 if used several times or under a lambda or within a loop.

   The local table [bv] associates to each locally-let-bound variable
   its reference count, as above.  [bv] is enriched at let bindings
   but emptied when crossing lambdas and loops. 

   For this pass, when it' used under a lambda or within a loop, we don't do anything,
   in theory, we can still do something if it's pure but we are conservative here.

   [bv] is used to help caculate [occ] it is not useful outside

*)
