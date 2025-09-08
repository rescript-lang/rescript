open P

@@ocaml.text(
  /* ************************************************************************ */
  /*  */
  /* OCaml */
  /*  */
  /* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
  /*  */
  /* Copyright 1996 Institut National de Recherche en Informatique et */
  /* en Automatique. */
  /*  */
  /* All rights reserved.  This file is distributed under the terms of */
  /* the GNU Lesser General Public License version 2.1, with the */
  /* special exception on linking described in the file LICENSE. */
  /*  */
  /* ************************************************************************ */

  " Auxiliary AST types used by parsetree and typedtree. "
)

type constant =
  | Const_int(int)
  | Const_char(char)
  | Const_string(string, option<string>)
  | Const_float(string)
  | Const_int32(int32)
  | Const_int64(int64)
  | Const_nativeint(nativeint)

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

/* Order matters, used in polymorphic comparison */
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type arg_label =
  | Nolabel
  | Labelled(string) /* label:T -> ... */
  | Optional(string) /* ?label:T -> ... */

type loc<'a> = Location.loc<'a> = {
  txt: 'a,
  loc: Location.t,
}

type variance =
  | Covariant
  | Contravariant
  | Invariant

