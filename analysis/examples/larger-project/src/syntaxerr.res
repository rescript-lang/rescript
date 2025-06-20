/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1997 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Auxiliary type for reporting syntax errors */

type error =
  | Unclosed(Location.t, string, Location.t, string)
  | Expecting(Location.t, string)
  | Not_expecting(Location.t, string)
  | Applicative_path(Location.t)
  | Variable_in_scope(Location.t, string)
  | Other(Location.t)
  | Ill_formed_ast(Location.t, string)
  | Invalid_package_type(Location.t, string)

exception Error(error)
exception Escape_error

let prepare_error = x =>
  switch x {
  | Unclosed(opening_loc, opening, closing_loc, closing) =>
    Location.errorf(
      ~loc=closing_loc,
      ~sub=list{Location.errorf(~loc=opening_loc, "This '%s' might be unmatched", opening)},
      ~if_highlight=Printf.sprintf(
        "Syntax error: '%s' expected, \
                           the highlighted '%s' might be unmatched",
        closing,
        opening,
      ),
      "Syntax error: '%s' expected",
      closing,
    )

  | Expecting(loc, nonterm) => Location.errorf(~loc, "Syntax error: %s expected.", nonterm)
  | Not_expecting(loc, nonterm) => Location.errorf(~loc, "Syntax error: %s not expected.", nonterm)
  | Applicative_path(loc) =>
    Location.errorf(
      ~loc,
      "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set.",
    )
  | Variable_in_scope(loc, var) =>
    Location.errorf(
      ~loc,
      "In this scoped type, variable '%s \
         is reserved for the local type %s.",
      var,
      var,
    )
  | Other(loc) => Location.errorf(~loc, "Syntax error")
  | Ill_formed_ast(loc, s) => Location.errorf(~loc, "broken invariant in parsetree: %s", s)
  | Invalid_package_type(loc, s) => Location.errorf(~loc, "invalid package type: %s", s)
  }

let () = Location.register_error_of_exn(x =>
  switch x {
  | Error(err) => Some(prepare_error(err))
  | _ => None
  }
)

let report_error = (ppf, err) => ()

let location_of_error = x =>
  switch x {
  | Unclosed(l, _, _, _)
  | Applicative_path(l)
  | Variable_in_scope(l, _)
  | Other(l)
  | Not_expecting(l, _)
  | Ill_formed_ast(l, _)
  | Invalid_package_type(l, _)
  | Expecting(l, _) => l
  }

let ill_formed_ast = (loc, s) => throw(Error(Ill_formed_ast(loc, s)))

