(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2016 - Hongbo Zhang, Authors of ReScript
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let setup_env () =
  Env.Persistent_signature.load := Bs_cmi_load.load_cmi;
  Matching.make_test_sequence_variant_constant :=
    Polyvar_pattern_match.make_test_sequence_variant_constant;
  Matching.call_switcher_variant_constant :=
    Polyvar_pattern_match.call_switcher_variant_constant;
  Matching.call_switcher_variant_constr :=
    Polyvar_pattern_match.call_switcher_variant_constr;
  Ctype.variant_is_subtype := Matching_polyfill.variant_is_subtype;
  Clflags.dump_location := false;
  Parmatch.print_res_pat := Pattern_printer.print_pattern;
  (* default true
     otherwise [bsc -I sc src/hello.ml ] will include current directory to search path
  *)
  Clflags.debug := true;
  Clflags.binary_annotations := true;
  (* Turn on [-no-alias-deps] by default -- double check *)
  Oprint.out_ident := Outcome_printer_ns.out_ident;
  Builtin_attributes.check_bs_attributes_inclusion :=
    Record_attributes_check.check_bs_attributes_inclusion;
  Builtin_attributes.check_duplicated_labels :=
    Record_attributes_check.check_duplicated_labels;
  Matching.names_from_construct_pattern :=
    Matching_polyfill.names_from_construct_pattern;

  Printtyp.print_res_poly_identifier := Res_printer.polyvar_ident_to_string
(*; Switch.cut := 100*)
(* tweakable but not very useful *)

let () = at_exit (fun _ -> Format.pp_print_flush Format.err_formatter ())
