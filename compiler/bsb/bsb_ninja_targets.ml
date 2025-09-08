(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
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

let oc_list xs oc =
  Ext_list.iter xs (fun s ->
      output_string oc Ext_string.single_space;
      output_string oc s)

let output_build ~outputs ~inputs ~rule oc =
  let rule = Bsb_ninja_rule.get_name rule oc in
  (* Trigger building if not used *)
  output_string oc "o";
  oc_list outputs oc;
  output_string oc " : ";
  output_string oc rule;
  oc_list inputs oc;
  output_string oc "\n"

let phony ?(order_only_deps = []) ~inputs ~output oc =
  output_string oc "o ";
  output_string oc output;
  output_string oc " : ";
  output_string oc "phony";
  oc_list inputs oc;
  if order_only_deps <> [] then (
    output_string oc " ||";
    oc_list order_only_deps oc);
  output_string oc "\n"

let output_finger key value oc =
  output_string oc key;
  output_string oc " := ";
  output_string oc value;
  output_string oc "\n"
