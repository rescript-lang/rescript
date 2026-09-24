(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
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

(** Browser is not set via command line only for internal use *)

type jsx_version = Jsx_v4
type jsx_module = React | Generic of {module_name: string}
type source_map = No_source_map | Linked | Inline | Hidden

let tool_name = "ReScript"

type t = {
  no_version_header: bool ref;
  directives: string list ref;
  cross_module_inline: bool ref;
  debug_ir: bool ref;
  check_lam: bool ref;
  no_builtin_ppx: bool ref;
  check_div_by_zero: bool ref;
  syntax_only: bool ref;
  binary_ast: bool ref;
  test_ast_conversion: bool ref;
  debug: bool ref;
  cmi_only: bool ref;
  cmj_only: bool ref;
  force_cmi: bool ref;
  force_cmj: bool ref;
  jsx_version: jsx_version option ref;
  jsx_module: jsx_module ref;
  jsx_preserve: bool ref;
  js_stdout: bool ref;
  source_map: source_map ref;
  source_map_sources_content: bool ref;
  source_map_root: string ref;
  all_module_aliases: bool ref;
  no_stdlib: bool ref;
  no_export: bool ref;
  as_pp: bool ref;
  self_stack: string Stack.t;
}

let create () =
  {
    no_version_header = ref false;
    directives = ref [];
    cross_module_inline = ref false;
    debug_ir = ref false;
    check_lam = ref false;
    no_builtin_ppx = ref false;
    check_div_by_zero = ref true;
    syntax_only = ref false;
    binary_ast = ref false;
    test_ast_conversion = ref false;
    debug = ref false;
    cmi_only = ref false;
    cmj_only = ref false;
    force_cmi = ref false;
    force_cmj = ref false;
    jsx_version = ref None;
    jsx_module = ref React;
    jsx_preserve = ref false;
    js_stdout = ref true;
    source_map = ref No_source_map;
    source_map_sources_content = ref false;
    source_map_root = ref "";
    all_module_aliases = ref false;
    no_stdlib = ref false;
    no_export = ref false;
    as_pp = ref false;
    self_stack = Stack.create ();
  }

(* Every option, including the stack used by the uncurry transform, belongs to
   the request's domain. *)
let key = Domain.DLS.new_key create
let current () = Domain.DLS.get key

let with_fresh action =
  let previous = current () in
  Domain.DLS.set key (create ());
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)
let int_of_jsx_version = function
  | Jsx_v4 -> 4

let string_of_jsx_module = function
  | React -> "react"
  | Generic {module_name} -> module_name

let jsx_version_of_int = function
  | 4 -> Some Jsx_v4
  | _ -> None

let jsx_module_of_string = function
  | "react" -> React
  | module_name -> Generic {module_name}

let reset () =
  let state = current () in
  state.no_version_header := false;
  state.directives := [];
  state.cross_module_inline := false;
  state.debug_ir := false;
  state.check_lam := false;
  state.no_builtin_ppx := false;
  state.check_div_by_zero := true;
  state.syntax_only := false;
  state.binary_ast := false;
  state.test_ast_conversion := false;
  state.debug := false;
  state.cmi_only := false;
  state.cmj_only := false;
  state.force_cmi := false;
  state.force_cmj := false;
  state.jsx_version := None;
  state.jsx_module := React;
  state.jsx_preserve := false;
  state.js_stdout := true;
  state.source_map := No_source_map;
  state.source_map_sources_content := false;
  state.source_map_root := "";
  state.all_module_aliases := false;
  state.no_stdlib := false;
  state.no_export := false;
  state.as_pp := false;
  Stack.clear state.self_stack
