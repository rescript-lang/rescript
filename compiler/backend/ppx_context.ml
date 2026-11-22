(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Alain Frisch, LexiFi                           *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The ocaml.ppx.context attribute passed to external PPXes. *)

module Context = struct
  open Longident
  open Asttypes
  open Ast_helper

  let lid name = {txt = Lident name; loc = Location.none}

  let make_string x =
    Exp.constant
      ~attrs:[(Location.mknoloc "_res.ppx_context_string", Parsetree.PStr [])]
      (Const.string x)

  let make_bool x =
    if x then Exp.construct (lid "true") (Location.mknoloc [])
    else Exp.construct (lid "false") (Location.mknoloc [])

  let rec make_list f lst =
    match lst with
    | x :: rest ->
      Exp.construct (lid "::") (Location.mknoloc [f x; make_list f rest])
    | [] -> Exp.construct (lid "[]") (Location.mknoloc [])

  let mk fields =
    ( {txt = "ocaml.ppx.context"; loc = Location.none},
      Parsetree.PStr [Str.eval (Exp.record fields None)] )

  let make ~tool_name () =
    let fields : Parsetree.expression Parsetree.record_element list =
      [
        {lid = lid "tool_name"; x = make_string tool_name; opt = false};
        {
          lid = lid "include_dirs";
          x = make_list make_string !((Clflags.current ()).include_dirs);
          opt = false;
        };
        {
          lid = lid "load_path";
          x = make_list make_string (Config.get_load_path ());
          opt = false;
        };
        {
          lid = lid "open_modules";
          x = make_list make_string !((Clflags.current ()).open_modules);
          opt = false;
        };
        {
          lid = lid "debug";
          x = make_bool !((Clflags.current ()).debug);
          opt = false;
        };
        (* The compiler doesn't read back the cookies a PPX sets. *)
        {lid = lid "cookies"; x = make_list Fun.id []; opt = false};
      ]
    in
    mk fields
end

let ppx_context = Context.make

let drop_str = function
  | {
      Parsetree.pstr_desc =
        Pstr_attribute ({Location.txt = "ocaml.ppx.context"}, _);
    }
    :: items ->
    items
  | items -> items

let drop_sig = function
  | {
      Parsetree.psig_desc =
        Psig_attribute ({Location.txt = "ocaml.ppx.context"}, _);
    }
    :: items ->
    items
  | items -> items

let add_str ~tool_name ast =
  Ast_helper.Str.attribute (ppx_context ~tool_name ()) :: ast

let add_sig ~tool_name ast =
  Ast_helper.Sig.attribute (ppx_context ~tool_name ()) :: ast
