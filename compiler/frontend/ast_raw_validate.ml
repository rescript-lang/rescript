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

let validate_raw_source ~(kind : Js_raw_info.raw_kind) ?is_function ~loc ~offset
    str =
  Bs_flow_ast_utils.check_flow_errors ~loc ~offset
    (match kind with
    | Raw_re | Raw_exp ->
      let ((_loc, expression) as program), errors =
        let open Parser_flow in
        let env = Parser_env.init_env None str in
        do_parse env Parse.expression false
      in
      (if kind = Raw_re then
         match expression with
         | RegExpLiteral _ -> ()
         | _ ->
           Loc_report.raise_errorf ~loc
             "Syntax error: a valid JS regex literal expected");
      (match is_function with
      | Some is_function -> (
        match Classify_function.classify_exp program with
        | Js_function {arity; _} -> is_function := Some arity
        | _ -> ())
      | None -> ());
      errors
    | Raw_program -> snd (Parser_flow.parse_program false None str))

let raw_as_string_exp_exn ~(kind : Js_raw_info.raw_kind) ?is_function
    (x : Ast_payload.t) : Parsetree.expression option =
  let braced =
    match x with
    | PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_braces _}, _)}] -> true
    | _ -> false
  in
  let string_expression =
    match Ast_payload.unwrap_payload_expression x with
    (* TODO also need detect empty phrase case *)
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( ({
                     pexp_desc =
                       Pexp_template {source_segments = [source]; values = []};
                   } as expression),
                  _ );
          };
        ] ->
      let source = String_literal.decode_raw_template_source source.txt in
      Some (source, Bs_flow_ast_utils.flow_deli_offset (Some "js"), expression)
    | PStr
        [
          {
            pstr_desc =
              Pstr_eval
                (({pexp_desc = Pexp_constant constant; _} as expression), _);
            _;
          };
        ] -> (
      match constant with
      | Pconst_string payload when braced ->
        (* A braced payload is an ordinary string expression, so its escapes
           are decoded. *)
        Some (String_literal.string_semantic payload, 0, expression)
      | Pconst_string payload ->
        (* A raw payload is JavaScript source, so the string is taken as
           written. *)
        Some
          ( String_literal.string_source payload,
            Bs_flow_ast_utils.flow_deli_offset (Some "js"),
            expression )
      | _ -> None)
    | _ -> None
  in
  match string_expression with
  | Some (str, offset, ({pexp_loc = loc} as expression)) ->
    validate_raw_source ~kind ?is_function ~loc ~offset str;
    Some
      {
        expression with
        pexp_desc =
          Pexp_constant
            (Pconst_string (String_literal.string_from_semantic str));
      }
  | None -> None
