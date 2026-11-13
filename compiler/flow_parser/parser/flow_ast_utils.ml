(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast
module I = Identifier

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string 

type 'loc source = 'loc * string 

let pattern_has_type_annotation =
  let open Pattern in
  function
  | (_, Identifier { Identifier.annot = Type.Available _; _ }) -> true
  | (_, Object { Object.annot = Type.Available _; _ }) -> true
  | (_, Array { Array.annot = Type.Available _; _ }) -> true
  | _ -> false

let name_of_ident (_, { Identifier.name; comments = _ }) = name

let ident_of_source ?comments (loc, name) = (loc, { Identifier.name; comments })

let mk_comments ~leading ~trailing a = { Syntax.leading; trailing; internal = a }

let mk_comments_opt ?(leading = []) ?(trailing = []) () =
  match (leading, trailing) with
  | ([], []) -> None
  | (_, _) -> Some (mk_comments ~leading ~trailing ())

let mk_comments_with_internal_opt ?(leading = []) ?(trailing = []) ~internal () =
  match (leading, trailing, internal) with
  | ([], [], []) -> None
  | _ -> Some (mk_comments ~leading ~trailing internal)

let merge_comments ~inner ~outer =
  let open Syntax in
  match (inner, outer) with
  | (None, c)
  | (c, None) ->
    c
  | (Some inner, Some outer) ->
    mk_comments_opt
      ~leading:(outer.leading @ inner.leading)
      ~trailing:(inner.trailing @ outer.trailing)
      ()

let merge_comments_with_internal ~inner ~outer =
  match (inner, outer) with
  | (inner, None) -> inner
  | (None, Some { Syntax.leading; trailing; _ }) ->
    mk_comments_with_internal_opt ~leading ~trailing ~internal:[] ()
  | ( Some { Syntax.leading = inner_leading; trailing = inner_trailing; internal },
      Some { Syntax.leading = outer_leading; trailing = outer_trailing; _ }
    ) ->
    mk_comments_with_internal_opt
      ~leading:(outer_leading @ inner_leading)
      ~trailing:(inner_trailing @ outer_trailing)
      ~internal
      ()


let effective_export_kind ~statement_export_kind specifier_export_kind =
  match specifier_export_kind with
  | Statement.ExportType -> Statement.ExportType
  | Statement.ExportValue -> statement_export_kind

let string_of_enum_explicit_type =
  let open Statement.EnumDeclaration in
  function
  | Boolean -> "boolean"
  | Number -> "number"
  | String -> "string"
  | Symbol -> "symbol"
  | BigInt -> "bigint"

let string_of_enum_member_name =
  let open Statement.EnumDeclaration in
  function
  | Identifier (_, { I.name; _ }) -> name
  | StringLiteral (_, { StringLiteral.value; _ }) -> value

