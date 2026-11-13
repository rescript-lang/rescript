(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc binding = 'loc * string

type 'loc ident = 'loc * string 

type 'loc source = 'loc * string 

val pattern_has_type_annotation : ('m, 't) Flow_ast.Pattern.t -> bool

val name_of_ident : ('loc, 'a) Flow_ast.Identifier.t -> string

val ident_of_source :
  ?comments:('a, unit) Flow_ast.Syntax.t -> 'a source -> ('a, 'a) Flow_ast.Identifier.t

val mk_comments_opt :
  ?leading:'loc Flow_ast.Comment.t list ->
  ?trailing:'loc Flow_ast.Comment.t list ->
  unit ->
  ('loc, unit) Flow_ast.Syntax.t option

val mk_comments_with_internal_opt :
  ?leading:'loc Flow_ast.Comment.t list ->
  ?trailing:'loc Flow_ast.Comment.t list ->
  internal:'loc Flow_ast.Comment.t list ->
  unit ->
  ('loc, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option

val merge_comments :
  inner:('M, unit) Flow_ast.Syntax.t option ->
  outer:('M, unit) Flow_ast.Syntax.t option ->
  ('M, unit) Flow_ast.Syntax.t option

val merge_comments_with_internal :
  inner:('M, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option ->
  outer:('M, 'a) Flow_ast.Syntax.t option ->
  ('M, 'loc Flow_ast.Comment.t list) Flow_ast.Syntax.t option


val effective_export_kind :
  statement_export_kind:Flow_ast.Statement.export_kind ->
  Flow_ast.Statement.export_kind ->
  Flow_ast.Statement.export_kind

val string_of_enum_explicit_type : Flow_ast.Statement.EnumDeclaration.explicit_type -> string

val string_of_enum_member_name : 'M Flow_ast.Statement.EnumDeclaration.member_name -> string

