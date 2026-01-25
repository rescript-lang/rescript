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

(** Helpers to produce Parsetree fragments *)

open Asttypes
open Parsetree

type lid = Longident.t loc
type str = string loc
type loc = Location.t
type attrs = attribute list

(** {1 Default locations} *)

val default_loc : loc ref
(** Default value for all optional location arguments. *)

val with_default_loc : loc -> (unit -> 'a) -> 'a
(** Set the [default_loc] within the scope of the execution
        of the provided function. *)

(** {1 Constants} *)

module Const : sig
  val char : char -> constant
  val string : ?quotation_delimiter:string -> string -> constant
  val integer : ?suffix:char -> string -> constant
  val int : ?suffix:char -> int -> constant
  val int32 : ?suffix:char -> int32 -> constant
  val int64 : ?suffix:char -> int64 -> constant
  val nativeint : ?suffix:char -> nativeint -> constant
  val float : ?suffix:char -> string -> constant
end

(** {1 Core language} *)

(** Type expressions *)
module Typ : sig
  val mk : ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
  val attr : core_type -> attribute -> core_type

  val any : ?loc:loc -> ?attrs:attrs -> unit -> core_type
  val var : ?loc:loc -> ?attrs:attrs -> string -> core_type
  val arrow :
    ?loc:loc -> ?attrs:attrs -> arity:arity -> arg -> core_type -> core_type
  val arrows : ?loc:loc -> ?attrs:attrs -> arg list -> core_type -> core_type
  val tuple : ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
  val constr : ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
  val object_ :
    ?loc:loc -> ?attrs:attrs -> object_field list -> closed_flag -> core_type
  val alias : ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
  val variant :
    ?loc:loc ->
    ?attrs:attrs ->
    row_field list ->
    closed_flag ->
    label list option ->
    core_type
  val poly : ?loc:loc -> ?attrs:attrs -> str list -> core_type -> core_type
  val package :
    ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list -> core_type
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> core_type

  val force_poly : core_type -> core_type

  val varify_constructors : str list -> core_type -> core_type
  (** [varify_constructors newtypes te] is type expression [te], of which
        any of nullary type constructor [tc] is replaced by type variable of
        the same name, if [tc]'s name appears in [newtypes].
        Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
        appears in [newtypes].
        @since 4.05
     *)
end

(** Patterns *)
module Pat : sig
  val mk : ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
  val attr : pattern -> attribute -> pattern

  val any : ?loc:loc -> ?attrs:attrs -> unit -> pattern
  val var : ?loc:loc -> ?attrs:attrs -> str -> pattern
  val alias : ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
  val constant : ?loc:loc -> ?attrs:attrs -> constant -> pattern
  val interval : ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
  val tuple : ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
  val construct : ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
  val variant : ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
  val record :
    ?loc:loc ->
    ?attrs:attrs ->
    pattern record_element list ->
    closed_flag ->
    pattern
  val array : ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
  val or_ : ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
  val constraint_ : ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
  val type_ : ?loc:loc -> ?attrs:attrs -> lid -> pattern
  val unpack : ?loc:loc -> ?attrs:attrs -> str -> pattern
  val open_ : ?loc:loc -> ?attrs:attrs -> lid -> pattern -> pattern
  val exception_ : ?loc:loc -> ?attrs:attrs -> pattern -> pattern
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> pattern
end

(** Expressions *)
module Exp : sig
  val mk : ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
  val attr : expression -> attribute -> expression

  val ident : ?loc:loc -> ?attrs:attrs -> lid -> expression
  val constant : ?loc:loc -> ?attrs:attrs -> constant -> expression
  val let_ :
    ?loc:loc ->
    ?attrs:attrs ->
    rec_flag ->
    value_binding list ->
    expression ->
    expression
  val fun_ :
    ?loc:loc ->
    ?attrs:attrs ->
    ?async:bool ->
    arity:int option ->
    arg_label ->
    expression option ->
    pattern ->
    expression ->
    expression
  val apply :
    ?loc:loc ->
    ?attrs:attrs ->
    ?partial:bool ->
    ?transformed_jsx:bool ->
    expression ->
    (arg_label * expression) list ->
    expression
  val match_ : ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
  val try_ : ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
  val tuple : ?loc:loc -> ?attrs:attrs -> expression list -> expression
  val construct :
    ?loc:loc -> ?attrs:attrs -> lid -> expression option -> expression
  val variant :
    ?loc:loc -> ?attrs:attrs -> label -> expression option -> expression
  val record :
    ?loc:loc ->
    ?attrs:attrs ->
    expression record_element list ->
    expression option ->
    expression
  val field : ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
  val setfield :
    ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression -> expression
  val array : ?loc:loc -> ?attrs:attrs -> expression list -> expression
  val ifthenelse :
    ?loc:loc ->
    ?attrs:attrs ->
    expression ->
    expression ->
    expression option ->
    expression
  val sequence :
    ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
  val while_ :
    ?loc:loc -> ?attrs:attrs -> expression -> expression -> expression
  val for_ :
    ?loc:loc ->
    ?attrs:attrs ->
    pattern ->
    expression ->
    expression ->
    direction_flag ->
    expression ->
    expression
  val coerce : ?loc:loc -> ?attrs:attrs -> expression -> core_type -> expression
  val constraint_ :
    ?loc:loc -> ?attrs:attrs -> expression -> core_type -> expression
  val send : ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
  val letmodule :
    ?loc:loc -> ?attrs:attrs -> str -> module_expr -> expression -> expression
  val letexception :
    ?loc:loc ->
    ?attrs:attrs ->
    extension_constructor ->
    expression ->
    expression
  val assert_ : ?loc:loc -> ?attrs:attrs -> expression -> expression
  val newtype : ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
  val pack : ?loc:loc -> ?attrs:attrs -> module_expr -> expression
  val open_ :
    ?loc:loc -> ?attrs:attrs -> override_flag -> lid -> expression -> expression
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> expression
  val jsx_fragment :
    ?loc:loc ->
    ?attrs:attrs ->
    Lexing.position ->
    Parsetree.jsx_children ->
    Lexing.position ->
    expression
  val jsx_unary_element :
    ?loc:loc ->
    ?attrs:attrs ->
    Parsetree.jsx_tag_name Location.loc ->
    Parsetree.jsx_props ->
    expression
  val jsx_container_element :
    ?loc:loc ->
    ?attrs:attrs ->
    Parsetree.jsx_tag_name Location.loc ->
    Parsetree.jsx_props ->
    Lexing.position ->
    Parsetree.jsx_children ->
    Parsetree.jsx_closing_container_tag option ->
    expression

  val case :
    ?bar:Lexing.position -> pattern -> ?guard:expression -> expression -> case
  val await : ?loc:loc -> ?attrs:attrs -> expression -> expression

  val make_list_expression :
    Location.t -> expression list -> expression option -> expression
end

(** Value declarations *)
module Val : sig
  val mk :
    ?loc:loc ->
    ?attrs:attrs ->
    ?prim:string list ->
    str ->
    core_type ->
    value_description
end

(** Type declarations *)
module Type : sig
  val mk :
    ?loc:loc ->
    ?attrs:attrs ->
    ?params:(core_type * variance) list ->
    ?cstrs:(core_type * core_type * loc) list ->
    ?kind:type_kind ->
    ?priv:private_flag ->
    ?manifest:core_type ->
    str ->
    type_declaration

  val constructor :
    ?loc:loc ->
    ?attrs:attrs ->
    ?args:constructor_arguments ->
    ?res:core_type ->
    str ->
    constructor_declaration
  val field :
    ?loc:loc ->
    ?attrs:attrs ->
    ?mut:mutable_flag ->
    ?optional:bool ->
    str ->
    core_type ->
    label_declaration
end

(** Type extensions *)
module Te : sig
  val mk :
    ?attrs:attrs ->
    ?params:(core_type * variance) list ->
    ?priv:private_flag ->
    lid ->
    extension_constructor list ->
    type_extension

  val constructor :
    ?loc:loc ->
    ?attrs:attrs ->
    str ->
    extension_constructor_kind ->
    extension_constructor

  val decl :
    ?loc:loc ->
    ?attrs:attrs ->
    ?args:constructor_arguments ->
    ?res:core_type ->
    str ->
    extension_constructor
  val rebind : ?loc:loc -> ?attrs:attrs -> str -> lid -> extension_constructor
end

module Jsx : sig
  val string_of_jsx_tag_name : Parsetree.jsx_tag_name -> string
  val longident_of_jsx_tag_name : Parsetree.jsx_tag_name -> Longident.t
end

(** {1 Module language} *)

(** Module type expressions *)
module Mty : sig
  val mk : ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
  val attr : module_type -> attribute -> module_type

  val ident : ?loc:loc -> ?attrs:attrs -> lid -> module_type
  val alias : ?loc:loc -> ?attrs:attrs -> lid -> module_type
  val signature : ?loc:loc -> ?attrs:attrs -> signature -> module_type
  val functor_ :
    ?loc:loc ->
    ?attrs:attrs ->
    str ->
    module_type option ->
    module_type ->
    module_type
  val with_ :
    ?loc:loc ->
    ?attrs:attrs ->
    module_type ->
    with_constraint list ->
    module_type
  val typeof_ : ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> module_type
end

(** Module expressions *)
module Mod : sig
  val mk : ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
  val attr : module_expr -> attribute -> module_expr

  val ident : ?loc:loc -> ?attrs:attrs -> lid -> module_expr
  val structure : ?loc:loc -> ?attrs:attrs -> structure -> module_expr
  val functor_ :
    ?loc:loc ->
    ?attrs:attrs ->
    str ->
    module_type option ->
    module_expr ->
    module_expr
  val apply :
    ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr -> module_expr
  val constraint_ :
    ?loc:loc -> ?attrs:attrs -> module_expr -> module_type -> module_expr
  val unpack : ?loc:loc -> ?attrs:attrs -> expression -> module_expr
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> module_expr
end

(** Signature items *)
module Sig : sig
  val mk : ?loc:loc -> signature_item_desc -> signature_item

  val value : ?loc:loc -> value_description -> signature_item
  val type_ : ?loc:loc -> rec_flag -> type_declaration list -> signature_item
  val type_extension : ?loc:loc -> type_extension -> signature_item
  val exception_ : ?loc:loc -> extension_constructor -> signature_item
  val module_ : ?loc:loc -> module_declaration -> signature_item
  val rec_module : ?loc:loc -> module_declaration list -> signature_item
  val modtype : ?loc:loc -> module_type_declaration -> signature_item
  val open_ : ?loc:loc -> open_description -> signature_item
  val include_ : ?loc:loc -> include_description -> signature_item
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> signature_item
  val attribute : ?loc:loc -> attribute -> signature_item
end

(** Structure items *)
module Str : sig
  val mk : ?loc:loc -> structure_item_desc -> structure_item

  val eval : ?loc:loc -> ?attrs:attributes -> expression -> structure_item
  val value : ?loc:loc -> rec_flag -> value_binding list -> structure_item
  val primitive : ?loc:loc -> value_description -> structure_item
  val type_ : ?loc:loc -> rec_flag -> type_declaration list -> structure_item
  val type_extension : ?loc:loc -> type_extension -> structure_item
  val exception_ : ?loc:loc -> extension_constructor -> structure_item
  val module_ : ?loc:loc -> module_binding -> structure_item
  val rec_module : ?loc:loc -> module_binding list -> structure_item
  val modtype : ?loc:loc -> module_type_declaration -> structure_item
  val open_ : ?loc:loc -> open_description -> structure_item
  val include_ : ?loc:loc -> include_declaration -> structure_item
  val extension : ?loc:loc -> ?attrs:attrs -> extension -> structure_item
  val attribute : ?loc:loc -> attribute -> structure_item
end

(** Module declarations *)
module Md : sig
  val mk : ?loc:loc -> ?attrs:attrs -> str -> module_type -> module_declaration
end

(** Module type declarations *)
module Mtd : sig
  val mk :
    ?loc:loc ->
    ?attrs:attrs ->
    ?typ:module_type ->
    str ->
    module_type_declaration
end

(** Module bindings *)
module Mb : sig
  val mk : ?loc:loc -> ?attrs:attrs -> str -> module_expr -> module_binding
end

(** Opens *)
module Opn : sig
  val mk :
    ?loc:loc ->
    ?attrs:attrs ->
    ?override:override_flag ->
    lid ->
    open_description
end

(** Includes *)
module Incl : sig
  val mk : ?loc:loc -> ?attrs:attrs -> 'a -> 'a include_infos
end

(** Value bindings *)
module Vb : sig
  val mk : ?loc:loc -> ?attrs:attrs -> pattern -> expression -> value_binding
end
