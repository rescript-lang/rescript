(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ident.create
let ident_create_predef_exn = wrap Ident.create_predef_exn

let ident_int = ident_create "int"

and ident_char = ident_create "char"

and ident_float = ident_create "float"

and ident_bool = ident_create "bool"

and ident_unit = ident_create "unit"

and ident_exn = ident_create "exn"

and ident_array = ident_create "array"

and ident_list = ident_create "list"

and ident_option = ident_create "option"

and ident_result = ident_create "result"

and ident_dict = ident_create "dict"

and ident_bigint = ident_create "bigint"

and ident_lazy_t = ident_create "lazy_t"

and ident_string = ident_create "string"

and ident_extension_constructor = ident_create "extension_constructor"

and ident_unknown = ident_create "unknown"

and ident_promise = ident_create "promise"

and ident_uncurried = ident_create "function$"

type test = For_sure_yes | For_sure_no | NA

let type_is_builtin_path_but_option (p : Path.t) : test =
  match p with
  | Pident {stamp} when stamp = ident_option.stamp -> For_sure_no
  | Pident {stamp} when stamp = ident_unit.stamp -> For_sure_no
  | Pident {stamp}
    when stamp >= ident_int.stamp && stamp <= ident_uncurried.stamp ->
    For_sure_yes
  | _ -> NA

let path_int = Pident ident_int

and path_char = Pident ident_char

and path_float = Pident ident_float

and path_bool = Pident ident_bool

and path_unit = Pident ident_unit

and path_exn = Pident ident_exn

and path_array = Pident ident_array

and path_list = Pident ident_list

and path_option = Pident ident_option

and path_result = Pident ident_result

and path_dict = Pident ident_dict

and path_bigint = Pident ident_bigint

and path_lazy_t = Pident ident_lazy_t

and path_string = Pident ident_string

and path_unkonwn = Pident ident_unknown

and path_extension_constructor = Pident ident_extension_constructor

and path_promise = Pident ident_promise

and path_uncurried = Pident ident_uncurried

let type_int = newgenty (Tconstr (path_int, [], ref Mnil))

and type_char = newgenty (Tconstr (path_char, [], ref Mnil))

and type_float = newgenty (Tconstr (path_float, [], ref Mnil))

and type_bool = newgenty (Tconstr (path_bool, [], ref Mnil))

and type_unit = newgenty (Tconstr (path_unit, [], ref Mnil))

and type_exn = newgenty (Tconstr (path_exn, [], ref Mnil))

and type_array t = newgenty (Tconstr (path_array, [t], ref Mnil))

and type_list t = newgenty (Tconstr (path_list, [t], ref Mnil))

and type_option t = newgenty (Tconstr (path_option, [t], ref Mnil))

and type_result t1 t2 = newgenty (Tconstr (path_result, [t1; t2], ref Mnil))

and type_dict t = newgenty (Tconstr (path_dict, [t], ref Mnil))

and type_bigint = newgenty (Tconstr (path_bigint, [], ref Mnil))

and type_lazy_t t = newgenty (Tconstr (path_lazy_t, [t], ref Mnil))

and type_string = newgenty (Tconstr (path_string, [], ref Mnil))

and type_unknown = newgenty (Tconstr (path_unkonwn, [], ref Mnil))

and type_extension_constructor =
  newgenty (Tconstr (path_extension_constructor, [], ref Mnil))

let ident_match_failure = ident_create_predef_exn "Match_failure"

and ident_invalid_argument = ident_create_predef_exn "Invalid_argument"

and ident_failure = ident_create_predef_exn "Failure"

and ident_ok = ident_create_predef_exn "Ok"

and ident_error = ident_create_predef_exn "Error"

and ident_dict_magic_field_name =
  ident_create Dict_type_helpers.dict_magic_field_name

and ident_js_error = ident_create_predef_exn "JsError"

and ident_not_found = ident_create_predef_exn "Not_found"

and ident_end_of_file = ident_create_predef_exn "End_of_file"

and ident_division_by_zero = ident_create_predef_exn "Division_by_zero"

and ident_assert_failure = ident_create_predef_exn "Assert_failure"

and ident_undefined_recursive_module =
  ident_create_predef_exn "Undefined_recursive_module"

let all_predef_exns =
  [
    ident_match_failure;
    ident_invalid_argument;
    ident_failure;
    ident_js_error;
    ident_not_found;
    ident_end_of_file;
    ident_division_by_zero;
    ident_assert_failure;
    ident_undefined_recursive_module;
  ]

let path_match_failure = Pident ident_match_failure

and path_assert_failure = Pident ident_assert_failure

and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let decl_abstr =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_loc = Location.none;
    type_private = Asttypes.Public;
    type_manifest = None;
    type_variance = [];
    type_newtype_level = None;
    type_attributes = [];
    type_immediate = false;
    type_unboxed = unboxed_false_default_false;
    type_inlined_types = [];
  }

let decl_abstr_imm = {decl_abstr with type_immediate = true}

let cstr id args =
  {
    cd_id = id;
    cd_args = Cstr_tuple args;
    cd_res = None;
    cd_loc = Location.none;
    cd_attributes = [];
  }

let ident_false = ident_create "false"

and ident_true = ident_create "true"

and ident_void = ident_create "()"

and ident_nil = ident_create "[]"

and ident_cons = ident_create "::"

and ident_none = ident_create "None"

and ident_some = ident_create "Some"

and ident_ctor_unknown = ident_create "Unknown"

and ident_ctor_uncurried = ident_create "Function$"

let common_initial_env add_type add_extension empty_env =
  let decl_bool =
    {
      decl_abstr with
      type_kind = Type_variant [cstr ident_false []; cstr ident_true []];
      type_immediate = true;
    }
  and decl_unit =
    {
      decl_abstr with
      type_kind = Type_variant [cstr ident_void []];
      type_immediate = true;
    }
  and decl_exn = {decl_abstr with type_kind = Type_open}
  and decl_array =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar];
      type_arity = 1;
      type_variance = [Variance.full];
    }
  and decl_list =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar];
      type_arity = 1;
      type_kind =
        Type_variant [cstr ident_nil []; cstr ident_cons [tvar; type_list tvar]];
      type_variance = [Variance.covariant];
    }
  and decl_option =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar];
      type_arity = 1;
      type_kind = Type_variant [cstr ident_none []; cstr ident_some [tvar]];
      type_variance = [Variance.covariant];
    }
  and decl_result =
    let tvar1, tvar2 = (newgenvar (), newgenvar ()) in
    {
      decl_abstr with
      type_params = [tvar1; tvar2];
      type_arity = 2;
      type_kind = Type_variant [cstr ident_ok [tvar1]; cstr ident_error [tvar2]];
      type_variance = [Variance.covariant; Variance.covariant];
    }
  and decl_dict =
    let tvar = newgenvar () in
    (* Dicts are implemented as a single "magic" field record. This magic field
       is the medium through which we can piggy back on the existing record pattern
       matching mechanism. We do this by letting the compiler route any label lookup
       for the dict record type to the magic field, which has the type of the values
       of the dict.

       So, this definition is important for the dict pattern matching functionality,
       but not something intended to be exposed to the user. *)
    {
      decl_abstr with
      type_attributes =
        [
          Dict_type_helpers.dict_attr;
          (Location.mknoloc "live", Parsetree.PStr []);
        ];
      type_params = [tvar];
      type_arity = 1;
      type_variance = [Variance.full];
      type_kind =
        Type_record
          ( [
              {
                ld_id = ident_dict_magic_field_name;
                ld_attributes = [Dict_type_helpers.dict_magic_field_attr];
                ld_loc = Location.none;
                ld_mutable = Immutable;
                ld_optional = true;
                ld_type = newgenty (Tconstr (path_option, [tvar], ref Mnil));
              };
            ],
            Record_regular );
    }
  and decl_uncurried =
    let tvar1 = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar1];
      type_arity = 1;
      type_kind = Type_variant [cstr ident_ctor_uncurried [tvar1]];
      type_variance = [Variance.covariant];
      type_unboxed = Types.unboxed_true_default_false;
    }
  and decl_unknown =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [];
      type_arity = 0;
      type_kind =
        Type_variant
          [
            {
              cd_id = ident_ctor_unknown;
              cd_args = Cstr_tuple [tvar];
              cd_res = Some type_unknown;
              cd_loc = Location.none;
              cd_attributes = [];
            };
          ];
      type_unboxed = Types.unboxed_true_default_false;
    }
  and decl_lazy_t =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar];
      type_arity = 1;
      type_variance = [Variance.covariant];
    }
  and decl_promise =
    let tvar = newgenvar () in
    {
      decl_abstr with
      type_params = [tvar];
      type_arity = 1;
      type_variance = [Variance.covariant];
    }
  in

  let add_exception id l =
    add_extension id
      {
        ext_type_path = path_exn;
        ext_type_params = [];
        ext_args = Cstr_tuple l;
        ext_ret_type = None;
        ext_private = Asttypes.Public;
        ext_loc = Location.none;
        ext_attributes =
          [
            ( {
                Asttypes.txt = "ocaml.warn_on_literal_pattern";
                loc = Location.none;
              },
              Parsetree.PStr [] );
          ];
        ext_is_exception = true;
      }
  in
  empty_env
  |> add_type ident_bool decl_bool
  |> add_type ident_int decl_abstr_imm
  |> add_type ident_float decl_abstr
  |> add_type ident_bigint decl_abstr
  |> add_type ident_string decl_abstr
  |> add_type ident_unit decl_unit
  |> add_type ident_extension_constructor decl_abstr
  |> add_type ident_exn decl_exn
  |> add_type ident_uncurried decl_uncurried
  |> add_type ident_option decl_option
  |> add_type ident_result decl_result
  |> add_type ident_lazy_t decl_lazy_t
  |> add_type ident_promise decl_promise
  |> add_type ident_array decl_array
  |> add_type ident_list decl_list
  |> add_type ident_dict decl_dict
  |> add_type ident_unknown decl_unknown
  |> add_exception ident_undefined_recursive_module
       [newgenty (Ttuple [type_string; type_int; type_int])]
  |> add_exception ident_assert_failure
       [newgenty (Ttuple [type_string; type_int; type_int])]
  |> add_exception ident_division_by_zero []
  |> add_exception ident_end_of_file []
  |> add_exception ident_not_found []
  |> add_exception ident_failure [type_string]
  |> add_exception ident_js_error [type_unknown]
  |> add_exception ident_invalid_argument [type_string]
  |> add_exception ident_match_failure
       [newgenty (Ttuple [type_string; type_int; type_int])]

let build_initial_env add_type add_exception empty_env =
  let common = common_initial_env add_type add_exception empty_env in
  let decl_type_char =
    {decl_abstr with type_manifest = Some type_int; type_private = Private}
  in
  add_type ident_char decl_type_char common

let builtin_values =
  List.map
    (fun id ->
      Ident.make_global id;
      (Ident.name id, id))
    [
      ident_match_failure;
      ident_invalid_argument;
      ident_failure;
      ident_js_error;
      ident_not_found;
      ident_end_of_file;
      ident_division_by_zero;
      ident_assert_failure;
      ident_undefined_recursive_module;
    ]

(* Start non-predef identifiers at 1000.  This way, more predefs can
   be defined in this file (above!) without breaking .cmi
   compatibility. *)

let _ = Ident.set_current_time 999
let builtin_idents = List.rev !builtin_idents
