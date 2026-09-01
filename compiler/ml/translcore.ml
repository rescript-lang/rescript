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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error = Unknown_builtin_primitive of string

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref
    (fun _cc _rootpath _modl -> assert false
      : module_coercion -> Path.t option -> module_expr -> lambda)

(* Number of payload-carrying constructors of the variant declaring
   [cstr]; part of the runtime representation of its blocks *)
let num_nonconst_constructors (cstr : Types.constructor_description) =
  Variant_runtime.num_blocks (Datarepr.constructor_variant cstr)

(* Compile an exception/extension definition *)

let transl_extension_constructor env path ext =
  let name =
    match path (*!Clflags.for_package*) with
    | None -> Ident.name ext.ext_id
    | Some p -> Path.name p
  in
  let loc = ext.ext_loc in
  match ext.ext_kind with
  | Text_decl _ -> Lprim (Pcreate_extension name, [], loc)
  | Text_rebind (path, _lid) -> transl_extension_path ~loc env path

(* Translation of primitives *)

let builtin_of_lowering (l : Unified_ops.lowering) : Lambda.builtin =
  match l with
  | Lower p -> Primitive p
  | Pass_through -> Eliminated Identity

(** This is ad-hoc translation for unifying specific primitive operations
     See [Unified_ops] module for detailed explanation.
  *)
let translate_unified_ops (prim : Primitive.description) (env : Env.t)
    (lhs_type : type_expr) : Lambda.builtin option =
  (* lhs_type is already unified in type-level *)
  let entry = Hashtbl.find_opt Unified_ops.index_by_name prim.prim_name in
  let lowering =
    match entry with
    | Some {specialization} -> (
      match specialization with
      | {int}
        when is_base_type env lhs_type Predef.path_int
             || maybe_pointer_type env lhs_type = Immediate ->
        Some int
      | {float = Some float} when is_base_type env lhs_type Predef.path_float ->
        Some float
      | {bigint = Some bigint} when is_base_type env lhs_type Predef.path_bigint
        ->
        Some bigint
      | {string = Some string} when is_base_type env lhs_type Predef.path_string
        ->
        Some string
      | {bool = Some bool} when is_base_type env lhs_type Predef.path_bool ->
        Some bool
      | {int} -> Some int)
    | _ -> None
  in
  Option.map builtin_of_lowering lowering

type specialized = {
  objcomp: Lambda.primitive;
  intcomp: Lambda.primitive;
  boolcomp: Lambda.primitive;
  floatcomp: Lambda.primitive;
  stringcomp: Lambda.primitive;
  bigintcomp: Lambda.primitive;
  simplify_constant_constructor: bool;
}

let comparisons_table =
  create_hashtable
    [|
      ( "%equal",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = true;
        } );
      ( "%notequal",
        {
          objcomp = Pobjcomp Cneq;
          intcomp = Pintcomp Cneq;
          boolcomp = Pboolcomp Cneq;
          floatcomp = Pfloatcomp Cneq;
          stringcomp = Pstringcomp Cneq;
          bigintcomp = Pbigintcomp Cneq;
          simplify_constant_constructor = true;
        } );
      ( "%lessthan",
        {
          objcomp = Pobjcomp Clt;
          intcomp = Pintcomp Clt;
          boolcomp = Pboolcomp Clt;
          floatcomp = Pfloatcomp Clt;
          stringcomp = Pstringcomp Clt;
          bigintcomp = Pbigintcomp Clt;
          simplify_constant_constructor = false;
        } );
      ( "%greaterthan",
        {
          objcomp = Pobjcomp Cgt;
          intcomp = Pintcomp Cgt;
          boolcomp = Pboolcomp Cgt;
          floatcomp = Pfloatcomp Cgt;
          stringcomp = Pstringcomp Cgt;
          bigintcomp = Pbigintcomp Cgt;
          simplify_constant_constructor = false;
        } );
      ( "%lessequal",
        {
          objcomp = Pobjcomp Cle;
          intcomp = Pintcomp Cle;
          boolcomp = Pboolcomp Cle;
          floatcomp = Pfloatcomp Cle;
          stringcomp = Pstringcomp Cle;
          bigintcomp = Pbigintcomp Cle;
          simplify_constant_constructor = false;
        } );
      ( "%greaterequal",
        {
          objcomp = Pobjcomp Cge;
          intcomp = Pintcomp Cge;
          boolcomp = Pboolcomp Cge;
          floatcomp = Pfloatcomp Cge;
          stringcomp = Pstringcomp Cge;
          bigintcomp = Pbigintcomp Cge;
          simplify_constant_constructor = false;
        } );
      ( "%compare",
        {
          objcomp = Pobjorder;
          intcomp = Pintorder;
          boolcomp = Pboolorder;
          floatcomp = Pfloatorder;
          stringcomp = Pstringorder;
          bigintcomp = Pbigintorder;
          simplify_constant_constructor = false;
        } );
      ( "%max",
        {
          objcomp = Pobjmax;
          intcomp = Pintmax;
          boolcomp = Pboolmax;
          floatcomp = Pfloatmax;
          stringcomp = Pstringmax;
          bigintcomp = Pbigintmax;
          simplify_constant_constructor = false;
        } );
      ( "%min",
        {
          objcomp = Pobjmin;
          intcomp = Pintmin;
          boolcomp = Pboolmin;
          floatcomp = Pfloatmin;
          stringcomp = Pstringmin;
          bigintcomp = Pbigintmin;
          simplify_constant_constructor = false;
        } );
      ( "%equal_null",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      ( "%equal_undefined",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      ( "%equal_nullable",
        {
          objcomp = Pobjcomp Ceq;
          intcomp = Pintcomp Ceq;
          boolcomp = Pboolcomp Ceq;
          floatcomp = Pfloatcomp Ceq;
          stringcomp = Pstringcomp Ceq;
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = false;
        } );
      (* FIXME: Core compatibility *)
      ( "%bs_min",
        {
          objcomp = Pobjmin;
          intcomp = Pintmin;
          boolcomp = Pboolmin;
          floatcomp = Pfloatmin;
          stringcomp = Pstringmin;
          bigintcomp = Pbigintmin;
          simplify_constant_constructor = false;
        } );
      ( "%bs_max",
        {
          objcomp = Pobjmax;
          intcomp = Pintmax;
          boolcomp = Pboolmax;
          floatcomp = Pfloatmax;
          stringcomp = Pstringmax;
          bigintcomp = Pbigintmax;
          simplify_constant_constructor = false;
        } );
    |]

(* Builtins with no primitive form: [Lambda.mk_builtin] erases them at
   translation. *)
let erased_builtins : (string * Lambda.builtin) array =
  [|
    ("%identity", Eliminated Identity);
    ("%component_identity", Eliminated Identity);
    ("%ignore", Eliminated Ignore);
    ("%incr", Offset_ref 1);
    ("%decr", Offset_ref (-1));
    ("%null", Constant Const_js_null);
    ("%undefined", Constant (Const_js_undefined {is_unit = false}));
    (* FIXME: Core compatibility *)
    ("#null", Constant Const_js_null);
    ("#undefined", Constant (Const_js_undefined {is_unit = false}));
  |]

let primitive_builtins : (string * Lambda.builtin) array =
  Array.map
    (fun (name, p) -> (name, Lambda.Primitive p))
    [|
      (* BEGIN Triples for  ref data type *)
      ("%makeref", Pmakeblock Lambda.ref_tag_info);
      ("%refset", Psetfield (0, Lambda.ref_field_set_info));
      ("%refget", Pfield (0, Lambda.ref_field_info));
      (* Finish Triples for  ref data type *)
      ("%field0", Pfield (0, Fld_tuple));
      ("%field1", Pfield (1, Fld_tuple));
      ("%obj_dup", Pduprecord);
      ("%obj_tag", Pobjtag);
      ("%obj_size", Pobjsize);
      ("%obj_get_field", Parrayrefu);
      ("%obj_set_field", Parraysetu);
      ("%raise", Praise);
      (* bool primitives *)
      ("%sequand", Psequand);
      ("%sequor", Psequor);
      ("%boolnot", Pnot);
      ("%boolorder", Pboolorder);
      ("%boolmin", Pboolmin);
      ("%boolmax", Pboolmax);
      (* int primitives *)
      ("%obj_is_int", Pisint);
      ("%negint", Pnegint);
      ("%addint", Paddint);
      ("%subint", Psubint);
      ("%mulint", Pmulint);
      ("%divint", Pdivint);
      ("%modint", Pmodint);
      ("%bitnot_int", Pnotint);
      ("%andint", Pandint);
      ("%orint", Porint);
      ("%xorint", Pxorint);
      ("%lslint", Plslint);
      ("%lsrint", Plsrint);
      ("%asrint", Pasrint);
      ("%eq", Pintcomp Ceq);
      ("%noteq", Pintcomp Cneq);
      ("%ltint", Pintcomp Clt);
      ("%leint", Pintcomp Cle);
      ("%gtint", Pintcomp Cgt);
      ("%geint", Pintcomp Cge);
      ("%intorder", Pintorder);
      ("%intmin", Pintmin);
      ("%intmax", Pintmax);
      (* float primitives *)
      ("%negfloat", Pnegfloat);
      ("%addfloat", Paddfloat);
      ("%subfloat", Psubfloat);
      ("%mulfloat", Pmulfloat);
      ("%divfloat", Pdivfloat);
      ("%modfloat", Pmodfloat);
      ("%eqfloat", Pfloatcomp Ceq);
      ("%noteqfloat", Pfloatcomp Cneq);
      ("%ltfloat", Pfloatcomp Clt);
      ("%lefloat", Pfloatcomp Cle);
      ("%gtfloat", Pfloatcomp Cgt);
      ("%gefloat", Pfloatcomp Cge);
      ("%floatorder", Pfloatorder);
      ("%floatmin", Pfloatmin);
      ("%floatmax", Pfloatmax);
      (* bigint primitives *)
      ("%negbigint", Pnegbigint);
      ("%addbigint", Paddbigint);
      ("%subbigint", Psubbigint);
      ("%mulbigint", Pmulbigint);
      ("%divbigint", Pdivbigint);
      ("%powbigint", Ppowbigint);
      ("%modbigint", Pmodbigint);
      ("%eqbigint", Pbigintcomp Ceq);
      ("%noteqbigint", Pbigintcomp Cneq);
      ("%ltbigint", Pbigintcomp Clt);
      ("%lebigint", Pbigintcomp Cle);
      ("%gtbigint", Pbigintcomp Cgt);
      ("%gebigint", Pbigintcomp Cge);
      ("%bitnot_bigint", Pnotbigint);
      ("%andbigint", Pandbigint);
      ("%orbigint", Porbigint);
      ("%xorbigint", Pxorbigint);
      ("%lslbigint", Plslbigint);
      ("%asrbigint", Pasrbigint);
      ("%bigintorder", Pbigintorder);
      ("%bigintmin", Pbigintmin);
      ("%bigintmax", Pbigintmax);
      (* string primitives *)
      ("%string_length", Pstringlength);
      ("%string_safe_get", Pstringrefs);
      ("%string_unsafe_get", Pstringrefu);
      ("%stringorder", Pstringorder);
      ("%stringmin", Pstringmin);
      ("%stringmax", Pstringmax);
      ("%string_concat", Pstringadd);
      (* array primitives *)
      ("%array_length", Parraylength);
      ("%array_safe_get", Parrayrefs);
      ("%array_safe_set", Parraysets);
      ("%array_unsafe_get", Parrayrefu);
      ("%array_unsafe_set", Parraysetu);
      (* dict primitives *)
      ("%makedict", Pmakedict);
      ("%dict_has", Pdict_has);
      (* promise *)
      ("%await", Pawait);
      (* module *)
      (* hash *)
      ("%hash", Phash);
      ("%hash_mix_int", Phash_mixint);
      ("%hash_mix_string", Phash_mixstring);
      ("%hash_final_mix", Phash_finalmix);
      (* etc *)
      ("%typeof", Ptypeof);
      ("%debugger", Pdebugger);
      ("%intoffloat", Pintoffloat);
      ("%floatofint", Pfloatofint);
      ("%unsafe_eq", Pjscomp Ceq);
      ("%unsafe_neq", Pjscomp Cneq);
      ("%unsafe_lt", Pjscomp Clt);
      ("%unsafe_le", Pjscomp Cle);
      ("%unsafe_gt", Pjscomp Cgt);
      ("%unsafe_ge", Pjscomp Cge);
      ("%is_nullable", Pis_null_undefined);
      ("%null_to_opt", Pnull_to_opt);
      ("%nullable_to_opt", Pnull_undefined_to_opt);
      ("%makemutablelist", Pmakelist);
      ("%unsafe_to_method", Pjs_fn_method);
      (* Compiler internals, never expose to ReScript files *)
      (* FIXME: Core compatibility *)
      ("#typeof", Ptypeof);
      ("#is_nullable", Pis_null_undefined);
      ("#null_to_opt", Pnull_to_opt);
      ("#nullable_to_opt", Pnull_undefined_to_opt);
      ("#makemutablelist", Pmakelist);
      (* FIXME: Deprecated *)
      ("%obj_field", Parrayrefu);
    |]

let builtins_table : (string, Lambda.builtin) Hashtbl.t =
  create_hashtable (Array.append erased_builtins primitive_builtins)

let find_builtin prim_name = Hashtbl.find builtins_table prim_name

let specialize_comparison
    ({objcomp; intcomp; floatcomp; stringcomp; bigintcomp; boolcomp} :
      specialized) env ty =
  match () with
  | ()
    when is_base_type env ty Predef.path_int
         || is_base_type env ty Predef.path_char
         || maybe_pointer_type env ty = Immediate ->
    intcomp
  | () when is_base_type env ty Predef.path_float -> floatcomp
  | () when is_base_type env ty Predef.path_string -> stringcomp
  | () when is_base_type env ty Predef.path_bigint -> bigintcomp
  | () when is_base_type env ty Predef.path_bool -> boolcomp
  | () -> objcomp

(* Specialize a primitive from available type information,
   raise Not_found if primitive is unknown *)

let specialize_primitive p env ty (* ~has_constant_constructor *) =
  let fn_expr = is_function_type env ty in
  let unified =
    match fn_expr with
    | Some (lhs, _) -> translate_unified_ops p env lhs
    | None -> None
  in
  match unified with
  | Some builtin -> builtin
  | None -> (
    try
      let table = Hashtbl.find comparisons_table p.prim_name in
      match fn_expr with
      | Some (lhs, _rhs) -> Primitive (specialize_comparison table env lhs)
      | None -> Primitive table.objcomp
    with Not_found -> find_builtin p.prim_name)

(* [is_unit] excluded: unit shares the undefined constant but is not a
   [%null] / [%undefined] literal, and comparing it never suppressed the
   warning. *)
let is_null_undefined_constant = function
  | Lconst (Const_js_null | Const_js_undefined {is_unit = false}) -> true
  | _ -> false

let warn_polymorphic_comparison loc (builtin : Lambda.builtin) args =
  match (builtin, args) with
  | Primitive (Pobjcomp (Ceq | Cneq)), [arg1; arg2]
    when is_null_undefined_constant arg1 || is_null_undefined_constant arg2 ->
    ()
  | Primitive (Pobjcomp _ | Pobjorder | Pobjmin | Pobjmax), _ ->
    Location.prerr_warning loc Warnings.Bs_polymorphic_comparison
  | _ -> ()

(* Expansion of an external declaration: the final FFI form is produced
   here, at translation, from the structured spec. *)

let lambda_of_inline_const (c : External_ffi_types.inline_const) :
    Lambda.structured_constant =
  match c with
  | Const_str {s; delim} -> Const_string {s; delim}
  | Const_bool true -> Const_js_true
  | Const_bool false -> Const_js_false
  | Const_int i -> Const_int i
  | Const_bigint {negative; digits} -> Const_bigint (negative, digits)
  | Const_float f -> Const_float f

(* The argument of the dynamic-import primitive is a module reference,
   never an expression: resolve it here, at translation, from the typedtree.
   The backend emits [import("path")[.then(m => m.<name>)]] directly from
   the resolved source. *)
let import_source_of_arg (arg : Typedtree.expression) : Lambda.import_source =
  let unsupported loc =
    Location.raise_errorf ~loc
      "Invalid argument: unsupported argument to dynamic import. If you \
       believe this should be supported, please open an issue."
  in
  let of_global_path ?(is_module = false) loc env (path : Path.t) :
      Lambda.import_source =
    (* a module path is normalized fully (resolving a final alias hop such
       as a namespace's [module List = Stdlib_List]); a value path
       normalizes its module prefix, like [transl_value_path] *)
    let path =
      if is_module then Env.normalize_path (Some loc) env path
      else Env.normalize_path_prefix (Some loc) env path
    in
    let rec segments (p : Path.t) acc =
      match p with
      | Pident id -> if Ident.persistent id then Some (id, acc) else None
      | Pdot (p, name, _) -> segments p (name :: acc)
      | Papply _ -> None
    in
    match segments path [] with
    | Some (module_, path) -> Import_module {module_; path}
    | None ->
      Location.raise_errorf ~loc
        "Invalid argument: Dynamic import requires a module or module value \
         that is a file as argument. Passing a value or local module is not \
         allowed."
  in
  let rec module_path (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_ident (path, _) -> Some path
    | Tmod_constraint (me, _, _, _) -> module_path me
    | _ -> None
  in
  match arg.exp_desc with
  | Texp_pack me -> (
    match module_path me with
    | Some path -> of_global_path ~is_module:true arg.exp_loc me.mod_env path
    | None -> unsupported arg.exp_loc)
  | Texp_ident (_, _, {val_kind = Val_prim ({prim_kind; prim_name} as p)}) -> (
    match prim_kind with
    | Kind_external
        (Ffi_bs (_, _, {kind = Decl_val {name}; module_; scopes; _})) -> (
      match module_ with
      | Some (Module_named emn) ->
        (* the external's value lives at [module.<scopes>.<name>] *)
        Import_external {module_ = emn; path = scopes @ [name]}
      | Some Module_itself ->
        Import_external
          {
            module_ =
              {
                bundle = prim_name;
                module_bind_name = Phint_nothing;
                import_attributes = None;
              };
            path = [];
          }
      | None ->
        ignore p;
        unsupported arg.exp_loc)
    | _ -> unsupported arg.exp_loc)
  | Texp_ident (path, _, _) -> of_global_path arg.exp_loc arg.exp_env path
  | _ -> unsupported arg.exp_loc

(* Does the external's declared type return unit? Decides whether the JS
   call's result is replaced by unit. Computed from the declared scheme so
   it is stable across use sites; aliases of unit count as unit. *)
let external_returns_unit env (p : Primitive.description) (val_type : type_expr)
    =
  let result_type =
    if p.prim_arity = 0 then val_type
    else
      match (Ctype.expand_head env val_type).desc with
      | Tarrow (_, ret) -> ret
      | _ -> val_type
  in
  is_base_type env result_type Predef.path_unit

let external_result_wrap loc (result_type : External_ffi_types.return_wrapper)
    ~returns_unit result =
  match result_type with
  | Return_unset when returns_unit -> Lsequence (result, Lconst const_unit)
  | Return_null_to_opt -> Lprim (Pnull_to_opt, [result], loc)
  | Return_null_undefined_to_opt -> Lprim (Pnull_undefined_to_opt, [result], loc)
  | Return_unset | Return_identity -> result

(* Does importing this external as a value require the FFI adaptation a
   direct call would apply? Identity bindings (and pure unit-return
   conventions) hand out the raw JS value; adapters whose absence is
   observably wrong at the import's call sites get a wrapper. *)
let external_import_needs_adaptation (arg_types : External_arg_spec.params)
    (decl : External_ffi_types.external_decl)
    (return_wrapper : External_ffi_types.return_wrapper) =
  decl.variadic
  || (match return_wrapper with
    | Return_null_to_opt | Return_null_undefined_to_opt -> true
    | Return_unset | Return_identity -> false)
  || Ext_list.exists arg_types (fun {arg_type; arg_label} ->
      (match arg_type with
        | Nothing | Extern_unit -> false
        | Poly_var_string _ | Poly_var _ | Int _ | Arg_cst _ | Ignore | Unwrap
          ->
          true)
      ||
      match arg_label with
      | Arg_optional -> true
      | Arg_label | Arg_empty -> false)

(* [import(f)] where [f]'s binding needs adaptation lowers to

     import("module").then(m => (a1 .. an) => <adapted call of m.<f>>)

   so the imported value honestly has the external's ReScript type. The
   [.then] is itself a send, and the adapted call is a send on the awaited
   module (a get for an arity-0 value), reusing the ordinary FFI call
   machinery - including scope-chain access on the receiver. *)
let transl_adapted_external_import loc env
    ~(emn : External_ffi_types.external_module_name) ~name ~scopes ~variadic
    ~(arg_types : External_arg_spec.params) ~return_wrapper
    (p : Primitive.description) (val_type : type_expr) : Lambda.lambda =
  let returns_unit = external_returns_unit env p val_type in
  let send_call receiver args (kind : External_ffi_types.decl_kind) =
    Lprim
      ( Pjs_call
          {
            prim_name = name;
            arg_types = External_arg_spec.dummy :: arg_types;
            ffi =
              {
                kind;
                module_ = None;
                scopes;
                variadic;
                effective_arity = List.length arg_types + 1;
              };
            transformed_jsx = false;
          },
        receiver :: args,
        loc )
  in
  let m = Ident.create "m" in
  let adapted_value =
    if p.prim_arity = 0 then
      external_result_wrap loc return_wrapper ~returns_unit
        (send_call (Lvar m) [] (Decl_get {name}))
    else
      let params =
        List.init p.prim_arity (fun i ->
            Ident.create ("prim" ^ string_of_int i))
      in
      Lfunction
        {
          params;
          attr = default_function_attribute;
          loc;
          body =
            external_result_wrap loc return_wrapper ~returns_unit
              (send_call (Lvar m)
                 (List.map (fun i -> Lvar i) params)
                 (Decl_send {name}));
        }
  in
  let callback =
    Lfunction
      {
        params = [m];
        attr = default_function_attribute;
        loc;
        body = adapted_value;
      }
  in
  Lprim
    ( Pjs_call
        {
          prim_name = "then";
          arg_types = [External_arg_spec.dummy; External_arg_spec.dummy];
          ffi =
            {
              kind = Decl_send {name = "then"};
              module_ = None;
              scopes = [];
              variadic = false;
              effective_arity = 2;
            };
          transformed_jsx = false;
        },
      [
        Lprim (Pimport (Import_external {module_ = emn; path = []}), [], loc);
        callback;
      ],
      loc )

let transl_dynamic_import loc (arg : Typedtree.expression) : Lambda.lambda =
  match arg.exp_desc with
  | Texp_ident
      ( _,
        _,
        {
          val_kind =
            Val_prim
              ({
                 prim_kind =
                   Kind_external
                     (Ffi_bs
                        ( arg_types,
                          return_wrapper,
                          ({
                             kind = Decl_val {name};
                             module_ = Some (Module_named emn);
                             scopes;
                             variadic;
                             _;
                           } as decl) ));
               } as p);
          val_type;
        } )
    when external_import_needs_adaptation arg_types decl return_wrapper ->
    transl_adapted_external_import loc arg.exp_env ~emn ~name ~scopes ~variadic
      ~arg_types ~return_wrapper p val_type
  | _ -> Lprim (Pimport (import_source_of_arg arg), [], loc)

let transl_external_application loc env (p : Primitive.description)
    ~(val_type : type_expr) argl ~transformed_jsx : Lambda.lambda =
  match p.prim_kind with
  | Kind_inline_const c -> Lconst (lambda_of_inline_const c)
  | Kind_external (Ffi_obj_create labels) ->
    Lprim (Pjs_object_create labels, argl, loc)
  | Kind_external (Ffi_bs (arg_types, result_type, decl)) ->
    external_result_wrap loc result_type
      ~returns_unit:(external_returns_unit env p val_type)
      (Lprim
         ( Pjs_call
             {prim_name = p.prim_name; arg_types; ffi = decl; transformed_jsx},
           argl,
           loc ))
  | Kind_intrinsic ->
    Location.raise_errorf ~loc
      "@{<error>Error:@} internal error, using unrecognized primitive %s"
      p.prim_name

(* Compile-time source location (`__LOC__` / `%loc_*`). Lowered here so
   Lambda never carries a location primitive. *)
type loc_kind = Loc_FILE | Loc_LINE | Loc_MODULE | Loc_LOC | Loc_POS

let loc_kind_of_prim_name = function
  | "%loc_LOC" -> Some Loc_LOC
  | "%loc_FILE" -> Some Loc_FILE
  | "%loc_LINE" -> Some Loc_LINE
  | "%loc_POS" -> Some Loc_POS
  | "%loc_MODULE" -> Some Loc_MODULE
  | _ -> None

let lam_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let file, lnum, cnum = Location.get_pos_info loc_start in
  let file = Filename.basename file in
  let enum =
    loc.Location.loc_end.Lexing.pos_cnum - loc_start.Lexing.pos_cnum + cnum
  in
  match kind with
  | Loc_POS ->
    Lconst
      (Const_block
         ( Blk_tuple,
           [
             const_string file None;
             const_int lnum;
             const_int cnum;
             const_int enum;
           ] ))
  | Loc_FILE -> Lconst (const_string file None)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Env.get_unit_name () in
    let module_name = if name = "" then "//" ^ filename ^ "//" else name in
    Lconst (const_string module_name None)
  | Loc_LOC ->
    let loc =
      Printf.sprintf "File %S, line %d, characters %d-%d" file lnum cnum enum
    in
    Lconst (const_string loc None)
  | Loc_LINE -> Lconst (const_int lnum)

(* Eta-expand a primitive *)

let transl_primitive loc p env ty ~val_type =
  (* Printf.eprintf "----transl_primitive %s----\n" p.prim_name; *)
  match loc_kind_of_prim_name p.prim_name with
  | Some kind -> (
    let lam = lam_of_loc kind loc in
    match p.prim_arity with
    | 0 -> lam
    | 1 ->
      let param = Ident.create "prim" in
      Lfunction
        {
          params = [param];
          attr = default_function_attribute;
          loc;
          body = Lprim (Pmakeblock Blk_tuple, [lam; Lvar param], loc);
        }
    | _ -> assert false)
  | None -> (
    let prim =
      try Some (specialize_primitive p env ty) with Not_found -> None
    in
    match prim with
    | None when p.prim_name = "%import" || p.prim_name = "#import" ->
      Location.raise_errorf ~loc
        "Dynamic import must be applied directly to a module or a value from \
         another module; it cannot be used as a first-class value."
    | None ->
      (* an external: expand its FFI spec, eta-expanded to its arity *)
      if p.prim_from_constructor || p.prim_arity = 0 then
        transl_external_application loc env p ~val_type []
          ~transformed_jsx:false
      else
        let params =
          if p.prim_arity = 1 then [Ident.create "prim"]
          else
            List.init p.prim_arity (fun i ->
                Ident.create ("prim" ^ string_of_int i))
        in
        Lfunction
          {
            params;
            attr = default_function_attribute;
            loc;
            body =
              transl_external_application loc env p ~val_type
                (List.map (fun id -> Lvar id) params)
                ~transformed_jsx:false;
          }
    | Some builtin ->
      warn_polymorphic_comparison loc builtin [];
      let rec make_params n total =
        if n <= 0 then []
        else
          Ident.create ("prim" ^ string_of_int (total - n))
          :: make_params (n - 1) total
      in
      let prim_arity = p.prim_arity in
      if p.prim_from_constructor || prim_arity = 0 then
        mk_builtin builtin [] loc
      else
        let params =
          if prim_arity = 1 then [Ident.create "prim"]
          else make_params prim_arity prim_arity
        in
        Lfunction
          {
            params;
            attr = default_function_attribute;
            loc;
            body = mk_builtin builtin (List.map (fun id -> Lvar id) params) loc;
          })

(* [None] means the primitive is an external whose application must be
   expanded from its FFI spec *)
let transl_primitive_application loc prim env ty args : Lambda.builtin option =
  let prim_name = prim.prim_name in
  let unified =
    match args with
    | [arg1] | [arg1; _] -> translate_unified_ops prim env arg1.exp_type
    | _ -> None
  in
  match unified with
  | Some builtin -> Some builtin
  | None -> (
    try
      match args with
      | [arg1; _]
        when is_base_type env arg1.exp_type Predef.path_bool
             && Hashtbl.mem comparisons_table prim_name ->
        Some (Primitive (Hashtbl.find comparisons_table prim_name).boolcomp)
      | _ ->
        let has_constant_constructor =
          match args with
          | [
              _;
              {
                exp_desc =
                  Texp_construct
                    (_, {cstr_kind = Ordinary_constructor _; cstr_args = []}, _);
              };
            ]
          | [
              {
                exp_desc =
                  Texp_construct
                    (_, {cstr_kind = Ordinary_constructor _; cstr_args = []}, _);
              };
              _;
            ]
          | [_; {exp_desc = Texp_variant (_, None)}]
          | [{exp_desc = Texp_variant (_, None)}; _] ->
            true
          | _ -> false
        in
        if has_constant_constructor then
          match Hashtbl.find_opt comparisons_table prim_name with
          | Some table when table.simplify_constant_constructor ->
            Some (Primitive table.intcomp)
          | Some _ | None -> Some (specialize_primitive prim env ty)
          (* ~has_constant_constructor*)
        else Some (specialize_primitive prim env ty)
    with Not_found ->
      if String.length prim_name > 0 && prim_name.[0] = '%' then
        raise (Error (loc, Unknown_builtin_primitive prim_name));
      None)

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
  | Lconst sc -> sc
  | _ -> raise_notrace Not_constant

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

(* Assertions *)

let assert_failed exp =
  let fname, line, char =
    Location.get_pos_info exp.exp_loc.Location.loc_start
  in
  let fname = Filename.basename fname in
  Lprim
    ( Praise,
      [
        Lprim
          ( Pmakeblock Blk_extension,
            [
              transl_normal_path Predef.path_assert_failure;
              Lconst
                (Const_block
                   ( Blk_tuple,
                     [const_string fname None; const_int line; const_int char]
                   ));
            ],
            exp.exp_loc );
      ],
      exp.exp_loc )

let rec cut n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> failwith "Translcore.cut"
    | a :: l ->
      let l1, l2 = cut (n - 1) l in
      (a :: l1, l2)

(* Translation of expressions *)

(* JS catch can receive a ReScript exception or a raw throw. If the handler
   inspects [fv], bind [fv] to [Primitive_exceptions.internalToException raw]
   so matching sees a ReScript value. Pure [throw v] is not an inspect:
   rethrow the raw JS value. *)
let wrap_exn loc arg =
  Lapply
    {
      ap_func =
        Lprim
          ( Pfield (0, Fld_module {name = "internalToException"}),
            [
              Lprim
                ( Pgetglobal
                    (Ident.create_persistent Primitive_modules.exceptions),
                  [],
                  loc );
            ],
            loc );
      ap_args = [arg];
      ap_loc = loc;
      ap_inlined = Default_inline;
      ap_transformed_jsx = false;
    }
let exception_id_destructed (l : lambda) (fv : Ident.t) : bool =
  let rec hit_opt = function
    | None -> false
    | Some a -> hit a
  and hit_list_snd : 'a. ('a * _) list -> bool =
   fun x -> Ext_list.exists_snd x hit
  and hit_list xs = Ext_list.exists xs hit
  and hit (l : lambda) =
    match l with
    | Lprim (Praise, [Lvar _], _) -> false
    | Lprim (_, args, _) -> hit_list args
    | Lvar id -> Ident.same id fv
    | Lassign (id, e) -> Ident.same id fv || hit e
    | Lstaticcatch (e1, _, e2) -> hit e1 || hit e2
    | Ltrywith (e1, _, e2) -> hit e1 || hit e2
    | Lfunction {body} -> hit body
    | Llet (_, _, _, arg, body) -> hit arg || hit body
    | Lletrec (decl, body) -> hit body || hit_list_snd decl
    | Lfor (_, e1, e2, _, e3) -> hit e1 || hit e2 || hit e3
    | Lfor_of (_, e1, e2) | Lfor_await_of (_, e1, e2) -> hit e1 || hit e2
    | Lconst _ -> false
    | Lapply {ap_func; ap_args} -> hit ap_func || hit_list ap_args
    | Lswitch (arg, sw, _) ->
      hit arg || hit_list_snd sw.sw_consts || hit_list_snd sw.sw_blocks
      || hit_opt sw.sw_failaction
    | Lstringswitch (arg, cases, default, _) ->
      hit arg || hit_list_snd cases || hit_opt default
    | Lstaticraise (_, args) -> hit_list args
    | Lifthenelse (e1, e2, e3) -> hit e1 || hit e2 || hit e3
    | Lsequence (e1, e2) -> hit e1 || hit e2
    | Lbreak | Lcontinue -> false
    | Lwhile (e1, e2) -> hit e1 || hit e2
  in
  hit l

let pack_trywith_exn id handler =
  if exception_id_destructed handler id then
    let raw_id = Ident.create ("raw_" ^ id.name) in
    ( raw_id,
      Llet
        (StrictOpt, Pgenval, id, wrap_exn Location.none (Lvar raw_id), handler)
    )
  else (id, handler)

let extract_directive_for_fn exp =
  exp.exp_attributes
  |> List.find_map (fun ({txt}, payload) ->
      if txt = "directive" then Ast_payload.is_single_string payload else None)

let hoisted_function_attr_name = "res.hoistedFunction"

let find_js_hoisted_attr attrs =
  Translattribute.get_empty_attribute hoisted_function_attr_name attrs

(* A value binding's source attributes are not carried all the way to JS
   emission. Record the binding and its source path here so later compiler
   stages can add the flat JS export and matching .cmj metadata. *)
let mark_js_hoisted_pattern ~js_hoist attrs pat lam =
  match find_js_hoisted_attr attrs with
  | None -> ()
  | Some loc -> (
    match lam with
    | Lfunction _ -> (
      match pat.pat_desc with
      | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) -> (
        match js_hoist with
        | Some register -> register id loc
        | None ->
          Location.prerr_warning loc
            (Warnings.Misplaced_attribute hoisted_function_attr_name))
      | _ ->
        Location.prerr_warning loc
          (Warnings.Misplaced_attribute hoisted_function_attr_name))
    | _ ->
      Location.prerr_warning loc
        (Warnings.Misplaced_attribute hoisted_function_attr_name))

let rec transl_exp e =
  Builtin_attributes.warning_scope ~ppwarning:false e.exp_attributes (fun () ->
      List.iter (Translattribute.check_attribute e) e.exp_attributes;
      transl_exp0 e)

and transl_exp0 (e : Typedtree.expression) : Lambda.lambda =
  match e.exp_desc with
  | Texp_ident (_, _, ({val_kind = Val_prim p} as vd)) ->
    transl_primitive e.exp_loc p e.exp_env e.exp_type ~val_type:vd.val_type
  | Texp_ident (path, _, {val_kind = Val_reg}) ->
    transl_value_path ~loc:e.exp_loc e.exp_env path
  | Texp_constant cst -> Lconst (const_of_typed cst)
  | Texp_let (rec_flag, pat_expr_list, body) ->
    transl_let ~js_hoist:None rec_flag pat_expr_list (transl_exp body)
  | Texp_function {params = fparams; body; async} ->
    let directive =
      match extract_directive_for_fn e with
      | None -> None
      | Some (directive, _) -> Some directive
    in
    let params, lbody, return_unit = transl_function e.exp_loc fparams body in
    let one_unit_arg =
      match (fparams, (Ctype.expand_head e.exp_env e.exp_type).desc) with
      | [{fp_pat}], Tarrow ([{lbl = Nolabel; typ}], _)
        when Typedtree.pat_bound_idents fp_pat = [] -> (
        match (Ctype.expand_head e.exp_env typ).desc with
        | Tconstr (Pident {name = "unit"}, [], _) -> true
        | _ -> false)
      | _ -> false
    in
    let attr =
      {
        default_function_attribute with
        inline = Translattribute.get_inline_attribute e.exp_attributes;
        async;
        return_unit;
        directive;
        one_unit_arg;
      }
    in
    let loc = e.exp_loc in
    Lfunction {params; body = lbody; attr; loc}
  | Texp_apply {funct; args = oargs}
    when List.exists
           (fun (attr, _) -> attr.txt = "res.taggedTemplate")
           e.exp_attributes ->
    (* Backtick tagged-template syntax on a value of the builtin
       [taggedTemplate<'param, 'output>] type. Typecore has already checked the
       tag's type, so here we just emit a real JS tagged-template literal,
       regardless of how the tag value was obtained (external, let-binding,
       function parameter, factory result, cross-module). *)
    let strings, values =
      match oargs with
      | [(_, Some strings); (_, Some values)] -> (strings, values)
      | _ -> assert false
    in
    Lprim
      ( Ptagged_template,
        [transl_exp funct; transl_exp strings; transl_exp values],
        e.exp_loc )
  | Texp_apply
      {
        funct =
          {
            exp_desc = Texp_ident (_, _, ({val_kind = Val_prim p} as prim_vd));
            exp_type = prim_type;
          } as funct;
        args = oargs;
        transformed_jsx;
      }
    when List.length oargs >= p.prim_arity
         && List.for_all (fun (_, arg) -> arg <> None) oargs -> (
    let args, args' = cut p.prim_arity oargs in
    let wrap f =
      if args' = [] then f
      else
        let inlined, _ =
          Translattribute.get_and_remove_inlined_attribute funct
        in
        transl_apply ~inlined ~transformed_jsx f args' e.exp_loc
    in
    let args =
      List.map
        (function
          | _, Some x -> x
          | _ -> assert false)
        args
    in
    match (p.prim_name, args) with
    | ("%import" | "#import"), [arg] ->
      wrap (transl_dynamic_import e.exp_loc arg)
    | _ -> (
      let argl = transl_list args in
      match loc_kind_of_prim_name p.prim_name with
      | Some kind -> (
        match args with
        | [] -> wrap (lam_of_loc kind e.exp_loc)
        | [arg1] ->
          let lam = lam_of_loc kind arg1.exp_loc in
          wrap (Lprim (Pmakeblock Blk_tuple, lam :: argl, e.exp_loc))
        | _ -> assert false)
      | None -> (
        match
          transl_primitive_application e.exp_loc p e.exp_env prim_type args
        with
        | None -> (
          (* an external: expand its FFI spec here; %raw parses and classifies
         its snippet *)
          match (p.prim_name, argl) with
          | "#raw_expr", [Lconst (Const_string {s = code})] ->
            let kind = Classify_function.classify code in
            wrap
              (Lprim (Praw_js_code {code; code_info = Exp kind}, [], e.exp_loc))
          | "#raw_stmt", [Lconst (Const_string {s = code})] ->
            let kind = Classify_function.classify_stmt code in
            wrap
              (Lprim (Praw_js_code {code; code_info = Stmt kind}, [], e.exp_loc))
          | ("#raw_expr" | "#raw_stmt"), _ -> assert false
          | _ ->
            wrap
              (transl_external_application e.exp_loc e.exp_env p
                 ~val_type:prim_vd.val_type argl ~transformed_jsx))
        | Some builtin ->
          warn_polymorphic_comparison e.exp_loc builtin argl;
          wrap (mk_builtin builtin argl e.exp_loc))))
  | Texp_apply {funct; args = oargs; partial; transformed_jsx} ->
    let inlined, funct =
      Translattribute.get_and_remove_inlined_attribute funct
    in
    let uncurried_partial_application =
      (* In case of partial application foo(args, ...) when some args are missing,
         get the arity *)
      if partial then
        let arity_opt = Ctype.get_arity funct.exp_env funct.exp_type in
        match arity_opt with
        | Some arity ->
          let real_args = List.filter (fun (_, x) -> Option.is_some x) oargs in
          if arity > List.length real_args then Some arity else None
        | None -> None
      else None
    in
    transl_apply ~inlined ~uncurried_partial_application ~transformed_jsx
      (transl_exp funct) oargs e.exp_loc
  | Texp_match (arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try (body, pat_expr_list) ->
    let id = Typecore.name_pattern "exn" pat_expr_list in
    let handler = Matching.for_trywith (Lvar id) (transl_cases pat_expr_list) in
    let id, handler = pack_trywith_exn id handler in
    Ltrywith (transl_exp body, id, handler)
  | Texp_tuple el -> (
    let ll = transl_list el in
    try Lconst (Const_block (Blk_tuple, List.map extract_constant ll))
    with Not_constant -> Lprim (Pmakeblock Blk_tuple, ll, e.exp_loc))
  | Texp_construct ({txt = Lident "false"}, _, []) -> Lconst Const_js_false
  | Texp_construct ({txt = Lident "true"}, _, []) -> Lconst Const_js_true
  | Texp_construct (_, cstr, args) -> (
    let ll = transl_list args in
    if cstr.cstr_inlined <> None then
      match ll with
      | [x] -> x
      | _ -> assert false
    else
      match cstr.cstr_kind with
      | Ordinary_constructor _ when cstr.cstr_args = [] ->
        Lconst
          (if Datarepr.constructor_has_optional_shape cstr then const_shape_none
           else
             const_constructor
               (match Datarepr.constructor_case cstr with
               | Constant tag -> tag
               | Block _ -> assert false))
      | Ordinary_constructor _ -> (
        let runtime =
          match Datarepr.constructor_case cstr with
          | Block {runtime} -> runtime
          | Constant _ -> assert false
        in
        if Datarepr.constructor_is_unboxed cstr then
          match ll with
          | [value] -> value
          | _ -> assert false
        else if Datarepr.constructor_has_optional_shape cstr then
          let value =
            match ll with
            | [value] -> value
            | _ -> assert false
          in
          let primitive : Lambda.primitive =
            match args with
            | [arg]
              when Typeopt.type_cannot_contain_undefined arg.exp_type
                     arg.exp_env ->
              Psome_not_nest
            | _ -> Psome
          in
          try Lconst (Const_some (extract_constant value))
          with Not_constant -> Lprim (primitive, ll, e.exp_loc)
        else
          let tag_info : Lambda.tag_info =
            Blk_constructor
              {
                name = cstr.cstr_name;
                num_nonconst = num_nonconst_constructors cstr;
                runtime;
              }
          in
          try Lconst (Const_block (tag_info, List.map extract_constant ll))
          with Not_constant -> Lprim (Pmakeblock tag_info, ll, e.exp_loc))
      | Extension_constructor path ->
        Lprim
          ( Pmakeblock Blk_extension,
            transl_extension_path e.exp_env path :: ll,
            e.exp_loc ))
  | Texp_extension_constructor (_, path) -> transl_extension_path e.exp_env path
  | Texp_variant (l, arg) -> (
    match arg with
    | None -> Lconst (const_polyvar l)
    | Some arg -> (
      let lam = transl_exp arg in
      let name = const_polyvar_name l in
      try Lconst (Const_block (Blk_poly_var, [name; extract_constant lam]))
      with Not_constant ->
        Lprim (Pmakeblock Blk_poly_var, [Lconst name; lam], e.exp_loc)))
  | Texp_record {fields; representation; extended_expression} ->
    transl_record e.exp_loc e.exp_env fields representation extended_expression
  | Texp_field (arg, _, lbl) -> (
    let targ = transl_exp arg in
    match lbl.lbl_repres with
    | Record_float_unused -> assert false
    | Record_regular ->
      Lprim (Pfield (lbl.lbl_pos, Lambda.fld_record lbl), [targ], e.exp_loc)
    | Record_inlined _ ->
      Lprim
        (Pfield (lbl.lbl_pos, Lambda.fld_record_inline lbl), [targ], e.exp_loc)
    | Record_unboxed _ -> targ
    | Record_extension ->
      Lprim
        ( Pfield (lbl.lbl_pos + 1, Lambda.fld_record_extension lbl),
          [targ],
          e.exp_loc ))
  | Texp_setfield (arg, _, lbl, newval) ->
    let access =
      match lbl.lbl_repres with
      | Record_float_unused -> assert false
      | Record_regular -> Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
      | Record_inlined _ ->
        Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
      | Record_unboxed _ -> assert false
      | Record_extension ->
        Psetfield (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
    in
    Lprim (access, [transl_exp arg; transl_exp newval], e.exp_loc)
  | Texp_array expr_list ->
    let ll = transl_list expr_list in
    Lprim (Pmakearray, ll, e.exp_loc)
  | Texp_ifthenelse (cond, ifso, Some ifnot) ->
    Lifthenelse (transl_exp cond, transl_exp ifso, transl_exp ifnot)
  | Texp_ifthenelse (cond, ifso, None) ->
    Lifthenelse (transl_exp cond, transl_exp ifso, lambda_unit)
  | Texp_sequence (expr1, expr2) ->
    Lsequence (transl_exp expr1, transl_exp expr2)
  | Texp_break -> Lbreak
  | Texp_continue -> Lcontinue
  | Texp_while (cond, body) -> Lwhile (transl_exp cond, transl_exp body)
  | Texp_for (param, _, low, high, dir, body) ->
    Lfor (param, transl_exp low, transl_exp high, dir, transl_exp body)
  | Texp_for_of (param, _, iterable, body) ->
    Lfor_of (param, transl_exp iterable, transl_exp body)
  | Texp_for_await_of (param, _, iterable, body) ->
    Lfor_await_of (param, transl_exp iterable, transl_exp body)
  | Texp_object_literal fields ->
    let labels =
      List.map
        (fun ((s : string Asttypes.loc), _) ->
          {
            External_arg_spec.obj_arg_label = External_arg_spec.obj_label s.txt;
            obj_arg_type = External_arg_spec.Nothing;
          })
        fields
    in
    Lprim
      ( Pjs_object_create labels,
        List.map (fun (_, field) -> transl_exp field) fields,
        e.exp_loc )
  | Texp_object_get (expr, nm) ->
    Lprim (Pjs_object_get nm.txt, [transl_exp expr], e.exp_loc)
  | Texp_object_set (expr, nm, value) ->
    Lprim (Pjs_object_set nm.txt, [transl_exp expr; transl_exp value], e.exp_loc)
  | Texp_letmodule (id, _loc, modl, body) ->
    let defining_expr = !transl_module Tcoerce_none None modl in
    Llet (Strict, Pgenval, id, defining_expr, transl_exp body)
  | Texp_letexception (cd, body) ->
    Llet
      ( Strict,
        Pgenval,
        cd.ext_id,
        transl_extension_constructor e.exp_env None cd,
        transl_exp body )
  | Texp_pack modl -> !transl_module Tcoerce_none None modl
  | Texp_assert {exp_desc = Texp_construct (_, {cstr_name = "false"}, _)} ->
    if !Clflags.no_assert_false then Lambda.lambda_assert_false
    else assert_failed e
  | Texp_assert cond ->
    if !Clflags.noassert then lambda_unit
    else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)

and transl_list expr_list = List.map transl_exp expr_list

and transl_guard guard rhs =
  let expr = transl_exp rhs in
  match guard with
  | None -> expr
  | Some cond -> Lifthenelse (transl_exp cond, expr, staticfail)

and transl_case {c_lhs; c_guard; c_rhs} = (c_lhs, transl_guard c_guard c_rhs)

and transl_cases cases = List.map transl_case cases

and transl_apply ?(inlined = Default_inline)
    ?(uncurried_partial_application = None) ?(transformed_jsx = false) lam sargs
    loc =
  let lapply ap_func ap_args =
    Lapply
      {
        ap_loc = loc;
        ap_func;
        ap_args;
        ap_inlined = inlined;
        ap_transformed_jsx = transformed_jsx;
      }
  in
  let rec build_apply lam args = function
    | (None, optional) :: l ->
      let defs = ref [] in
      let protect name lam =
        match lam with
        | Lvar _ | Lconst _ -> lam
        | _ ->
          let id = Ident.create name in
          defs := (id, lam) :: !defs;
          Lvar id
      in
      let args, args' =
        if List.for_all (fun (_, opt) -> opt) args then ([], args)
        else (args, [])
      in
      let lam = if args = [] then lam else lapply lam (List.rev_map fst args) in
      let handle = protect "func" lam
      and l = List.map (fun (arg, opt) -> (may_map (protect "arg") arg, opt)) l
      and id_arg = Ident.create "param" in
      let body =
        match build_apply handle ((Lvar id_arg, optional) :: args') l with
        | Lfunction {params = ids; body = lam; attr; loc} ->
          Lfunction {params = id_arg :: ids; body = lam; attr; loc}
        | lam ->
          Lfunction
            {
              params = [id_arg];
              body = lam;
              attr = default_function_attribute;
              loc;
            }
      in
      List.fold_left
        (fun body (id, lam) -> Llet (Strict, Pgenval, id, lam, body))
        body !defs
    | (Some arg, optional) :: l -> build_apply lam ((arg, optional) :: args) l
    | [] -> lapply lam (List.rev_map fst args)
  in
  match uncurried_partial_application with
  | Some arity ->
    let extra_arity = arity - List.length sargs in
    let none_ids = ref [] in
    let args =
      Ext_list.filter_map sargs (function
        | _, Some e -> Some (transl_exp e)
        | _, None ->
          let id_arg = Ident.create "none" in
          none_ids := id_arg :: !none_ids;
          Some (Lvar id_arg))
    in
    let extra_ids =
      Array.init extra_arity (fun _ -> Ident.create "extra") |> Array.to_list
    in
    let extra_args = Ext_list.map extra_ids (fun id -> Lvar id) in
    let ap_args = args @ extra_args in
    let l0 =
      Lapply
        {
          ap_func = lam;
          ap_args;
          ap_inlined = inlined;
          ap_loc = loc;
          ap_transformed_jsx = transformed_jsx;
        }
    in
    Lfunction
      {
        params = List.rev_append !none_ids extra_ids;
        body = l0;
        attr = default_function_attribute;
        loc;
      }
  | _ ->
    (build_apply lam []
       (List.map
          (fun (l, x) -> (may_map transl_exp x, Btype.is_optional l))
          sargs)
      : Lambda.lambda)

and transl_function loc (params : function_param list) body =
  match params with
  | [] -> assert false
  | [{fp_param; fp_pat; fp_partial}] ->
    ( [fp_param],
      Matching.for_function loc None (Lvar fp_param)
        [(fp_pat, transl_exp body)]
        fp_partial,
      is_base_type body.exp_env body.exp_type Predef.path_unit )
  | {fp_param; fp_pat; fp_partial} :: rest ->
    let lparams, lbody, return_unit = transl_function loc rest body in
    ( fp_param :: lparams,
      Matching.for_function loc None (Lvar fp_param)
        [(fp_pat, lbody)]
        fp_partial,
      return_unit )

and transl_let ~js_hoist rec_flag pat_expr_list body =
  match rec_flag with
  | Nonrecursive ->
    let rec transl = function
      | [] -> body
      | {vb_pat = pat; vb_expr = expr; vb_attributes = attr; vb_loc} :: rem ->
        let lam =
          Builtin_attributes.warning_scope ~ppwarning:false attr (fun () ->
              transl_exp expr)
        in
        let lam = Translattribute.add_inline_attribute lam vb_loc attr in
        mark_js_hoisted_pattern ~js_hoist attr pat lam;
        Matching.for_let pat.pat_loc lam pat (transl rem)
    in
    transl pat_expr_list
  | Recursive ->
    let transl_case {vb_expr = expr; vb_attributes; vb_loc; vb_pat = pat} =
      let id =
        match pat.pat_desc with
        | Tpat_var (id, _) -> id
        | Tpat_alias ({pat_desc = Tpat_any}, id, _) -> id
        | _ -> assert false
        (* Illegal_letrec_pat
           Only variables are allowed as left-hand side of `let rec'
        *)
      in
      let lam =
        Builtin_attributes.warning_scope ~ppwarning:false vb_attributes
          (fun () -> transl_exp expr)
      in
      let lam = Translattribute.add_inline_attribute lam vb_loc vb_attributes in
      mark_js_hoisted_pattern ~js_hoist vb_attributes pat lam;
      (id, lam)
    in
    Lambda_scc.bind_rec (Ext_list.map pat_expr_list transl_case) body

and transl_record loc env fields repres opt_init_expr =
  match (opt_init_expr, repres, fields) with
  | _ -> (
    let size = Array.length fields in
    let optional =
      Ext_array.exists fields (fun (ld, _, _) -> ld.lbl_optional)
    in
    (* Determine if there are "enough" fields (only relevant if this is a
       functional-style record update *)
    let no_init =
      match opt_init_expr with
      | None -> true
      | _ -> false
    in
    if
      no_init || (size < 20 && not optional)
      (* TODO: More strategies
         3 + 2 * List.length lbl_expr_list >= size (density)
      *)
    then
      (* Allocate new record with given fields (and remaining fields
         taken from init_expr if any *)
      let init_id = Ident.create "init" in
      let lv =
        Array.mapi
          (fun i (lbl, definition, _) ->
            match definition with
            | Kept _ ->
              let access =
                match repres with
                | Record_float_unused -> assert false
                | Record_regular -> Pfield (i, Lambda.fld_record lbl)
                | Record_inlined _ -> Pfield (i, Lambda.fld_record_inline lbl)
                | Record_unboxed _ -> assert false
                | Record_extension ->
                  Pfield (i + 1, Lambda.fld_record_extension lbl)
              in
              Lprim (access, [Lvar init_id], loc)
            | Overridden (_lid, expr) -> transl_exp expr)
          fields
      in
      let ll = Array.to_list lv in
      let mut =
        if Array.exists (fun (lbl, _, _) -> lbl.lbl_mut = Mutable) fields then
          Mutable
        else Immutable
      in
      let lam =
        try
          if mut = Mutable then raise Not_constant;
          let cl = List.map extract_constant ll in
          match repres with
          | Record_float_unused -> assert false
          | Record_regular ->
            Lconst (Const_block (Lambda.blk_record fields mut, cl))
          | Record_inlined {name; representation} ->
            let runtime =
              match Variant_runtime.representation representation with
              | Block {runtime} -> runtime
              | Constant _ -> assert false
            in
            let num_nonconsts =
              Variant_runtime.num_blocks
                (Variant_runtime.get_layout representation.variant)
            in
            Lconst
              (Const_block
                 ( Lambda.blk_record_inlined fields name num_nonconsts ~runtime
                     mut,
                   cl ))
          | Record_unboxed _ ->
            Lconst
              (match cl with
              | [v] -> v
              | _ -> assert false)
          | Record_extension -> raise Not_constant
        with Not_constant -> (
          match repres with
          | Record_regular ->
            Lprim (Pmakeblock (Lambda.blk_record fields mut), ll, loc)
          | Record_float_unused -> assert false
          | Record_inlined {name; representation} ->
            let runtime =
              match Variant_runtime.representation representation with
              | Block {runtime} -> runtime
              | Constant _ -> assert false
            in
            let num_nonconsts =
              Variant_runtime.num_blocks
                (Variant_runtime.get_layout representation.variant)
            in
            Lprim
              ( Pmakeblock
                  (Lambda.blk_record_inlined fields name num_nonconsts ~runtime
                     mut),
                ll,
                loc )
          | Record_unboxed _ -> (
            match ll with
            | [v] -> v
            | _ -> assert false)
          | Record_extension ->
            let path =
              let label, _, _ = fields.(0) in
              match label.lbl_res.desc with
              | Tconstr (p, _, _) -> p
              | _ -> assert false
            in
            let slot = transl_extension_path env path in
            Lprim
              (Pmakeblock (Lambda.blk_record_ext fields mut), slot :: ll, loc))
      in
      match opt_init_expr with
      | None -> lam
      | Some init_expr ->
        Llet (Strict, Pgenval, init_id, transl_exp init_expr, lam)
    else
      (* Take a shallow copy of the init record, then mutate the fields
         of the copy *)
      let copy_id = Ident.create "newrecord" in
      let update_field cont (lbl, definition, _opt) =
        match definition with
        | Kept _type -> cont
        | Overridden (_lid, expr) ->
          let upd =
            match repres with
            | Record_float_unused -> assert false
            | Record_regular ->
              Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
            | Record_inlined _ ->
              Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
            | Record_unboxed _ -> assert false
            | Record_extension ->
              Psetfield (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
          in
          Lsequence (Lprim (upd, [Lvar copy_id; transl_exp expr], loc), cont)
      in
      match opt_init_expr with
      | None -> assert false
      | Some init_expr ->
        Llet
          ( Strict,
            Pgenval,
            copy_id,
            Lprim (Pduprecord, [transl_exp init_expr], loc),
            Array.fold_left update_field (Lvar copy_id) fields ))

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = Typecore.name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    let exn_handler = Matching.for_trywith (Lvar id) exn_cases in
    let id, exn_handler = pack_trywith_exn id exn_handler in
    Lstaticcatch
      ( Ltrywith (Lstaticraise (static_exception_id, body), id, exn_handler),
        (static_exception_id, val_ids),
        handler )
  in
  match (arg, exn_cases) with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> Typecore.name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list argl) val_ids
      (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
    Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
    let val_id = Typecore.name_pattern "val" pat_expr_list in
    static_catch
      [transl_exp arg]
      [val_id]
      (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)

open Format

let report_error ppf = function
  | Unknown_builtin_primitive prim_name ->
    fprintf ppf "Unknown builtin primitive \"%s\"" prim_name

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
