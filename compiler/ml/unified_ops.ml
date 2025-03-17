open Misc

(*
  Unified_ops is for specialization of some primitive operators.

  For example adding two values. We have `+` for ints, `+.` for floats, and `++` for strings.
  That because we don't allow implicit conversion or overloading for operations.

  It is a fundamental property of the ReScript language, but it is far from the best DX we can think of,
  and it became a problem when new primitives like bigint were introduced.

  See discussion: https://github.com/rescript-lang/rescript-compiler/issues/6525

  Unified ops mitigate the problem by adding ad-hoc translation rules on applications of the core built-in operators
  which have form of binary infix ('a -> 'a -> 'a) or unary ('a -> 'a)

  Translation rules should be applied in its application, in both type-level and IR(lambda)-level.

  The rules:

  1. If the lhs type is a primitive type, unify the rhs and the result type to the lhs type.
  2. If the lhs type is not a primitive type but the rhs type is, unify lhs and the result type to the rhs type.
  3. If both lhs type and rhs type is not a primitive type, unify the whole types to the int. 

  Since these are simple ad-hoc translations for primitive applications, we cannot use the result type defined in other contexts.
  So falling back to int type is the simplest behavior that ensures backwards compatibility.

  Actual implementations of translation are colocated into core modules

  You can find it in:
  - Type-level : ml/typecore.ml
  - IR-level   : ml/translcore.ml

  With function name "translate_unified_ops"
*)

type form = Unary | Binary

(* Note: unified op must support int type *)
type specialization = {
  int: Lambda.primitive;
  bool: Lambda.primitive option;
  float: Lambda.primitive option;
  bigint: Lambda.primitive option;
  string: Lambda.primitive option;
}

type entry = {
  path: string;
      (** TODO: Maybe it can be a Path.t in Predef instead of string *)
  name: string;
  form: form;
  specialization: specialization;
}

let builtin x = Primitive_modules.pervasives ^ "." ^ x

let entries =
  [|
    {
      path = builtin "~+";
      name = "%plus";
      form = Unary;
      specialization =
        {
          int = Pidentity;
          bool = None;
          float = Some Pidentity;
          bigint = Some Pidentity;
          string = None;
        };
    };
    {
      path = builtin "~-";
      name = "%neg";
      form = Unary;
      specialization =
        {
          int = Pnegint;
          bool = None;
          float = Some Pnegfloat;
          bigint = Some Pnegbigint;
          string = None;
        };
    };
    {
      path = builtin "+";
      name = "%add";
      form = Binary;
      specialization =
        {
          int = Paddint;
          bool = None;
          float = Some Paddfloat;
          bigint = Some Paddbigint;
          string = Some Pstringadd;
        };
    };
    {
      path = builtin "-";
      name = "%sub";
      form = Binary;
      specialization =
        {
          int = Psubint;
          bool = None;
          float = Some Psubfloat;
          bigint = Some Psubbigint;
          string = None;
        };
    };
    {
      path = builtin "*";
      name = "%mul";
      form = Binary;
      specialization =
        {
          int = Pmulint;
          bool = None;
          float = Some Pmulfloat;
          bigint = Some Pmulbigint;
          string = None;
        };
    };
    {
      path = builtin "/";
      name = "%div";
      form = Binary;
      specialization =
        {
          int = Pdivint;
          bool = None;
          float = Some Pdivfloat;
          bigint = Some Pdivbigint;
          string = None;
        };
    };
    {
      path = builtin "%";
      name = "%mod";
      form = Binary;
      specialization =
        {
          int = Pmodint;
          bool = None;
          float = Some Pmodfloat;
          bigint = Some Pmodbigint;
          string = None;
        };
    };
    {
      path = builtin "mod";
      name = "%mod";
      form = Binary;
      specialization =
        {
          int = Pmodint;
          bool = None;
          float = Some Pmodfloat;
          bigint = Some Pmodbigint;
          string = None;
        };
    };
  |]

let index_by_path =
  entries |> Array.map (fun entry -> (entry.path, entry)) |> create_hashtable

let index_by_name =
  entries |> Array.map (fun entry -> (entry.name, entry)) |> create_hashtable
