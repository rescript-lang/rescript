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

(* How an operator lowers for one operand type. *)
type lowering =
  | Lower of Lambda.primitive
  | Pass_through  (** the operand is already the result: unary [+] *)

(* [None] means the operand type is not supported by the operator; the payload
   says how the supported ones lower.
   Note: unified op must support int type *)
type specialization = {
  int: lowering;
  bool: lowering option;
  float: lowering option;
  bigint: lowering option;
  string: lowering option;
}

type entry = {
  path: string;
      (** TODO: Maybe it can be a Path.t in Predef instead of string *)
  name: string;
  form: form;
  specialization: specialization;
}

let pervasives_path x = Primitive_modules.pervasives ^ "." ^ x

let entries =
  [|
    {
      path = pervasives_path "~+";
      name = "%plus";
      form = Unary;
      specialization =
        {
          int = Pass_through;
          bool = None;
          float = Some Pass_through;
          bigint = Some Pass_through;
          string = None;
        };
    };
    {
      path = pervasives_path "~-";
      name = "%neg";
      form = Unary;
      specialization =
        {
          int = Lower Pnegint;
          bool = None;
          float = Some (Lower Pnegfloat);
          bigint = Some (Lower Pnegbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "+";
      name = "%add";
      form = Binary;
      specialization =
        {
          int = Lower Paddint;
          bool = None;
          float = Some (Lower Paddfloat);
          bigint = Some (Lower Paddbigint);
          string = Some (Lower Pstringadd);
        };
    };
    {
      path = pervasives_path "-";
      name = "%sub";
      form = Binary;
      specialization =
        {
          int = Lower Psubint;
          bool = None;
          float = Some (Lower Psubfloat);
          bigint = Some (Lower Psubbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "*";
      name = "%mul";
      form = Binary;
      specialization =
        {
          int = Lower Pmulint;
          bool = None;
          float = Some (Lower Pmulfloat);
          bigint = Some (Lower Pmulbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "/";
      name = "%div";
      form = Binary;
      specialization =
        {
          int = Lower Pdivint;
          bool = None;
          float = Some (Lower Pdivfloat);
          bigint = Some (Lower Pdivbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "%";
      name = "%mod";
      form = Binary;
      specialization =
        {
          int = Lower Pmodint;
          bool = None;
          float = Some (Lower Pmodfloat);
          bigint = Some (Lower Pmodbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "<<";
      name = "%lsl";
      form = Binary;
      specialization =
        {
          int = Lower Plslint;
          bool = None;
          float = None;
          bigint = Some (Lower Plslbigint);
          string = None;
        };
    };
    {
      path = pervasives_path ">>";
      name = "%asr";
      form = Binary;
      specialization =
        {
          int = Lower Pasrint;
          bool = None;
          float = None;
          bigint = Some (Lower Pasrbigint);
          string = None;
        };
    };
    {
      path = pervasives_path ">>>";
      name = "%lsr";
      form = Binary;
      specialization =
        {
          int = Lower Plsrint;
          bool = None;
          float = None;
          bigint = None;
          string = None;
        };
    };
    {
      path = pervasives_path "mod";
      name = "%mod";
      form = Binary;
      specialization =
        {
          int = Lower Pmodint;
          bool = None;
          float = Some (Lower Pmodfloat);
          bigint = Some (Lower Pmodbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "**";
      name = "%pow";
      form = Binary;
      specialization =
        {
          int = Lower Ppowint;
          bool = None;
          float = Some (Lower Ppowfloat);
          bigint = Some (Lower Ppowbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "~~~";
      name = "%bitnot";
      form = Unary;
      specialization =
        {
          int = Lower Pnotint;
          bool = None;
          float = None;
          bigint = Some (Lower Pnotbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "|||";
      name = "%bitor";
      form = Binary;
      specialization =
        {
          int = Lower Porint;
          bool = None;
          float = None;
          bigint = Some (Lower Porbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "^^^";
      name = "%bitxor";
      form = Binary;
      specialization =
        {
          int = Lower Pxorint;
          bool = None;
          float = None;
          bigint = Some (Lower Pxorbigint);
          string = None;
        };
    };
    {
      path = pervasives_path "&&&";
      name = "%bitand";
      form = Binary;
      specialization =
        {
          int = Lower Pandint;
          bool = None;
          float = None;
          bigint = Some (Lower Pandbigint);
          string = None;
        };
    };
  |]

let index_by_path =
  entries |> Array.map (fun entry -> (entry.path, entry)) |> create_hashtable

let index_by_name =
  entries |> Array.map (fun entry -> (entry.name, entry)) |> create_hashtable
