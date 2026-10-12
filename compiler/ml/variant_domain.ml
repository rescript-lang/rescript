open Variant_runtime

let invalid message = invalid_arg ("Variant_domain: " ^ message)

(* Work in decimal digits rather than machine integers: bigint literals have
   arbitrary precision even when the compiler runs on a 32-bit platform. *)
let decimal_step digits base digit =
  let carry = ref digit in
  let output = Bytes.of_string digits in
  for i = String.length digits - 1 downto 0 do
    let value = ((Char.code digits.[i] - 48) * base) + !carry in
    Bytes.set output i (Char.chr (48 + (value mod 10)));
    carry := value / 10
  done;
  let prefix = if !carry = 0 then "" else string_of_int !carry in
  prefix ^ Bytes.to_string output

(* [Bigint_utils.parse_bigint] formats emitted spellings, retaining base
   prefixes and accepting repeated signs. Domain equality needs stricter
   validation and one decimal representation for each runtime value. *)
let normalize_bigint value =
  let value = String.concat "" (String.split_on_char '_' value) in
  let len = String.length value in
  let len = if len > 0 && value.[len - 1] = 'n' then len - 1 else len in
  let negative = len > 0 && value.[0] = '-' in
  let start =
    if len > 0 && (value.[0] = '-' || value.[0] = '+') then 1 else 0
  in
  let base, start =
    if len > start + 1 && value.[start] = '0' then
      match value.[start + 1] with
      | 'x' | 'X' -> (16, start + 2)
      | 'o' | 'O' -> (8, start + 2)
      | 'b' | 'B' -> (2, start + 2)
      | _ -> (10, start)
    else (10, start)
  in
  if start >= len then invalid "empty bigint literal";
  let digits = ref "0" in
  for i = start to len - 1 do
    let digit =
      match value.[i] with
      | '0' .. '9' as c -> Char.code c - Char.code '0'
      | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
      | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
      | _ -> invalid "invalid bigint literal"
    in
    if digit >= base then invalid "invalid bigint digit";
    digits := decimal_step !digits base digit
  done;
  if negative && !digits <> "0" then "-" ^ !digits else !digits

let normalize_number value =
  match classify_float value with
  | FP_nan | FP_infinite -> invalid "nonfinite number literal"
  | FP_normal | FP_subnormal | FP_zero ->
    if
      value >= -2147483648. && value <= 2147483647. && Float.floor value = value
    then Int (Int32.to_int (Int32.of_float value))
    else Float (Printf.sprintf "%.17g" value)

let normalize_literal = function
  | Int value -> Int (Int32.to_int (Int32.of_int value))
  | Float value ->
    let number =
      try float_of_string value
      with Failure _ -> invalid "invalid number literal"
    in
    normalize_number number
  | BigInt value -> BigInt (normalize_bigint value)
  | (String _ | Bool _ | Null | Undefined) as literal -> literal

let literal_equal left right =
  try
    match (left, right) with
    | Int left, Int right -> Int32.of_int left = Int32.of_int right
    | Int integer, Float number | Float number, Int integer ->
      Int32.to_float (Int32.of_int integer) = float_of_string number
    | Float left, Float right -> float_of_string left = float_of_string right
    | _ -> normalize_literal left = normalize_literal right
  with Invalid_argument _ | Failure _ -> false

let literal_mem literal literals = List.exists (literal_equal literal) literals

let normalize_shape = function
  | FloatType -> IntType
  | UnknownType | ObjectDiscriminant _ -> invalid "invalid domain shape"
  | ( IntType | StringType | BigintType | BooleanType | InstanceType _
    | FunctionType | ObjectType ) as shape ->
    shape

let literal_matches_shape literal shape =
  match (literal, normalize_shape shape) with
  | (Int _ | Float _), IntType
  | String _, StringType
  | BigInt _, BigintType
  | Bool _, BooleanType ->
    true
  | _ -> false

let is_typed_array = function
  | Instance.BigInt64Array | BigUint64Array | Float32Array | Float64Array
  | Int16Array | Int32Array | Int8Array | Uint16Array | Uint32Array | Uint8Array
  | Uint8ClampedArray | TypedArray ->
    true
  | Array | ArrayBuffer | Blob | DataView | Date | File | Promise | RegExp | Set
  | Map | WeakSet | WeakMap ->
    false

let shapes_overlap left right =
  let left = normalize_shape left and right = normalize_shape right in
  left = right
  ||
  match (left, right) with
  | InstanceType Instance.File, InstanceType Instance.Blob
  | InstanceType Instance.Blob, InstanceType Instance.File ->
    true
  | InstanceType Instance.TypedArray, InstanceType instance
  | InstanceType instance, InstanceType Instance.TypedArray ->
    is_typed_array instance
  | _ -> false

let normalize {literals; shapes} =
  let shapes = List.sort_uniq compare (List.map normalize_shape shapes) in
  (* File is a Blob subclass, so retaining File adds no values to a Blob set. *)
  let shapes =
    if List.mem (InstanceType Instance.Blob) shapes then
      List.filter (fun shape -> shape <> InstanceType Instance.File) shapes
    else shapes
  in
  let shapes =
    if List.mem (InstanceType Instance.TypedArray) shapes then
      List.filter
        (function
          | InstanceType Instance.TypedArray -> true
          | InstanceType instance -> not (is_typed_array instance)
          | IntType | StringType | FloatType | BigintType | BooleanType
          | FunctionType | ObjectType | UnknownType | ObjectDiscriminant _ ->
            true)
        shapes
    else shapes
  in
  let literals =
    List.sort_uniq compare (List.map normalize_literal literals)
    |> List.filter (fun literal ->
        not (List.exists (literal_matches_shape literal) shapes))
  in
  {literals; shapes}

let singleton_literal literal = normalize {literals = [literal]; shapes = []}
let singleton_shape shape = normalize {literals = []; shapes = [shape]}

let union left right =
  normalize
    {
      literals = left.literals @ right.literals;
      shapes = left.shapes @ right.shapes;
    }

type overlap =
  | Shared_literal of literal_tag
  | Shared_shape of block_type * block_type
  | Literal_shape of literal_tag * block_type

let overlaps left right =
  let left = normalize left and right = normalize right in
  let shared_literals =
    List.filter
      (fun literal -> literal_mem literal right.literals)
      left.literals
    |> List.map (fun literal -> Shared_literal literal)
  in
  let shared_shapes =
    List.concat_map
      (fun left_shape ->
        List.filter_map
          (fun right_shape ->
            if shapes_overlap left_shape right_shape then
              Some (Shared_shape (left_shape, right_shape))
            else None)
          right.shapes)
      left.shapes
  in
  let literal_shapes literals shapes =
    List.concat_map
      (fun literal ->
        List.filter_map
          (fun shape ->
            if literal_matches_shape literal shape then
              Some (Literal_shape (literal, shape))
            else None)
          shapes)
      literals
  in
  List.sort_uniq compare
    (shared_literals @ shared_shapes
    @ literal_shapes left.literals right.shapes
    @ literal_shapes right.literals left.shapes)

let contains_literal domain literal =
  let domain = normalize domain in
  literal_mem literal domain.literals
  || List.exists (literal_matches_shape literal) domain.shapes

let is_empty domain =
  let domain = normalize domain in
  domain.literals = [] && domain.shapes = []

let literal_to_string literal =
  match normalize_literal literal with
  | String value -> Printf.sprintf "%S" value
  | Int value -> string_of_int value
  | Float value -> value
  | BigInt value -> value ^ "n"
  | Bool value -> string_of_bool value
  | Null -> "null"
  | Undefined -> "undefined"

let shape_to_string shape =
  match normalize_shape shape with
  | IntType -> "number"
  | StringType -> "string"
  | BigintType -> "bigint"
  | BooleanType -> "boolean"
  | InstanceType instance -> Variant_runtime.Instance.to_string instance
  | FunctionType -> "function"
  | ObjectType -> "object"
  | FloatType | UnknownType | ObjectDiscriminant _ -> assert false
