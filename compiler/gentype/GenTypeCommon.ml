module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module Config = GenTypeConfig

module DocString = struct
  type t = string option
  let render t =
    match t with
    | None | Some "" -> ""
    | Some doc_string -> "/** " ^ String.trim doc_string ^ " */\n"
  let empty = None
  let has_content doc_string = Option.is_some doc_string
end

let log_not_implemented x =
  if !Debug.not_implemented then Log_.item "Not Implemented: %s\n" x

type optional = Mandatory | Optional
type mutable_ = Immutable | Mutable

type label_js =
  | NullLabel
  | UndefinedLabel
  | BoolLabel of bool
  | FloatLabel of string
  | IntLabel of string
  | StringLabel of string

type case = {label_js: label_js}

let is_js_safe_property_name name =
  name = ""
  || (match name.[0] [@doesNotRaise] with
     | 'A' .. 'z' -> true
     | _ -> false)
     && name
        |> String.for_all (function
             | 'A' .. 'z' | '0' .. '9' -> true
             | _ -> false)

let is_number s =
  let len = String.length s in
  len > 0
  && (match len > 1 with
     | true -> (s.[0] [@doesNotRaise]) > '0'
     | false -> true)
  &&
  let res = ref true in
  for i = 0 to len - 1 do
    match s.[i] [@doesNotRaise] with
    | '0' .. '9' -> ()
    | _ -> res := false
  done;
  res.contents

let label_js_to_string case =
  match case.label_js with
  | NullLabel -> "null"
  | UndefinedLabel -> "undefined"
  | BoolLabel b -> b |> string_of_bool
  | FloatLabel s -> s
  | IntLabel i -> i
  | StringLabel s -> s |> EmitText.quotes

type closed_flag = Open | Closed | Inline

type type_ =
  | Array of type_ * mutable_
  | Dict of type_
  | Function of function_
  | Ident of ident
  | Null of type_
  | Nullable of type_
  | Object of closed_flag * fields
  | Option of type_
  | Promise of type_
  | Tuple of type_ list
  | TypeVar of string
  | Variant of variant (* ordinary and polymorphic variants *)

and fields = field list
and arg_type = {a_name: string; a_type: type_}

and field = {
  mutable_: mutable_;
  name_js: string;
  optional: optional;
  type_: type_;
  doc_string: DocString.t;
}

and function_ = {
  arg_types: arg_type list;
  ret_type: type_;
  type_vars: string list;
}

and ident = {builtin: bool; name: string; type_args: type_ list}

and variant = {
  inherits: type_ list;
  no_payloads: case list;
  payloads: payload list;
  polymorphic: bool; (* If true, this is a polymorphic variant *)
  tag: string option; (* The name of the tag field at runtime *)
  unboxed: bool;
}

and payload = {case: case; t: type_}

type label = Nolabel | Label of string | OptLabel of string

type dep =
  | External of string
  | Internal of ResolvedName.t
  | Dot of dep * string

module ScopedPackage =
(* Taken from ext_namespace.ml in bukclescript *)
struct
  let namespace_of_package_name (s : string) : string =
    let len = String.length s in
    let buf = Buffer.create len in
    let add capital ch =
      Buffer.add_char buf (if capital then Char.uppercase_ascii ch else ch)
    in
    let rec aux capital off len =
      if off >= len then ()
      else
        let ch = String.unsafe_get s off in
        match ch with
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          add capital ch;
          aux false (off + 1) len
        | '/' | '-' -> aux true (off + 1) len
        | _ -> aux capital (off + 1) len
    in
    aux true 0 len;
    Buffer.contents buf

  (** @demo/some-library -> DemoSomelibrary *)
  let package_name_to_generated_module_name package_name =
    if String.contains package_name '/' then
      Some (package_name |> namespace_of_package_name)
    else None

  let is_generated_module id ~(config : Config.t) =
    config.bs_dependencies
    |> List.exists (fun package_name ->
           package_name |> package_name_to_generated_module_name
           = Some (id |> Ident.name))

  (** (Common, DemoSomelibrary) -> Common-DemoSomelibrary *)
  let add_generated_module s ~generated_module =
    s ^ "-" ^ Ident.name generated_module

  (** Common-DemoSomelibrary -> Common *)
  let remove_generated_module s =
    match s |> String.split_on_char '-' with
    | [name; _scope] -> name
    | _ -> s
end

let rec dep_to_string dep =
  match dep with
  | External name -> name |> ScopedPackage.remove_generated_module
  | Internal resolved_name -> resolved_name |> ResolvedName.to_string
  | Dot (d, s) -> dep_to_string d ^ "_" ^ s

let rec dep_to_resolved_name (dep : dep) =
  match dep with
  | External name -> name |> ResolvedName.from_string
  | Internal resolved_name -> resolved_name
  | Dot (p, s) -> ResolvedName.dot s (p |> dep_to_resolved_name)

let create_variant ~inherits ~no_payloads ~payloads ~polymorphic ~tag ~unboxed =
  Variant {inherits; no_payloads; payloads; polymorphic; tag; unboxed}

let ident ?(builtin = true) ?(type_args = []) name =
  Ident {builtin; name; type_args}

let sanitize_type_name name =
  name
  |> String.map (function
       | '\'' -> '_'
       | c -> c)
let unknown = ident "unknown"
let bigint_t = ident "BigInt"
let boolean_t = ident "boolean"
let date_t = ident "Date"
let map_t (x, y) = ident ~type_args:[x; y] "Map"
let number_t = ident "number"
let regexp_t = ident "RegExp"
let set_t x = ident ~type_args:[x] "Set"
let string_t = ident "string"
let unit_t = ident "void"
let weakmap_t (x, y) = ident ~type_args:[x; y] "WeakMap"
let weakset_t x = ident ~type_args:[x] "WeakSet"

module NodeFilename = struct
  include Filename

  (* Force "/" separator. *)
  let dir_sep = "/"

  module Path : sig
    type t

    val normalize : string -> t
    val concat : t -> string -> t
    val to_string : t -> string
  end = struct
    type t = string

    let normalize path : t =
      match Sys.os_type with
      | "Win32" -> path |> String.split_on_char '\\' |> String.concat dir_sep
      | _ -> path

    let to_string path = path
    let length path = String.length path

    let concat dirname filename =
      let is_dir_sep s i =
        let c = (s.[i] [@doesNotRaise]) in
        c = '/' || c = '\\' || c = ':'
      in
      let l = length dirname in
      if l = 0 || is_dir_sep dirname (l - 1) then dirname ^ filename
      else dirname ^ dir_sep ^ filename
  end

  let concat (dirname : string) filename =
    let open Path in
    Path.concat (normalize dirname) filename |> to_string
end
