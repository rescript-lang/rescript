type version = Jsx_v4
type module_ = React | Generic of {moduleName: string}
type dependencies = string list

type t = {version: version option; module_: module_ option}

let encode_no_nl jsx =
  (match jsx.version with
  | None -> ""
  | Some Jsx_v4 -> "4")
  ^
  match jsx.module_ with
  | None -> ""
  | Some React -> "React"
  | Some (Generic {moduleName}) -> moduleName

let ( .?() ) = Map_string.find_opt
let ( |? ) m (key, cb) = m |> Ext_json.test key cb

let get_list_string_acc (s : Ext_json_types.t array) acc =
  Ext_array.to_list_map_acc s acc (fun x ->
      match x with
      | Str x -> Some x.str
      | _ -> None)

let get_list_string s = get_list_string_acc s []

let from_map map =
  let version : version option ref = ref None in
  let module_ : module_ option ref = ref None in
  map
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_version) with
           | Some (Flo {loc; flo}) -> (
             match flo with
             | "4" -> version := Some Jsx_v4
             | _ -> Bsb_exception.errorf ~loc "Unsupported jsx version %s" flo)
           | Some x ->
             Bsb_exception.config_error x
               "Unexpected input (expect a version number) for jsx version"
           | None -> ()) )
  |? ( Bsb_build_schemas.jsx,
       `Obj
         (fun m ->
           match m.?(Bsb_build_schemas.jsx_module) with
           | Some (Str {str}) -> (
             match str with
             | "react" -> module_ := Some React
             | moduleName -> module_ := Some (Generic {moduleName}))
           | Some x ->
             Bsb_exception.config_error x
               "Unexpected input (jsx module name) for jsx module"
           | None -> ()) )
  |> ignore;
  {version = !version; module_ = !module_}
