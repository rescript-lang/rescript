type item = SharedTypes.ScopeTypes.item

type t = item list

open SharedTypes.ScopeTypes

let itemToString item =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  match item with
  | Constructor (s, loc) -> "Constructor " ^ s ^ " " ^ Loc.toString loc
  | Field (s, loc) -> "Field " ^ s ^ " " ^ Loc.toString loc
  | Open sl -> "Open " ^ list sl
  | Module (s, loc) -> "Module " ^ s ^ " " ^ Loc.toString loc
  | Value (s, loc, _, _) -> "Value " ^ s ^ " " ^ Loc.toString loc
  | Type (s, loc) -> "Type " ^ s ^ " " ^ Loc.toString loc
  | Include (s, loc) -> "Include " ^ s ^ " " ^ Loc.toString loc
[@@live]

let create () : t = []
let addConstructor ~name ~loc x = Constructor (name, loc) :: x
let addField ~name ~loc x = Field (name, loc) :: x
let addModule ~name ~loc x = Module (name, loc) :: x
let addOpen ~lid x = Open (Utils.flattenLongIdent lid @ ["place holder"]) :: x
let addValue ~name ~loc ?contextPath x =
  let showDebug = !Cfg.debugFollowCtxPath in
  (if showDebug then
     match contextPath with
     | None -> Printf.printf "adding value '%s', no ctxPath\n" name
     | Some contextPath ->
       if showDebug then
         Printf.printf "adding value '%s' with ctxPath: %s\n" name
           (SharedTypes.Completable.contextPathToString contextPath));
  Value (name, loc, contextPath, x) :: x
let addType ~name ~loc x = Type (name, loc) :: x
let addInclude ~name ~loc x = Include (name, loc) :: x

let iterValuesBeforeFirstOpen f x =
  let rec loop items =
    match items with
    | Value (s, loc, contextPath, scope) :: rest ->
      f s loc contextPath scope;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iterValuesAfterFirstOpen f x =
  let rec loop foundOpen items =
    match items with
    | Value (s, loc, contextPath, scope) :: rest ->
      if foundOpen then f s loc contextPath scope;
      loop foundOpen rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop foundOpen rest
    | [] -> ()
  in
  loop false x

let iterConstructorsBeforeFirstOpen f x =
  let rec loop items =
    match items with
    | Constructor (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iterConstructorsAfterFirstOpen f x =
  let rec loop foundOpen items =
    match items with
    | Constructor (s, loc) :: rest ->
      if foundOpen then f s loc;
      loop foundOpen rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop foundOpen rest
    | [] -> ()
  in
  loop false x

let iterTypesBeforeFirstOpen f x =
  let rec loop items =
    match items with
    | Type (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iterTypesAfterFirstOpen f x =
  let rec loop foundOpen items =
    match items with
    | Type (s, loc) :: rest ->
      if foundOpen then f s loc;
      loop foundOpen rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop foundOpen rest
    | [] -> ()
  in
  loop false x

let iterModulesBeforeFirstOpen f x =
  let rec loop items =
    match items with
    | Module (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iterModulesAfterFirstOpen f x =
  let rec loop foundOpen items =
    match items with
    | Module (s, loc) :: rest ->
      if foundOpen then f s loc;
      loop foundOpen rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop foundOpen rest
    | [] -> ()
  in
  loop false x

let iterIncludes f x =
  let rec loop items =
    match items with
    | [] -> ()
    | Include (s, loc) :: rest ->
      f s loc;
      loop rest
    | _ :: rest -> loop rest
  in
  loop x

let getRawOpens x =
  x
  |> Utils.filterMap (function
       | Open path -> Some path
       | _ -> None)
