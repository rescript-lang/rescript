type t = string

let curry = "Curry"
let rescript_pervasives = "RescriptPervasives"

let sanitize_id s =
  let s =
    if String.contains s '.' || String.contains s '[' || String.contains s ']'
    then
      s
      |> String.map (function
           | '.' | '[' | ']' -> '_'
           | c -> c)
    else s
  in
  if s <> "" && (s.[0] [@doesNotRaise]) >= 'A' && (s.[0] [@doesNotRaise]) <= 'z'
  then s
  else "_" ^ s

let for_js_file s = sanitize_id s ^ "JS"

let for_inner_module ~file_name ~inner_module_name =
  (file_name |> for_js_file) ^ "." ^ inner_module_name

let from_string_unsafe s = s
let to_string s = s
let compare (s1 : string) s2 = compare s1 s2
let uncapitalize = String.uncapitalize_ascii
