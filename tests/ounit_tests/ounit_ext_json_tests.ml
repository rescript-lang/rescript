let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
type t = Ext_json_noloc.t
let rec equal (x : t) (y : t) =
  match x with
  | Null -> (
    (* [%p? Null _ ] *)
    match y with
    | Null -> true
    | _ -> false)
  | Str str -> (
    match y with
    | Str str2 -> str = str2
    | _ -> false)
  | Flo flo -> (
    match y with
    | Flo flo2 -> flo = flo2
    | _ -> false)
  | True -> (
    match y with
    | True -> true
    | _ -> false)
  | False -> (
    match y with
    | False -> true
    | _ -> false)
  | Arr content -> (
    match y with
    | Arr content2 -> Ext_array.for_all2_no_exn content content2 equal
    | _ -> false)
  | Obj map -> (
    match y with
    | Obj map2 ->
      let xs =
        Map_string.bindings map |> List.sort (fun (a, _) (b, _) -> compare a b)
      in
      let ys =
        Map_string.bindings map2 |> List.sort (fun (a, _) (b, _) -> compare a b)
      in
      Ext_list.for_all2_no_exn xs ys (fun (k0, v0) (k1, v1) ->
          k0 = k1 && equal v0 v1)
    | _ -> false)

open Ext_json_parse
let ( |? ) m (key, cb) = m |> Ext_json.test key cb

let rec strip (x : Ext_json_types.t) : Ext_json_noloc.t =
  let open Ext_json_noloc in
  match x with
  | True _ -> true_
  | False _ -> false_
  | Null _ -> null
  | Flo {flo = s} -> flo s
  | Str {str = s} -> str s
  | Arr {content} -> arr (Array.map strip content)
  | Obj {map} -> obj (Map_string.map map strip)

let id_parsing_serializing x =
  let normal_s =
    Ext_json_noloc.to_string @@ strip @@ Ext_json_parse.parse_json_from_string x
  in
  let normal_ss =
    Ext_json_noloc.to_string @@ strip
    @@ Ext_json_parse.parse_json_from_string normal_s
  in
  if normal_s <> normal_ss then (
    prerr_endline "ERROR";
    prerr_endline normal_s;
    prerr_endline normal_ss);
  OUnit.assert_equal ~cmp:(fun (x : string) y -> x = y) normal_s normal_ss

let id_parsing_x2 x =
  let stru = Ext_json_parse.parse_json_from_string x |> strip in
  let normal_s = Ext_json_noloc.to_string stru in
  let normal_ss = strip (Ext_json_parse.parse_json_from_string normal_s) in
  if equal stru normal_ss then true
  else (
    prerr_endline "ERROR";
    prerr_endline normal_s;
    Format.fprintf Format.err_formatter "%a@.%a@." Ext_obj.pp_any stru
      Ext_obj.pp_any normal_ss;

    prerr_endline (Ext_json_noloc.to_string normal_ss);
    false)

let test_data =
  [
    {|
      {}
      |};
    {| [] |};
    {| [1,2,3]|};
    {| ["x", "y", 1,2,3 ]|};
    {| { "x" :  3, "y" : "x", "z" : [1,2,3, "x"] }|};
    {| {"x " : true , "y" : false , "z\"" : 1} |};
  ]
exception Parse_error
let suites =
  __FILE__
  >::: [
         (__LOC__ >:: fun _ -> List.iter id_parsing_serializing test_data);
         ( __LOC__ >:: fun _ ->
           List.iteri
             (fun i x ->
               OUnit.assert_bool (__LOC__ ^ string_of_int i) (id_parsing_x2 x))
             test_data );
         ( "empty_json" >:: fun _ ->
           let v = parse_json_from_string "{}" in
           match v with
           | Obj {map = v} -> OUnit.assert_equal (Map_string.is_empty v) true
           | _ -> OUnit.assert_failure "should be empty" );
         ( "empty_arr" >:: fun _ ->
           let v = parse_json_from_string "[]" in
           match v with
           | Arr {content = [||]} -> ()
           | _ -> OUnit.assert_failure "should be empty" );
         ( "empty trails" >:: fun _ ->
           ( OUnit.assert_raises Parse_error @@ fun _ ->
             try parse_json_from_string {| [,]|} with _ -> raise Parse_error );
           OUnit.assert_raises Parse_error @@ fun _ ->
           try parse_json_from_string {| {,}|} with _ -> raise Parse_error );
         ( "two trails" >:: fun _ ->
           ( OUnit.assert_raises Parse_error @@ fun _ ->
             try parse_json_from_string {| [1,2,,]|}
             with _ -> raise Parse_error );
           OUnit.assert_raises Parse_error @@ fun _ ->
           try parse_json_from_string {| { "x": 3, ,}|}
           with _ -> raise Parse_error );
         ( "two trails fail" >:: fun _ ->
           OUnit.assert_raises Parse_error @@ fun _ ->
           try parse_json_from_string {| { "x": 3, 2 ,}|}
           with _ -> raise Parse_error );
         ( "trail comma obj" >:: fun _ ->
           let v = parse_json_from_string {| { "x" : 3 , }|} in
           let v1 = parse_json_from_string {| { "x" : 3 , }|} in
           let test (v : Ext_json_types.t) =
             match v with
             | Obj {map = v} ->
               v |? ("x", `Flo (fun x -> OUnit.assert_equal x "3")) |> ignore
             | _ -> OUnit.assert_failure "trail comma"
           in
           test v;
           test v1 );
         ( "trail comma arr" >:: fun _ ->
           let v = parse_json_from_string {| [ 1, 3, ]|} in
           let v1 = parse_json_from_string {| [ 1, 3 ]|} in
           let test (v : Ext_json_types.t) =
             match v with
             | Arr {content = [|Flo {flo = "1"}; Flo {flo = "3"}|]} -> ()
             | _ -> OUnit.assert_failure "trailing comma array"
           in
           test v;
           test v1 );
       ]
