let raises_lib_table : (Name.t, Exceptions.t) Hashtbl.t =
  let table = Hashtbl.create 15 in
  let open Exn in
  let belt_array =
    [
      ("getExn", [assert_failure]);
      ("getOrThrow", [assert_failure]);
      ("setExn", [assert_failure]);
      ("setOrThrow", [assert_failure]);
    ]
  in
  let belt_list =
    [
      ("getExn", [not_found]);
      ("getOrThrow", [not_found]);
      ("headExn", [not_found]);
      ("headOrThrow", [not_found]);
      ("tailExn", [not_found]);
      ("tailOrThrow", [not_found]);
    ]
  in
  let belt_map = [("getExn", [not_found]); ("getOrThrow", [not_found])] in
  let belt_mutable_map = belt_map in
  let belt_mutable_queue =
    [
      ("peekExn", [not_found]);
      ("peekOrThrow", [not_found]);
      ("popExn", [not_found]);
      ("popOrThrow", [not_found]);
    ]
  in
  let belt_set = [("getExn", [not_found]); ("getOrThrow", [not_found])] in
  let belt_mutable_set = belt_set in
  let belt_option = [("getExn", [not_found]); ("getOrThrow", [not_found])] in
  let belt_result = [("getExn", [not_found]); ("getOrThrow", [not_found])] in
  let bs_json =
    (* bs-json *)
    [
      ("bool", [decode_error]);
      ("float", [decode_error]);
      ("int", [decode_error]);
      ("string", [decode_error]);
      ("char", [decode_error]);
      ("date", [decode_error]);
      ("nullable", [decode_error]);
      ("nullAs", [decode_error]);
      ("array", [decode_error]);
      ("list", [decode_error]);
      ("pair", [decode_error]);
      ("tuple2", [decode_error]);
      ("tuple3", [decode_error]);
      ("tuple4", [decode_error]);
      ("dict", [decode_error]);
      ("field", [decode_error]);
      ("at", [decode_error; invalid_argument]);
      ("oneOf", [decode_error]);
      ("either", [decode_error]);
    ]
  in
  let stdlib =
    [
      ("panic", [js_exn]);
      ("assertEqual", [js_exn]);
      ("/", [division_by_zero]);
      ("mod", [division_by_zero]);
    ]
  in
  let stdlib_big_int =
    [("fromStringOrThrow", [js_exn]); ("fromFloatOrThrow", [js_exn])]
  in
  let stdlib_bool = [("fromStringOrThrow", [invalid_argument])] in
  let stdlib_js_error =
    [
      ("EvalError.throwWithMessage", [js_exn]);
      ("RangeError.throwWithMessage", [js_exn]);
      ("ReferenceError.throwWithMessage", [js_exn]);
      ("SyntaxError.throwWithMessage", [js_exn]);
      ("TypeError.throwWithMessage", [js_exn]);
      ("URIError.throwWithMessage", [js_exn]);
      ("panic", [js_exn]);
      ("throw", [js_exn]);
      ("throwWithMessage", [js_exn]);
    ]
  in
  let stdlib_json = [("parseOrThrow", [js_exn]); ("stringifyAny", [js_exn])] in
  let stdlib_list =
    [
      ("headOrThrow", [not_found]);
      ("tailOrThrow", [not_found]);
      ("getOrThrow", [not_found]);
    ]
  in
  let stdlib_null = [("getOrThrow", [invalid_argument])] in
  let stdlib_nullable = [("getOrThrow", [invalid_argument])] in
  let stdlib_option = [("getOrThrow", [js_exn])] in
  let stdlib_result = [("getOrThrow", [js_exn])] in
  let yojson_basic = [("from_string", [yojson_json_error])] in
  let yojson_basic_util =
    [
      ("member", [yojson_type_error]);
      ("to_assoc", [yojson_type_error]);
      ("to_bool", [yojson_type_error]);
      ("to_bool_option", [yojson_type_error]);
      ("to_float", [yojson_type_error]);
      ("to_float_option", [yojson_type_error]);
      ("to_int", [yojson_type_error]);
      ("to_list", [yojson_type_error]);
      ("to_number", [yojson_type_error]);
      ("to_number_option", [yojson_type_error]);
      ("to_string", [yojson_type_error]);
      ("to_string_option", [yojson_type_error]);
    ]
  in
  [
    ("Belt.Array", belt_array);
    ("Belt_Array", belt_array);
    ("Belt.List", belt_list);
    ("Belt_List", belt_list);
    ("Belt.Map", belt_map);
    ("Belt.Map.Int", belt_map);
    ("Belt.Map.String", belt_map);
    ("Belt_Map", belt_map);
    ("Belt_Map.Int", belt_map);
    ("Belt_Map.String", belt_map);
    ("Belt_MapInt", belt_map);
    ("Belt_MapString", belt_map);
    ("Belt.MutableMap", belt_mutable_map);
    ("Belt.MutableMap.Int", belt_mutable_map);
    ("Belt.MutableMap.String", belt_mutable_map);
    ("Belt_MutableMap", belt_mutable_map);
    ("Belt_MutableMap.Int", belt_mutable_map);
    ("Belt_MutableMap.String", belt_mutable_map);
    ("Belt_MutableMapInt", belt_mutable_map);
    ("Belt_MutableMapString", belt_mutable_map);
    ("Belt.MutableQueue", belt_mutable_queue);
    ("Belt_MutableQueue", belt_mutable_queue);
    ("Belt_MutableSetInt", belt_mutable_set);
    ("Belt_MutableSetString", belt_mutable_set);
    ("Belt.MutableSet", belt_mutable_set);
    ("Belt.MutableSet.Int", belt_mutable_set);
    ("Belt.MutableSet.String", belt_mutable_set);
    ("Belt.Option", belt_option);
    ("Belt_Option", belt_option);
    ("Belt.Result", belt_result);
    ("Belt_Result", belt_result);
    ("Belt.Set", belt_set);
    ("Belt.Set.Int", belt_set);
    ("Belt.Set.String", belt_set);
    ("Belt_Set", belt_set);
    ("Belt_Set.Int", belt_set);
    ("Belt_Set.String", belt_set);
    ("Belt_SetInt", belt_set);
    ("Belt_SetString", belt_set);
    ("BigInt", stdlib_big_int);
    ("Bool", stdlib_bool);
    ("JsError", stdlib_js_error);
    ("JSON", stdlib_json);
    ("Json_decode", bs_json);
    ("Json.Decode", bs_json);
    ("List", stdlib_list);
    ("MutableSet", belt_mutable_set);
    ("MutableSet.Int", belt_mutable_set);
    ("MutableSet.String", belt_mutable_set);
    ("Null", stdlib_null);
    ("Nullable", stdlib_nullable);
    ("Option", stdlib_option);
    ("Pervasives", stdlib);
    ("Result", stdlib_result);
    ("Stdlib", stdlib);
    ("Stdlib_BigInt", stdlib_big_int);
    ("Stdlib.BigInt", stdlib_big_int);
    ("Stdlib_Bool", stdlib_bool);
    ("Stdlib.Bool", stdlib_bool);
    ("Stdlib_JsError", stdlib_js_error);
    ("Stdlib.JsError", stdlib_js_error);
    ("Stdlib_JSON", stdlib_json);
    ("Stdlib.JSON", stdlib_json);
    ("Stdlib_List", stdlib_list);
    ("Stdlib.List", stdlib_list);
    ("Stdlib_Null", stdlib_null);
    ("Stdlib.Null", stdlib_null);
    ("Stdlib_Nullable", stdlib_nullable);
    ("Stdlib.Nullable", stdlib_nullable);
    ("Stdlib_Option", stdlib_option);
    ("Stdlib.Option", stdlib_option);
    ("Stdlib_Result", stdlib_result);
    ("Stdlib.Result", stdlib_result);
    ("Yojson.Basic", yojson_basic);
    ("Yojson.Basic.Util", yojson_basic_util);
  ]
  |> List.iter (fun (name, group) ->
      group
      |> List.iter (fun (s, e) ->
          Hashtbl.add table
            (name ^ "." ^ s |> Name.create)
            (e |> Exceptions.from_list)));
  table

let find (path : Dce_path.t) =
  Hashtbl.find_opt raises_lib_table (path |> Dce_path.to_name)
