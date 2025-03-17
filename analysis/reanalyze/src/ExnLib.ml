let raisesLibTable : (Name.t, Exceptions.t) Hashtbl.t =
  let table = Hashtbl.create 15 in
  let open Exn in
  let beltArray = [("getExn", [assertFailure]); ("setExn", [assertFailure])] in
  let beltList =
    [("getExn", [notFound]); ("headExn", [notFound]); ("tailExn", [notFound])]
  in
  let beltMap = [("getExn", [notFound])] in
  let beltMutableMap = beltMap in
  let beltMutableQueue = [("peekExn", [notFound]); ("popExn", [notFound])] in
  let beltMutableSet = [("getExn", [notFound])] in
  let beltOption = [("getExn", [notFound])] in
  let beltResult = [("getExn", [notFound])] in
  let beltSet = [("getExn", [notFound])] in
  let bsJson =
    (* bs-json *)
    [
      ("bool", [decodeError]);
      ("float", [decodeError]);
      ("int", [decodeError]);
      ("string", [decodeError]);
      ("char", [decodeError]);
      ("date", [decodeError]);
      ("nullable", [decodeError]);
      ("nullAs", [decodeError]);
      ("array", [decodeError]);
      ("list", [decodeError]);
      ("pair", [decodeError]);
      ("tuple2", [decodeError]);
      ("tuple3", [decodeError]);
      ("tuple4", [decodeError]);
      ("dict", [decodeError]);
      ("field", [decodeError]);
      ("at", [decodeError; invalidArgument]);
      ("oneOf", [decodeError]);
      ("either", [decodeError]);
    ]
  in
  let stdlib =
    [
      ("panic", [jsExnError]);
      ("assertEqual", [jsExnError]);
      ("invalid_arg", [invalidArgument]);
      ("failwith", [failure]);
      ("/", [divisionByZero]);
      ("mod", [divisionByZero]);
      ("char_of_int", [invalidArgument]);
      ("bool_of_string", [invalidArgument]);
      ("int_of_string", [failure]);
      ("float_of_string", [failure]);
    ]
  in
  let stdlibBigInt = [("fromStringExn", [jsExnError])] in
  let stdlibError = [("raise", [jsExnError])] in
  let stdlibExn =
    [
      ("raiseError", [jsExnError]);
      ("raiseEvalError", [jsExnError]);
      ("raiseRangeError", [jsExnError]);
      ("raiseReferenceError", [jsExnError]);
      ("raiseSyntaxError", [jsExnError]);
      ("raiseTypeError", [jsExnError]);
      ("raiseUriError", [jsExnError]);
    ]
  in
  let stdlibJson =
    [
      ("parseExn", [jsExnError]);
      ("parseExnWithReviver", [jsExnError]);
      ("stringifyAny", [jsExnError]);
      ("stringifyAnyWithIndent", [jsExnError]);
      ("stringifyAnyWithReplacer", [jsExnError]);
      ("stringifyAnyWithReplacerAndIndent", [jsExnError]);
      ("stringifyAnyWithFilter", [jsExnError]);
      ("stringifyAnyWithFilterAndIndent", [jsExnError]);
    ]
  in
  let stdlibList =
    [("headExn", [notFound]); ("tailExn", [notFound]); ("getExn", [notFound])]
  in
  let stdlibNull = [("getExn", [invalidArgument])] in
  let stdlibNullable = [("getExn", [invalidArgument])] in
  let stdlibOption = [("getExn", [jsExnError])] in
  let stdlibResult = [("getExn", [notFound])] in
  let yojsonBasic = [("from_string", [yojsonJsonError])] in
  let yojsonBasicUtil =
    [
      ("member", [yojsonTypeError]);
      ("to_assoc", [yojsonTypeError]);
      ("to_bool", [yojsonTypeError]);
      ("to_bool_option", [yojsonTypeError]);
      ("to_float", [yojsonTypeError]);
      ("to_float_option", [yojsonTypeError]);
      ("to_int", [yojsonTypeError]);
      ("to_list", [yojsonTypeError]);
      ("to_number", [yojsonTypeError]);
      ("to_number_option", [yojsonTypeError]);
      ("to_string", [yojsonTypeError]);
      ("to_string_option", [yojsonTypeError]);
    ]
  in
  [
    ("Belt.Array", beltArray);
    ("Belt_Array", beltArray);
    ("Belt.List", beltList);
    ("Belt_List", beltList);
    ("Belt.Map", beltMap);
    ("Belt.Map.Int", beltMap);
    ("Belt.Map.String", beltMap);
    ("Belt_Map", beltMap);
    ("Belt_Map.Int", beltMap);
    ("Belt_Map.String", beltMap);
    ("Belt_MapInt", beltMap);
    ("Belt_MapString", beltMap);
    ("Belt.MutableMap", beltMutableMap);
    ("Belt.MutableMap.Int", beltMutableMap);
    ("Belt.MutableMap.String", beltMutableMap);
    ("Belt_MutableMap", beltMutableMap);
    ("Belt_MutableMap.Int", beltMutableMap);
    ("Belt_MutableMap.String", beltMutableMap);
    ("Belt_MutableMapInt", beltMutableMap);
    ("Belt_MutableMapString", beltMutableMap);
    ("Belt.MutableQueue", beltMutableQueue);
    ("Belt_MutableQueue", beltMutableQueue);
    ("Belt_MutableSetInt", beltMutableSet);
    ("Belt_MutableSetString", beltMutableSet);
    ("Belt.MutableSet", beltMutableSet);
    ("Belt.MutableSet.Int", beltMutableSet);
    ("Belt.MutableSet.String", beltMutableSet);
    ("Belt.Option", beltOption);
    ("Belt_Option", beltOption);
    ("Belt.Result", beltResult);
    ("Belt_Result", beltResult);
    ("Belt.Set", beltSet);
    ("Belt.Set.Int", beltSet);
    ("Belt.Set.String", beltSet);
    ("Belt_Set", beltSet);
    ("Belt_Set.Int", beltSet);
    ("Belt_Set.String", beltSet);
    ("Belt_SetInt", beltSet);
    ("Belt_SetString", beltSet);
    ("BigInt", stdlibBigInt);
    ("Char", [("chr", [invalidArgument])]);
    ("Error", stdlibError);
    ("Exn", stdlibExn);
    ("Js.Json", [("parseExn", [jsExnError])]);
    ("JSON", stdlibJson);
    ("Json_decode", bsJson);
    ("Json.Decode", bsJson);
    ("List", stdlibList);
    ("MutableSet", beltMutableSet);
    ("MutableSet.Int", beltMutableSet);
    ("MutableSet.String", beltMutableSet);
    ("Null", stdlibNull);
    ("Nullable", stdlibNullable);
    ("Option", stdlibOption);
    ("Pervasives", stdlib);
    ("Result", stdlibResult);
    ("Stdlib", stdlib);
    ("Stdlib_BigInt", stdlibBigInt);
    ("Stdlib.BigInt", stdlibBigInt);
    ("Stdlib_Error", stdlibError);
    ("Stdlib.Error", stdlibError);
    ("Stdlib_Exn", stdlibExn);
    ("Stdlib.Exn", stdlibExn);
    ("Stdlib_JSON", stdlibJson);
    ("Stdlib.JSON", stdlibJson);
    ("Stdlib_List", stdlibList);
    ("Stdlib.List", stdlibList);
    ("Stdlib_Null", stdlibNull);
    ("Stdlib.Null", stdlibNull);
    ("Stdlib_Nullable", stdlibNullable);
    ("Stdlib.Nullable", stdlibNullable);
    ("Stdlib_Option", stdlibOption);
    ("Stdlib.Option", stdlibOption);
    ("Stdlib_Result", stdlibResult);
    ("Stdlib.Result", stdlibResult);
    ("Yojson.Basic", yojsonBasic);
    ("Yojson.Basic.Util", yojsonBasicUtil);
  ]
  |> List.iter (fun (name, group) ->
         group
         |> List.iter (fun (s, e) ->
                Hashtbl.add table
                  (name ^ "." ^ s |> Name.create)
                  (e |> Exceptions.fromList)));
  table

let find (path : Common.Path.t) =
  Hashtbl.find_opt raisesLibTable (path |> Common.Path.toName)
