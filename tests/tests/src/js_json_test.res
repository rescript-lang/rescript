open Mocha
open Test_utils

module J = Js.Json
module Array = Ocaml_Array

describe(__MODULE__, () => {
  test("JSON object parsing and validation", () => {
    let v = J.parseExn(` { "x" : [1, 2, 3 ] } `)

    let ty = J.classify(v)
    switch ty {
    | J.JSONObject(x) =>
      /* compiler infer x : J.t dict */
      switch Js.Dict.get(x, "x") {
      | Some(v) =>
        let ty2 = J.classify(v)
        switch ty2 {
        | J.JSONArray(x) =>
          /* compiler infer x : J.t array */
          Js.Array2.forEach(
            x,
            x => {
              let ty3 = J.classify(x)
              switch ty3 {
              | J.JSONNumber(_) => ()
              | _ => assert(false)
              }
            },
          )
          ok(__LOC__, true)
        | _ => ok(__LOC__, false)
        }
      | None => ok(__LOC__, false)
      }
    | _ => ok(__LOC__, false)
    }

    eq(__LOC__, J.test(v, Object), true)
  })

  test("JSON null parsing", () => {
    let json = J.parseExn(J.stringify(J.null))
    let ty = J.classify(json)
    switch ty {
    | J.JSONNull => ok(__LOC__, true)
    | _ =>
      Js.log(ty)
      ok(__LOC__, false)
    }
  })

  test("JSON string parsing", () => {
    let json = J.parseExn(J.stringify(J.string("test string")))
    let ty = J.classify(json)
    switch ty {
    | J.JSONString(x) => eq(__LOC__, x, "test string")
    | _ => ok(__LOC__, false)
    }
  })

  test("JSON number parsing", () => {
    let json = J.parseExn(J.stringify(J.number(1.23456789)))
    let ty = J.classify(json)
    switch ty {
    | J.JSONNumber(x) => eq(__LOC__, x, 1.23456789)
    | _ => ok(__LOC__, false)
    }
  })

  test("JSON large integer parsing", () => {
    let json = J.parseExn(J.stringify(J.number(float_of_int(0xAFAFAFAF))))
    let ty = J.classify(json)
    switch ty {
    | J.JSONNumber(x) => eq(__LOC__, int_of_float(x), 0xAFAFAFAF)
    | _ => ok(__LOC__, false)
    }
  })

  test("JSON boolean parsing", () => {
    let test = v => {
      let json = J.parseExn(J.stringify(J.boolean(v)))
      let ty = J.classify(json)
      switch ty {
      | J.JSONTrue => eq(__LOC__, true, v)
      | J.JSONFalse => eq(__LOC__, false, v)
      | _ => ok(__LOC__, false)
      }
    }

    test(true)
    test(false)
  })

  test("JSON object with string and number fields", () => {
    let option_get = x =>
      switch x {
      | None => assert(false)
      | Some(x) => x
      }

    let dict = Js_dict.empty()
    Js_dict.set(dict, "a", J.string("test string"))
    Js_dict.set(dict, "b", J.number(123.0))

    let json = J.parseExn(J.stringify(J.object_(dict)))

    /* Make sure parsed as Object */
    let ty = J.classify(json)
    switch ty {
    | J.JSONObject(x) =>
      /* Test field 'a' */
      let ta = J.classify(option_get(Js_dict.get(x, "a")))
      switch ta {
      | J.JSONString(a) =>
        if a != "test string" {
          ok(__LOC__, false)
        } else {
          /* Test field 'b' */
          let ty = J.classify(option_get(Js_dict.get(x, "b")))
          switch ty {
          | J.JSONNumber(b) => approxEq(__LOC__, 0.001, 123.0, b)
          | _ => ok(__LOC__, false)
          }
        }
      | _ => ok(__LOC__, false)
      }
    | _ => ok(__LOC__, false)
    }
  })

  /* Check that the given json value is an array and that its element
   * a position [i] is equal to both the [kind] and [expected] value */
  let eq_at_i = (type a, loc: string, json: J.t, i: int, kind: J.Kind.t<a>, expected: a): unit => {
    let ty = J.classify(json)
    switch ty {
    | J.JSONArray(x) =>
      let ty = J.classify(x[i])
      switch kind {
      | J.Kind.Boolean =>
        switch ty {
        | JSONTrue => eq(loc, true, expected)
        | JSONFalse => eq(loc, false, expected)
        | _ => ok(loc, false)
        }
      | J.Kind.Number =>
        switch ty {
        | JSONNumber(f) => eq(loc, f, expected)
        | _ => ok(loc, false)
        }
      | J.Kind.Object =>
        switch ty {
        | JSONObject(f) => eq(loc, f, expected)
        | _ => ok(loc, false)
        }
      | J.Kind.Array =>
        switch ty {
        | JSONArray(f) => eq(loc, f, expected)
        | _ => ok(loc, false)
        }
      | J.Kind.Null =>
        switch ty {
        | JSONNull => ok(loc, true)
        | _ => ok(loc, false)
        }
      | J.Kind.String =>
        switch ty {
        | JSONString(f) => eq(loc, f, expected)
        | _ => ok(loc, false)
        }
      }
    | _ => ok(loc, false)
    }
  }

  test("JSON string array parsing", () => {
    let json = J.parseExn(
      J.stringify(J.array(Belt.Array.map(["string 0", "string 1", "string 2"], J.string))),
    )

    eq_at_i(__LOC__, json, 0, J.Kind.String, "string 0")
    eq_at_i(__LOC__, json, 1, J.Kind.String, "string 1")
    eq_at_i(__LOC__, json, 2, J.Kind.String, "string 2")
  })

  test("JSON stringArray parsing", () => {
    let json = J.parseExn(J.stringify(J.stringArray(["string 0", "string 1", "string 2"])))

    eq_at_i(__LOC__, json, 0, J.Kind.String, "string 0")
    eq_at_i(__LOC__, json, 1, J.Kind.String, "string 1")
    eq_at_i(__LOC__, json, 2, J.Kind.String, "string 2")
  })

  test("JSON number array parsing", () => {
    let a = [1.0000001, 10000000000.1, 123.0]
    let json = J.parseExn(J.stringify(J.numberArray(a)))

    /* Loop is unrolled to keep relevant location information */
    eq_at_i(__LOC__, json, 0, J.Kind.Number, a[0])
    eq_at_i(__LOC__, json, 1, J.Kind.Number, a[1])
    eq_at_i(__LOC__, json, 2, J.Kind.Number, a[2])
  })

  test("JSON integer array parsing", () => {
    let a = [0, 0xAFAFAFAF, 0xF000AABB]
    let json = J.parseExn(J.stringify(J.numberArray(a->Belt.Array.map(float_of_int))))

    /* Loop is unrolled to keep relevant location information */
    eq_at_i(__LOC__, json, 0, J.Kind.Number, float_of_int(a[0]))
    eq_at_i(__LOC__, json, 1, J.Kind.Number, float_of_int(a[1]))
    eq_at_i(__LOC__, json, 2, J.Kind.Number, float_of_int(a[2]))
  })

  test("JSON boolean array parsing", () => {
    let a = [true, false, true]
    let json = J.parseExn(J.stringify(J.booleanArray(a)))

    /* Loop is unrolled to keep relevant location information */
    eq_at_i(__LOC__, json, 0, J.Kind.Boolean, a[0])
    eq_at_i(__LOC__, json, 1, J.Kind.Boolean, a[1])
    eq_at_i(__LOC__, json, 2, J.Kind.Boolean, a[2])
  })

  test("JSON object array parsing", () => {
    let option_get = x =>
      switch x {
      | None => assert(false)
      | Some(x) => x
      }

    let make_d = (s, i) => {
      let d = Js_dict.empty()
      Js_dict.set(d, "a", J.string(s))
      Js_dict.set(d, "b", J.number(float_of_int(i)))
      d
    }

    let a = [make_d("aaa", 123), make_d("bbb", 456)]
    let json = J.parseExn(J.stringify(J.objectArray(a)))

    let ty = J.classify(json)
    switch ty {
    | J.JSONArray(x) =>
      let ty = J.classify(x[1])
      switch ty {
      | J.JSONObject(a1) =>
        let ty = J.classify(option_get(Js_dict.get(a1, "a")))
        switch ty {
        | J.JSONString(aValue) => eq(__LOC__, aValue, "bbb")
        | _ => ok(__LOC__, false)
        }
      | _ => ok(__LOC__, false)
      }
    | _ => ok(__LOC__, false)
    }
  })

  test("JSON invalid parsing", () => {
    let invalid_json_str = "{{ A}"
    try {
      let _ = J.parseExn(invalid_json_str)
      ok(__LOC__, false)
    } catch {
    | exn => ok(__LOC__, true)
    }
  })

  /* stringifyAny tests */
  test("JSON stringifyAny array", () => eq(__LOC__, J.stringifyAny([1, 2, 3]), Some("[1,2,3]")))

  test("JSON stringifyAny object", () =>
    eq(
      __LOC__,
      J.stringifyAny({"foo": 1, "bar": "hello", "baz": {"baaz": 10}}),
      Some(`{"foo":1,"bar":"hello","baz":{"baaz":10}}`),
    )
  )

  test("JSON stringifyAny null", () => eq(__LOC__, J.stringifyAny(Js.Null.empty), Some("null")))

  test("JSON stringifyAny undefined", () => eq(__LOC__, J.stringifyAny(Js.Undefined.empty), None))

  test("JSON decodeString", () => {
    eq(__LOC__, J.decodeString(J.string("test")), Some("test"))
    eq(__LOC__, J.decodeString(J.boolean(true)), None)
    eq(__LOC__, J.decodeString(J.array([])), None)
    eq(__LOC__, J.decodeString(J.null), None)
    eq(__LOC__, J.decodeString(J.object_(Js.Dict.empty())), None)
    eq(__LOC__, J.decodeString(J.number(1.23)), None)
  })

  test("JSON decodeNumber", () => {
    eq(__LOC__, J.decodeNumber(J.string("test")), None)
    eq(__LOC__, J.decodeNumber(J.boolean(true)), None)
    eq(__LOC__, J.decodeNumber(J.array([])), None)
    eq(__LOC__, J.decodeNumber(J.null), None)
    eq(__LOC__, J.decodeNumber(J.object_(Js.Dict.empty())), None)
    eq(__LOC__, J.decodeNumber(J.number(1.23)), Some(1.23))
  })

  test("JSON decodeObject", () => {
    eq(__LOC__, J.decodeObject(J.string("test")), None)
    eq(__LOC__, J.decodeObject(J.boolean(true)), None)
    eq(__LOC__, J.decodeObject(J.array([])), None)
    eq(__LOC__, J.decodeObject(J.null), None)
    eq(__LOC__, J.decodeObject(J.object_(Js.Dict.empty())), Some(Js.Dict.empty()))
    eq(__LOC__, J.decodeObject(J.number(1.23)), None)
  })

  test("JSON decodeArray", () => {
    eq(__LOC__, J.decodeArray(J.string("test")), None)
    eq(__LOC__, J.decodeArray(J.boolean(true)), None)
    eq(__LOC__, J.decodeArray(J.array([])), Some([]))
    eq(__LOC__, J.decodeArray(J.null), None)
    eq(__LOC__, J.decodeArray(J.object_(Js.Dict.empty())), None)
    eq(__LOC__, J.decodeArray(J.number(1.23)), None)
  })

  test("JSON decodeBoolean", () => {
    eq(__LOC__, J.decodeBoolean(J.string("test")), None)
    eq(__LOC__, J.decodeBoolean(J.boolean(true)), Some(true))
    eq(__LOC__, J.decodeBoolean(J.array([])), None)
    eq(__LOC__, J.decodeBoolean(J.null), None)
    eq(__LOC__, J.decodeBoolean(J.object_(Js.Dict.empty())), None)
    eq(__LOC__, J.decodeBoolean(J.number(1.23)), None)
  })

  test("JSON decodeNull", () => {
    eq(__LOC__, J.decodeNull(J.string("test")), None)
    eq(__LOC__, J.decodeNull(J.boolean(true)), None)
    eq(__LOC__, J.decodeNull(J.array([])), None)
    eq(__LOC__, J.decodeNull(J.null), Some(Js.null))
    eq(__LOC__, J.decodeNull(J.object_(Js.Dict.empty())), None)
    eq(__LOC__, J.decodeNull(J.number(1.23)), None)
  })

  test("JSON serialize/deserialize identity", () => {
    let id = (type t, obj: t): t => obj->J.serializeExn->J.deserializeUnsafe

    let idtest = obj => eq(__LOC__, obj, id(obj))
    idtest(None)
    idtest(list{(None, None, None)})
    idtest(
      Belt.List.makeBy(
        500,
        i =>
          if mod(i, 2) == 0 {
            None
          } else {
            Some(1)
          },
      ),
    )
    idtest(
      Belt.Array.makeBy(
        500,
        i =>
          if mod(i, 2) == 0 {
            None
          } else {
            Some(1)
          },
      ),
    )
  })
})
