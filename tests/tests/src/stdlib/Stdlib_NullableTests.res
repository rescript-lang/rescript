let shouldHandleNullableValues = () => {
  let tNull: Nullable.t<string> = %raw("null")
  let tUndefined: Nullable.t<string> = %raw("undefined")
  let tValue: Nullable.t<string> = %raw(`"hello"`)

  Test.run(
    __POS_OF__("Should handle null"),
    switch tNull {
    | Null => true
    | Value(_) | Undefined => false
    },
    \"==",
    true,
  )

  Test.run(
    __POS_OF__("Should handle undefined"),
    switch tUndefined {
    | Undefined => true
    | Value(_) | Null => false
    },
    \"==",
    true,
  )

  Test.run(
    __POS_OF__("Should handle value"),
    switch tValue {
    | Value("hello") => true
    | _ => false
    },
    \"==",
    true,
  )
}

shouldHandleNullableValues()
