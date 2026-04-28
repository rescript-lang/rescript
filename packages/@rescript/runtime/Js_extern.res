@deprecated({
  reason: "Use `Nullable.isNullable` instead.",
  migrate: Nullable.isNullable(),
})
external testAny: 'a => bool = "%is_nullable"

@deprecated({
  reason: "Use `Nullable.null` instead.",
  migrate: Nullable.null,
})
external null: Primitive_js_extern.null<'a> = "%null"

@deprecated({
  reason: "Use `Nullable.undefined` instead.",
  migrate: Nullable.undefined,
})
external undefined: Primitive_js_extern.null<'a> = "%undefined"

@deprecated({
  reason: "Use `Type.typeof` instead.",
  migrate: Type.typeof(),
})
external typeof: 'a => string = "%typeof"
