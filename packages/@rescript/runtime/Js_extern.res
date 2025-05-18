external testAny: 'a => bool = "%is_nullable"

external null: Primitive_js_extern.null<'a> = "%null"

external undefined: Primitive_js_extern.null<'a> = "%undefined"

external typeof: 'a => string = "%typeof"
