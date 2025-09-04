@module("react")
external useMemo: (unit => 'value, \"$runtime.array"<'deps>) => 'value = "useMemo"

let x = useMemo(() => 1, (22, 33))

external justAnObject: \"$runtime.object"<'v> => unit = "justAnObject"

let x = justAnObject({ "hello": "world" })
let x2 = justAnObject(1)

let x = (v: \"$runtime.string"<'v>) => v