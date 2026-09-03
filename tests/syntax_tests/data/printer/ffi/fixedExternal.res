@val
external unsafeAddStyle: (%raw("{}"), t, {..}) => t = "Object.assign"

@module("react")
external useEffect0: (unit => option<unit => unit>, %raw("[]")) => unit = "useEffect"

@val
external stringValue: (%raw(`"img"`), int) => int = "f"

@val
external legacyObject: (@as(json`{foo: true}`) _, int) => int = "Object.assign"

@val
external legacyString: (@as("img") _, int) => int = "f"
