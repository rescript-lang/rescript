@val
external unsafeAddStyle: (%raw("{}"), t, {..}) => t = "Object.assign"

@val
external legacyObject: (@as(json`{foo: true}`) _, int) => int = "Object.assign"
