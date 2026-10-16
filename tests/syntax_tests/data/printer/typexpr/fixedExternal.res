@val
external unsafeAddStyle: (@as(json`{}`) _, t, {..}) => t = "Object.assign"

@module("react")
external useEffect0: (unit => option<unit => unit>, @as(json`[]`) _) => unit = "useEffect"

@send external createElement: (document, @as("img") _) => element = "createElement"

@val external templateString: (@as(`ab`) _, int) => int = "f"

@val external numbers: (@as(1) _, @as(-1) _, @as(0x10) _, int) => int = "f"

@obj external makeOptions: (~objectMode: @as(false) _, ~name: string, ~mode: @as("fast") _, unit) => options = ""

@val external single: @as(1) _ => int = "f"

@val external attributes: (@foo @as(1) _, @as(1) @foo _, @as({1}) _, int) => int = "f"

@val
external commented: (/* a */ @as(1) /* b */ _ /* c */, @as(/* d */ "s") _, int) => int = "f"

type notExternal = (@as(1) _, int) => int

@val external payloadAttributes: (@as(@foo 1) _, @as(@foo json`{}`) _, int) => int = "f"

@val
external raw: (%raw("{}"), %raw(`"img"`), ~mode: %raw("[1, 2]"), int) => int = "f"

@val external singleRaw: %raw("null") => int = "f"

@val
external rawCommented: (/* a */ %raw("1") /* b */, %raw(/* c */ "2"), int) => int = "f"

@val external legacyNumbers: (@as(007) _, @as(1_000) _, int) => int = "f"
