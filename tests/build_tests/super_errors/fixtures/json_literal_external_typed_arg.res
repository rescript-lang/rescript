type t
@send external f: (t, @as(json`{"a": 1}`) int) => unit = "f"
