type resp
@set external set_okay: (resp, @as(200) _) => unit = "statusCode"

@set external set_hi: (resp, @as("hi") _) => unit = "hi"

let f = resp => {
  set_okay(resp)
  set_hi(resp)
}
