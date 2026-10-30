type resp
@set external set_okay: (resp, %raw("200")) => unit = "statusCode"

@set external set_hi: (resp, %raw(`"hi"`)) => unit = "hi"

let f = resp => {
  set_okay(resp)
  set_hi(resp)
}
