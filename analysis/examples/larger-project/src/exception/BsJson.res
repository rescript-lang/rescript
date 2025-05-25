@throw(DecodeError)
let testBsJson = x => Json_decode.string(x)

@throw(DecodeError)
let testBsJson2 = x => Json.Decode.string(x)
