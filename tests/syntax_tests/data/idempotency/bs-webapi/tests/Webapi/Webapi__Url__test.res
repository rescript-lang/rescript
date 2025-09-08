open Webapi.Url

let params = URLSearchParams.make("key1=value1&key2=value2")
URLSearchParams.forEach(Js.log2, params)
