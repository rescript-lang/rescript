open Warp_Types_Method

let options = (url: string) => Warp_Client.make(~url, ~method=OPTIONS)

let get = (url: string) => Warp_Client.make(~url, ~method=GET)

let head = (url: string) => Warp_Client.make(~url, ~method=HEAD)

let post = (url: string) => Warp_Client.make(~url, ~method=POST)

let put = (url: string) => Warp_Client.make(~url, ~method=PUT)

let delete = (url: string) => Warp_Client.make(~url, ~method=DELETE)

let trace = (url: string) => Warp_Client.make(~url, ~method=TRACE)

let connect = (url: string) => Warp_Client.make(~url, ~method=CONNECT)
