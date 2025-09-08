open Warp_Types_Client

let add = (client, key, value) => {
  ...client,
  headers: list{(key, value), ...client.headers},
}

let set = (client, headers) => {
  ...client,
  headers: headers,
}

let remove = (client, headerToRemove) => {
  ...client,
  headers: Belt.List.keep(client.headers, header => {
    let (key, _value) = header
    key !== headerToRemove
  }),
}
