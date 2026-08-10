open Warp_Types_Client

let add = (client, key, value) => {
  ...client,
  queryString: switch client.queryString {
  | Some(queryString) => Some(queryString ++ ("&" ++ (key ++ ("=" ++ value))))
  | None => Some(key ++ ("=" ++ value))
  },
}

let set = (client, queryString) => {
  ...client,
  queryString: Belt.List.map(queryString, ((key, value)) => key ++ ("=" ++ value))
  ->Belt.List.toArray
  ->Array.joinUnsafe("&")
  ->Some,
}

let remove = (client, keyToRemove) => {
  ...client,
  queryString: switch client.queryString {
  | Some(queryString) =>
    queryString
    ->String.split("&")
    ->Belt.Array.keep(item =>
      switch item->String.split("=") {
      | [key, _value] => key !== keyToRemove
      | _ => true
      }
    )
    ->Array.joinUnsafe("&")
    ->Some
  | None => None
  },
}
