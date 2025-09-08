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
  ->Js.Array2.joinWith("&")
  ->Some,
}

let remove = (client, keyToRemove) => {
  ...client,
  queryString: switch client.queryString {
  | Some(queryString) =>
    queryString
    ->Js.String2.split("&")
    ->Belt.Array.keep(item =>
      switch item->Js.String2.split("=") {
      | [key, _value] => key !== keyToRemove
      | _ => true
      }
    )
    ->Js.Array2.joinWith("&")
    ->Some
  | None => None
  },
}
