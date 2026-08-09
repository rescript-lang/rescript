open Warp_Types_Client

let add = (client, key, value) => {
  ...client,
  formData: switch client.formData {
  | Some(formData) => Some(formData ++ ("&" ++ (key ++ ("=" ++ value))))
  | None => Some(key ++ ("=" ++ value))
  },
  requestType: "application/x-www-form-urlencoded",
}

let set = (client, formData) => {
  ...client,
  formData: Belt.List.map(formData, ((key, value)) => key ++ ("=" ++ value))
  ->Belt.List.toArray
  ->Array.joinUnsafe("&")
  ->Some,
  requestType: "application/x-www-form-urlencoded",
}

let remove = (client, keyToRemove) => {
  ...client,
  formData: switch client.formData {
  | Some(formData) =>
    formData
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
  requestType: "application/x-www-form-urlencoded",
}

let setJson = (client, formData) => {
  ...client,
  formData: Some(formData),
  requestType: "application/json",
}
