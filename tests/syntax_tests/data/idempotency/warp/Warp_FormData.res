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
  ->Js.Array2.joinWith("&")
  ->Some,
  requestType: "application/x-www-form-urlencoded",
}

let remove = (client, keyToRemove) => {
  ...client,
  formData: switch client.formData {
  | Some(formData) =>
    formData
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
  requestType: "application/x-www-form-urlencoded",
}

let setJson = (client, formData) => {
  ...client,
  formData: Some(formData),
  requestType: "application/json",
}
