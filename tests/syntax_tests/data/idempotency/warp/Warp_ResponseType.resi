let setText: Warp_Types_Client.t<'a> => Warp_Types_Client.t<
  Warp_Types_ResponseType.payload<option<string>>,
>

let setDocument: Warp_Types_Client.t<'a> => Warp_Types_Client.t<
  Warp_Types_ResponseType.payload<option<Dom.document>>,
>

let setJson: Warp_Types_Client.t<'a> => Warp_Types_Client.t<
  Warp_Types_ResponseType.payload<option<Js.Json.t>>,
>

let setArrayBuffer: Warp_Types_Client.t<'a> => Warp_Types_Client.t<
  Warp_Types_ResponseType.payload<option<Js.Typed_array.ArrayBuffer.t>>,
>
