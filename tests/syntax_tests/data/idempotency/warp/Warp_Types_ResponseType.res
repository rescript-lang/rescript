type rec payload<'a> =
  | ArrayBufferResponse(option<Js.Typed_array.ArrayBuffer.t>): payload<
      option<Js.Typed_array.ArrayBuffer.t>,
    >
  | DocumentResponse(option<Dom.document>): payload<option<Dom.document>>
  | JSONResponse(option<Js.Json.t>): payload<option<Js.Json.t>>
  | TextResponse(option<string>): payload<option<string>>
