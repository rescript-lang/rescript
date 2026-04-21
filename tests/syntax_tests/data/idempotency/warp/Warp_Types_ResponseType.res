type rec payload<'a> =
  | ArrayBufferResponse(option<ArrayBuffer.t>): payload<
      option<ArrayBuffer.t>,
    >
  | DocumentResponse(option<Dom.document>): payload<option<Dom.document>>
  | JSONResponse(option<JSON.t>): payload<option<JSON.t>>
  | TextResponse(option<string>): payload<option<string>>
