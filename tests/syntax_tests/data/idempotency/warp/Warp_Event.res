open Warp_Types_Client

let onLoad:
  type a. (
    Warp_Types_Client.t<Warp_Types_ResponseType.payload<a>>,
    Belt.Result.t<a, string> => unit,
  ) => Warp_Types_Client.t<Warp_Types_ResponseType.payload<a>> =
  (client, callback) => {
    let eval:
      type a. Warp_Types_ResponseType.payload<a> => a =
      x =>
        switch x {
        | Warp_Types_ResponseType.ArrayBufferResponse(b) => b
        | Warp_Types_ResponseType.DocumentResponse(d) => d
        | Warp_Types_ResponseType.JSONResponse(j) => j
        | Warp_Types_ResponseType.TextResponse(t) => t
        }

    {
      ...client,
      onLoad: Some(
        data =>
          switch data {
          | Belt.Result.Ok(data) => callback(Ok(eval(data)))
          | Belt.Result.Error(message) => callback(Error(message))
          },
      ),
    }
  }

let onLoadWithStatusCode:
  type a. (
    Warp_Types_Client.t<Warp_Types_ResponseType.payload<a>>,
    (Belt.Result.t<a, string>, int) => unit,
  ) => Warp_Types_Client.t<Warp_Types_ResponseType.payload<a>> =
  (client, callback) => {
    let eval:
      type a. Warp_Types_ResponseType.payload<a> => a =
      x =>
        switch x {
        | Warp_Types_ResponseType.ArrayBufferResponse(b) => b
        | Warp_Types_ResponseType.DocumentResponse(d) => d
        | Warp_Types_ResponseType.JSONResponse(j) => j
        | Warp_Types_ResponseType.TextResponse(t) => t
        }

    {
      ...client,
      onLoadWithStatusCode: Some(
        (data, statusCode) =>
          switch data {
          | Belt.Result.Ok(data) => callback(Ok(eval(data)), statusCode)
          | Belt.Result.Error(message) => callback(Error(message), statusCode)
          },
      ),
    }
  }

let onProgress = (client, callback) => {
  ...client,
  onProgess: Some(callback),
}

let onAbort = (client, callback) => {
  ...client,
  onAbort: Some(callback),
}
