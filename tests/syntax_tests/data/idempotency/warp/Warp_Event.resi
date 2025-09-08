let onLoad: (
  Warp_Types_Client.t<Warp_Types_ResponseType.payload<'a>>,
  Belt.Result.t<'a, string> => unit,
) => Warp_Types_Client.t<Warp_Types_ResponseType.payload<'a>>

let onLoadWithStatusCode: (
  Warp_Types_Client.t<Warp_Types_ResponseType.payload<'a>>,
  (Belt.Result.t<'a, string>, int) => unit,
) => Warp_Types_Client.t<Warp_Types_ResponseType.payload<'a>>

let onProgress: (Warp_Types_Client.t<'a>, Dom.progressEvent => unit) => Warp_Types_Client.t<'a>

let onAbort: (Warp_Types_Client.t<'a>, unit => unit) => Warp_Types_Client.t<'a>
