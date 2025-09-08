let make: (
  ~url: string,
  ~method: Warp_Types_Method.t,
) => Warp_Types_Client.t<Warp_Types_ResponseType.payload<option<string>>>
