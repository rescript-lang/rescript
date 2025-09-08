open Warp_Types_Client

let make = (~url, ~method as method_) => {
  url: url,
  method: method_,
  timeout: 0,
  async: true,
  withCredentials: false,
  username: None,
  password: None,
  overrideMimeType: None,
  queryString: None,
  responseType: Warp_Types_ResponseType.TextResponse(None),
  requestType: "application/x-www-form-urlencoded",
  formData: None,
  headers: list{},
  onLoad: None,
  onLoadWithStatusCode: None,
  onProgess: None,
  onAbort: None,
}
