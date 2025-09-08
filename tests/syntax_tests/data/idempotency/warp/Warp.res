module Client = Warp_Client
module Header = Warp_Header
module Method = Warp_Method
module QueryString = Warp_QueryString
module FormData = Warp_FormData
module ResponseType = Warp_ResponseType
module Types = Warp_Types
module Event = Warp_Event
module Settings = Warp_Settings

let send:
  type a. Types.Client.t<Types.ResponseType.payload<a>> => option<unit => unit> =
  client => {
    let xhr = Warp_XHR.make()

    let url = switch client.queryString {
    | Some(queryString) => client.url ++ ("?" ++ queryString)
    | None => client.url
    }

    let method_ = switch client.method {
    | OPTIONS => "OPTIONS"
    | GET => "GET"
    | HEAD => "HEAD"
    | POST => "POST"
    | PUT => "PUT"
    | DELETE => "DELETE"
    | TRACE => "TRACE"
    | CONNECT => "CONNECT"
    }

    switch (client.username, client.password) {
    | (Some(user), Some(password)) =>
      xhr->Warp_XHR.open_(~url, ~method=method_, ~async=client.async, ~user, ~password, ())
    | _ => xhr->Warp_XHR.open_(~url, ~method=method_, ~async=client.async, ())
    }

    let _ = xhr->Warp_XHR.setWithCredentials(client.withCredentials)
    let _ = xhr->Warp_XHR.setTimeout(client.timeout)
    switch client.overrideMimeType {
    | Some(mimeType) => xhr->Warp_XHR.overrideMimeType(mimeType)
    | None => ()
    }

    Belt.List.forEach(client.headers, ((key, value)) => xhr->Warp_XHR.setRequestHeader(key, value))

    switch client.responseType {
    | Types.ResponseType.TextResponse(_) => xhr->Warp_XHR.setResponseType(#text)->ignore
    | Types.ResponseType.ArrayBufferResponse(_) =>
      xhr->Warp_XHR.setResponseType(#arrayBuffer)->ignore
    | Types.ResponseType.JSONResponse(_) => xhr->Warp_XHR.setResponseType(#json)->ignore
    | Types.ResponseType.DocumentResponse(_) => xhr->Warp_XHR.setResponseType(#document)->ignore
    }

    switch client.onProgess {
    | Some(onProgress) => xhr->Warp_XHR.onProgress(evt => onProgress(evt))
    | None => ()
    }

    switch client.onAbort {
    | Some(onAbort) => xhr->Warp_XHR.onAbort(_ => onAbort())
    | None => ()
    }

    switch client.onLoad {
    | Some(onLoad) =>
      xhr->Warp_XHR.onError(_ => onLoad(Belt.Result.Error(xhr->Warp_XHR.statusText)))
      xhr->Warp_XHR.onTimeout(_ => onLoad(Belt.Result.Error(xhr->Warp_XHR.statusText)))

      xhr->Warp_XHR.onLoad(_ =>
        switch client.responseType {
        | Types.ResponseType.TextResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.TextResponse(xhr->Warp_XHR.responseText->Js.Nullable.toOption),
            ),
          )
        | Types.ResponseType.JSONResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.JSONResponse(xhr->Warp_XHR.responseJson->Js.Nullable.toOption),
            ),
          )
        | Types.ResponseType.DocumentResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.DocumentResponse(
                xhr->Warp_XHR.responseDocument->Js.Nullable.toOption,
              ),
            ),
          )
        | Types.ResponseType.ArrayBufferResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.ArrayBufferResponse(
                xhr->Warp_XHR.responseArrayBuffer->Js.Nullable.toOption,
              ),
            ),
          )
        }
      )
    | None => ()
    }

    switch client.onLoadWithStatusCode {
    | Some(onLoad) =>
      xhr->Warp_XHR.onError(_ =>
        onLoad(Belt.Result.Error(xhr->Warp_XHR.statusText), xhr->Warp_XHR.status)
      )
      xhr->Warp_XHR.onTimeout(_ =>
        onLoad(Belt.Result.Error(xhr->Warp_XHR.statusText), xhr->Warp_XHR.status)
      )

      xhr->Warp_XHR.onLoad(_ =>
        switch client.responseType {
        | Types.ResponseType.TextResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.TextResponse(xhr->Warp_XHR.responseText->Js.Nullable.toOption),
            ),
            xhr->Warp_XHR.status,
          )
        | Types.ResponseType.JSONResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.JSONResponse(xhr->Warp_XHR.responseJson->Js.Nullable.toOption),
            ),
            xhr->Warp_XHR.status,
          )
        | Types.ResponseType.DocumentResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.DocumentResponse(
                xhr->Warp_XHR.responseDocument->Js.Nullable.toOption,
              ),
            ),
            xhr->Warp_XHR.status,
          )
        | Types.ResponseType.ArrayBufferResponse(_) =>
          onLoad(
            Belt.Result.Ok(
              Types.ResponseType.ArrayBufferResponse(
                xhr->Warp_XHR.responseArrayBuffer->Js.Nullable.toOption,
              ),
            ),
            xhr->Warp_XHR.status,
          )
        }
      )
    | None => ()
    }

    switch (client.method, client.formData) {
    | (GET, _)
    | (HEAD, _)
    | (_, None) =>
      xhr->Warp_XHR.send
    | (_, Some(formData)) =>
      xhr->Warp_XHR.setRequestHeader("content-type", client.requestType)
      xhr->Warp_XHR.sendString(formData)
    }

    Some(() => Warp_XHR.abort(xhr))
  }
