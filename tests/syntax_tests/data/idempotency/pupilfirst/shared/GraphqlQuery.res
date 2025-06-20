exception Graphql_error(string)

type notification = {
  kind: string,
  title: string,
  body: string,
}

let decodeNotification = json => {
  open Json.Decode
  {
    kind: json->field("kind", string),
    title: json->field("title", string),
    body: json->field("body", string),
  }
}

let decodeNotifications = json => json->Json.Decode.list(decodeNotification)

let flashNotifications = obj =>
  switch Js.Dict.get(obj, "notifications") {
  | Some(notifications) =>
    notifications
    ->decodeNotifications
    ->List.iter(n => {
      let notify = switch n.kind {
      | "success" => Notification.success
      | "error" => Notification.error
      | _ => Notification.notice
      }

      notify(n.title, n.body)
    })
  | None => ()
  }

let sendQuery = (~notify=true, q) => {
  open Bs_fetch
  fetchWithInit(
    "/graphql",
    RequestInit.make(
      ~method_=Post,
      ~body=Js.Dict.fromList(list{
        ("query", Js.Json.string(q["query"])),
        ("variables", q["variables"]),
      })
      ->Js.Json.object_
      ->Js.Json.stringify
      ->BodyInit.make,
      ~credentials=Include,
      ~headers=HeadersInit.makeWithArray([
        ("X-CSRF-Token", AuthenticityToken.fromHead()),
        ("Content-Type", "application/json"),
      ]),
      (),
    ),
  )
  ->Js.Promise.then_(resp =>
    if Response.ok(resp) {
      Response.json(resp)
    } else {
      if notify {
        let statusCode = resp->Fetch.Response.status->string_of_int

        Notification.error(
          "Error " ++ statusCode,
          "Our team has been notified of this error. Please reload the page and try again.",
        )
      }

      Js.Promise.reject(Graphql_error("Request failed: " ++ Response.statusText(resp)))
    }
  )
  ->Js.Promise.then_(json =>
    switch Js.Json.decodeObject(json) {
    | Some(obj) =>
      if notify {
        obj->flashNotifications
      }

      Js.Dict.unsafeGet(obj, "data")->q["parse"]->Js.Promise.resolve
    | None => Js.Promise.reject(Graphql_error("Response is not an object"))
    }
  )
}
