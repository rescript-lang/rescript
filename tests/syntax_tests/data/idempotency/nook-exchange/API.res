let makeAuthenticatedPostRequest = (~url, ~bodyJson, ~sessionId) =>
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(Json.Encode.object_(bodyJson))),
      ~headers=Fetch.HeadersInit.make({
        "X-Client-Version": Constants.gitCommitRef,
        "Content-Type": "application/json",
        "Authorization": "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
      }),
      ~credentials=Include,
      ~mode=CORS,
      (),
    ),
  )

let setItemStatus = (~userId, ~sessionId, ~itemId, ~variant, ~status) =>
  %Repromise.Js({
    let responseResult = makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++
      ("/@me/items/" ++
      (string_of_int(itemId) ++ ("/" ++ (string_of_int(variant) ++ "/status")))),
      ~bodyJson=list{
        ("status", Json.Encode.int(User.itemStatusToJs(status))),
        ("userId", Json.Encode.string(userId)),
      },
      ~sessionId,
    )
    Promise.resolved(responseResult)
  })

let setItemStatusBatch = (~sessionId, ~items: array<(int, int)>, ~status) => {
  let url = Constants.apiUrl ++ "/@me/items/batch/status"
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify({
            open Json.Encode
            object_(list{
              ("items", array(tuple2(int, int), items)),
              ("status", int(User.itemStatusToJs(status))),
            })
          }),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let setItemNote = (~userId, ~sessionId, ~itemId, ~variant, ~note) =>
  %Repromise.Js({
    let responseResult = makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++
      ("/@me/items/" ++
      (string_of_int(itemId) ++ ("/" ++ (string_of_int(variant) ++ "/note")))),
      ~bodyJson=list{("note", Json.Encode.string(note)), ("userId", Json.Encode.string(userId))},
      ~sessionId,
    )
    Promise.resolved(responseResult)
  })

let setItemPriority = (~sessionId, ~itemId, ~variant, ~isPriority) =>
  %Repromise.Js({
    let responseResult = makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++
      ("/@me/items/" ++
      (string_of_int(itemId) ++ ("/" ++ (string_of_int(variant) ++ "/priority")))),
      ~bodyJson=list{("isPriority", Json.Encode.bool(isPriority))},
      ~sessionId,
    )
    Promise.resolved(responseResult)
  })

let importItems = (~sessionId, ~updates: array<((int, int), User.itemStatus)>) =>
  %Repromise.Js({
    let responseResult = makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++ "/@me/items/import",
      ~bodyJson=list{
        (
          "updates",
          updates->{
            open Json.Encode
            array((((itemId, variant), status)) =>
              tuple3(int, int, int, (itemId, variant, User.itemStatusToJs(status)))
            )
          },
        ),
      },
      ~sessionId,
    )
    Promise.resolved(responseResult)
  })

let removeItem = (~userId, ~sessionId, ~itemId, ~variant) => {
  let url =
    Constants.apiUrl ++
    ("/@me/items/" ++
    (string_of_int(itemId) ++ ("/" ++ string_of_int(variant))))
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(Json.Encode.object_(list{("userId", Json.Encode.string(userId))})),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let removeItems = (~sessionId, ~items: array<(int, int)>) => {
  let url = Constants.apiUrl ++ "/@me/items/batch"
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify({
            open Json.Encode
            object_(list{("items", array(tuple2(int, int), items))})
          }),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let updateProfileText = (~userId, ~sessionId, ~profileText) =>
  %Repromise.Js({
    let responseResult = makeAuthenticatedPostRequest(
      ~url=Constants.apiUrl ++ "/@me/profileText",
      ~bodyJson=list{
        ("text", Json.Encode.string(profileText)),
        ("userId", Json.Encode.string(userId)),
      },
      ~sessionId,
    )
    Promise.resolved(responseResult)
  })

let updateSetting = (~userId, ~sessionId, ~settingKey, ~settingValue) => {
  let url = Constants.apiUrl ++ "/@me/settings"
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(
            Json.Encode.object_(list{
              ("key", Json.Encode.string(settingKey)),
              ("value", settingValue),
              ("userId", Json.Encode.string(userId)),
            }),
          ),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let patchMe = (~userId, ~sessionId, ~username, ~newPassword, ~email, ~oldPassword) => {
  open Belt
  let url = Constants.apiUrl ++ "/@me"
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(
            Js.Json.object_(
              Js.Dict.fromArray(
                Array.keepMap(
                  [
                    Option.map(username, username => ("username", Js.Json.string(username))),
                    Option.map(newPassword, newPassword => (
                      "password",
                      Js.Json.string(newPassword),
                    )),
                    Option.map(email, email => ("email", Js.Json.string(email))),
                    Option.map(oldPassword, oldPassword => (
                      "oldPassword",
                      Js.Json.string(oldPassword),
                    )),
                  ],
                  x => x,
                ),
              ),
            ),
          ),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(response)
  })
}

let getUserLists = (~sessionId) => {
  let url = Constants.apiUrl ++ "/@me/item-lists"
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let createItemList = (~sessionId, ~items: array<(int, int)>) => {
  let url = Constants.apiUrl ++ "/item-lists"
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify({
            open Json.Encode
            object_(list{("items", array(tuple2(int, int), items))})
          }),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ Belt.Option.getWithDefault(sessionId, ""),
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let cloneItemList = (~sessionId, ~listId) => {
  let url = Constants.apiUrl ++ ("/item-lists/" ++ (listId ++ "/clone"))
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let updateItemList = (~sessionId, ~listId, ~title=?, ~items: option<array<(int, int)>>=?, ()) => {
  let url = Constants.apiUrl ++ ("/item-lists/" ++ listId)
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Patch,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify({
            open Json.Encode
            object_(
              Belt.List.keepMap(
                list{
                  title->Belt.Option.map(title => ("title", string(title))),
                  items->Belt.Option.map(items => ("items", array(tuple2(int, int), items))),
                },
                x => x,
              ),
            )
          }),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let deleteItemList = (~sessionId, ~listId) => {
  let url = Constants.apiUrl ++ ("/item-lists/" ++ listId)
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let getItemList = (~listId: string) => {
  let url = Constants.apiUrl ++ ("/item-lists/" ++ listId)
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      url,
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
        }),
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
}

let followUser = (~userId, ~sessionId) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ ("/@me/follow/" ++ userId),
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    if Fetch.Response.status(response) < 300 {
      Promise.resolved(Ok())
    } else {
      %Repromise.JsExn({
        let text = Fetch.Response.text(response)
        Promise.resolved(Error(text))
      })
    }
  })

let unfollowUser = (~userId, ~sessionId) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ ("/@me/unfollow/" ++ userId),
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    if Fetch.Response.status(response) < 300 {
      Promise.resolved(Ok())
    } else {
      %Repromise.JsExn({
        let text = Fetch.Response.text(response)
        Promise.resolved(Error(text))
      })
    }
  })

let getFolloweesItem = (~sessionId, ~itemId) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ ("/@me/followees/items/" ++ string_of_int(itemId)),
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(response)
  })

let connectDiscordAccount = (~sessionId, ~code) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/discord",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(Json.Encode.object_(list{("code", Js.Json.string(code))})),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(response)
  })

let removeAllItems = (~sessionId) =>
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/items/all",
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })

let deleteAccount = (~sessionId, ~userId) =>
  %Repromise.Js({
    let responseResult = Fetch.fetchWithInit(
      Constants.apiUrl ++ ("/@me/" ++ userId),
      Fetch.RequestInit.make(
        ~method_=Delete,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
          "Authorization": "Bearer " ++ sessionId,
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved(responseResult)
  })
