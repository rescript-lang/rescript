open Belt

type state =
  | Loading
  | NotLoggedIn
  | LoggedIn(User.t)
type action =
  | Login(User.t)
  | UpdateUser(User.t)
  | Logout
  | FetchMeFailed
  | ConnectDiscordId(string)
  | FollowUser(string)
  | UnfollowUser(string)

let sessionId = ref(Dom.Storage.localStorage->Dom.Storage.getItem("sessionId"))
let updateSessionId = newValue => {
  sessionId := newValue
  open Dom.Storage
  switch newValue {
  | Some(sessionId) => localStorage->setItem("sessionId", sessionId)
  | None => localStorage->removeItem("sessionId")
  }
}

let api = Restorative.createStore(
  switch sessionId.contents {
  | Some(_) => Loading
  | None => NotLoggedIn
  },
  (state, action) =>
    switch action {
    | Login(user) => LoggedIn(user)
    | UpdateUser(user) => LoggedIn(user)
    | Logout => NotLoggedIn
    | FetchMeFailed => NotLoggedIn
    | ConnectDiscordId(discordId) =>
      switch state {
      | LoggedIn(user) => LoggedIn({...user, discordId: Some(discordId)})
      | _ => state
      }
    | FollowUser(followeeId) =>
      switch state {
      | LoggedIn(user) =>
        LoggedIn({
          ...user,
          followeeIds: user.followeeIds->Belt.Option.map(followeeIds =>
            Js.Array.includes(followeeId, followeeIds)
              ? followeeIds
              : followeeIds->Belt.Array.concat([followeeId])
          ),
        })
      | _ => state
      }
    | UnfollowUser(followeeId) =>
      switch state {
      | LoggedIn(user) =>
        LoggedIn({
          ...user,
          followeeIds: user.followeeIds->Belt.Option.map(followeeIds =>
            followeeIds->Belt.Array.keep(f => f !== followeeId)
          ),
        })
      | _ => state
      }
    },
)

let useStore = api.useStore
let useMe = () =>
  switch api.useStore() {
  | LoggedIn(user) => Some(user)
  | _ => None
  }
let useItem = (~itemId, ~variation) => {
  let selector = React.useCallback2((state: state) =>
    switch state {
    | LoggedIn(user) => user.items->Js.Dict.get(User.getItemKey(~itemId, ~variation))
    | _ => None
    }
  , (itemId, variation))
  api.useStoreWithSelector(selector, ())
}
let useIsLoggedIn = () => api.useStoreWithSelector(state =>
    switch state {
    | LoggedIn(_) => true
    | _ => false
    }
  , ())
exception ExpectedUser
let getUser = () =>
  switch api.getState() {
  | LoggedIn(user) => user
  | _ => raise(ExpectedUser)
  }
let getItem = (~itemId, ~variation) =>
  switch api.getState() {
  | LoggedIn(user) => user.items->Js.Dict.get(User.getItemKey(~itemId, ~variation))
  | _ => None
  }

let getUserOption = () =>
  switch api.getState() {
  | LoggedIn(user) => Some(user)
  | _ => None
  }
let isLoggedIn = () =>
  switch api.getState() {
  | LoggedIn(_) => true
  | _ => false
  }

let handleServerResponse = (url, responseResult) =>
  if (
    switch responseResult {
    | Error(_) => true
    | Ok(response) => Fetch.Response.status(response) >= 400
    }
  ) {
    Error.showPopup(
      ~message="Something went wrong. Sorry!\nRefresh your browser and try again.\nIf you are running into issues, please email hi@nook.exchange",
    )
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Error Dialog Shown",
      ~eventProperties={
        "url": url,
        "errorResponse": Js.Json.stringifyAny(
          switch responseResult {
          | Ok(response) => {
              "status": Fetch.Response.status(response),
              "statusText": Fetch.Response.statusText(response),
            }
          | Error(error) => Obj.magic(error)
          },
        )->Option.getExn,
      },
    )
  }

let makeAuthenticatedPostRequest = (~url, ~bodyJson) =>
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(Js.Json.stringify(Json.Encode.object_(bodyJson))),
      ~headers=Fetch.HeadersInit.make({
        "X-Client-Version": Constants.gitCommitRef,
        "Content-Type": "application/json",
        "Authorization": "Bearer " ++ Option.getWithDefault(sessionId.contents, ""),
      }),
      ~credentials=Include,
      ~mode=CORS,
      (),
    ),
  )

exception NotCanonicalVariant(int, int)
let numItemUpdatesLogged = ref(0)
let setItemStatus = (~itemId: int, ~variation: int, ~status: User.itemStatus) => {
  let item = Item.getItem(~itemId)
  if Item.getCanonicalVariant(~item, ~variant=variation) != variation {
    raise(NotCanonicalVariant(itemId, variation))
  }
  let user = getUser()
  let itemKey = User.getItemKey(~itemId, ~variation)
  let timeUpdated = Some(Js.Date.now() /. 1000.)
  let userItem = switch user.items->Js.Dict.get(itemKey) {
  | Some(item) => {...item, status: status, timeUpdated: timeUpdated}
  | None => {status: status, note: "", priorityTimestamp: None, timeUpdated: timeUpdated}
  }
  let updatedUser = {
    ...user,
    items: {
      let clone = Utils.cloneJsDict(user.items)
      clone->Js.Dict.set(itemKey, userItem)
      clone
    },
  }
  api.dispatch(UpdateUser(updatedUser))
  %Repromise({
    let responseResult = API.setItemStatus(
      ~userId=user.id,
      ~sessionId=sessionId.contents,
      ~itemId,
      ~variant=variation,
      ~status,
    )
    handleServerResponse("/@me/items/status", responseResult)
    Promise.resolved()
  })->ignore
  if numItemUpdatesLogged.contents < 2 || updatedUser.items->Js.Dict.keys->Js.Array.length < 4 {
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item Status Updated",
      ~eventProperties={
        "itemId": itemId,
        "variant": variation,
        "status": User.itemStatusToJs(status),
      },
    )
    numItemUpdatesLogged := numItemUpdatesLogged.contents + 1
  }
  Analytics.Amplitude.setItemCount(~itemCount=Js.Dict.keys(updatedUser.items)->Js.Array.length)
}

let setItemStatusBatch = (~items: array<(int, int)>, ~status) => {
  let user = getUser()
  let updatedUser = {
    ...user,
    items: {
      let clone = Utils.cloneJsDict(user.items)
      items->Js.Array.forEach(((itemId, variant)) => {
        let itemKey = User.getItemKey(~itemId, ~variation=variant)
        clone->Js.Dict.set(
          itemKey,
          switch user.items->Js.Dict.get(itemKey) {
          | Some(item) => {...item, status: status}
          | None => {
              status: status,
              note: "",
              priorityTimestamp: None,
              timeUpdated: None,
            }
          },
        )
      })
      clone
    },
  }
  api.dispatch(UpdateUser(updatedUser))
  %Repromise({
    let responseResult = API.setItemStatusBatch(
      ~sessionId=Belt.Option.getExn(sessionId.contents),
      ~items,
      ~status,
    )
    handleServerResponse("/@me/items/batch/status", responseResult)
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item Status Batch Updated",
      ~eventProperties={
        "numItems": Js.Array.length(items),
        "status": User.itemStatusToJs(status),
      },
    )
    Promise.resolved()
  })->ignore
  Analytics.Amplitude.setItemCount(~itemCount=Js.Dict.keys(updatedUser.items)->Js.Array.length)
}

let setItemNote = (~itemId: int, ~variation: int, ~note: string) => {
  let item = Item.getItem(~itemId)
  if Item.getCanonicalVariant(~item, ~variant=variation) != variation {
    raise(NotCanonicalVariant(itemId, variation))
  }
  let user = getUser()
  let itemKey = User.getItemKey(~itemId, ~variation)
  let userItem = {
    ...Belt.Option.getExn(user.items->Js.Dict.get(itemKey)),
    note: note,
  }
  let updatedUser = {
    ...user,
    items: {
      let clone = Utils.cloneJsDict(user.items)
      clone->Js.Dict.set(itemKey, userItem)
      clone
    },
  }
  api.dispatch(UpdateUser(updatedUser))
  let item = Item.getItem(~itemId)
  %Repromise({
    let responseResult = API.setItemNote(
      ~userId=user.id,
      ~sessionId=sessionId.contents,
      ~itemId,
      ~variant=variation,
      ~note,
    )
    handleServerResponse("/@me/items/note", responseResult)
    Promise.resolved()
  })->ignore
  if numItemUpdatesLogged.contents < 2 || updatedUser.items->Js.Dict.keys->Js.Array.length < 4 {
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item Note Updated",
      ~eventProperties={
        "itemId": item.id,
        "variant": variation,
        "note": note,
      },
    )
    numItemUpdatesLogged := numItemUpdatesLogged.contents + 1
  }
}

let didLogItemPriority = ref(false)
let setItemPriority = (~itemId: int, ~variant: int, ~isPriority: bool) => {
  let user = getUser()
  let itemKey = User.getItemKey(~itemId, ~variation=variant)
  let userItem = {
    ...Belt.Option.getExn(user.items->Js.Dict.get(itemKey)),
    priorityTimestamp: isPriority ? Some(Js.Date.now()) : None,
  }
  let updatedUser = {
    ...user,
    items: {
      let clone = Utils.cloneJsDict(user.items)
      clone->Js.Dict.set(itemKey, userItem)
      clone
    },
  }
  api.dispatch(UpdateUser(updatedUser))
  let item = Item.getItem(~itemId)
  %Repromise({
    let responseResult = API.setItemPriority(
      ~sessionId=sessionId.contents,
      ~itemId,
      ~variant,
      ~isPriority,
    )
    handleServerResponse("/@me/items/priority", responseResult)
    Promise.resolved()
  })->ignore
  if !didLogItemPriority.contents {
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item Priority Updated",
      ~eventProperties={
        "itemId": item.id,
        "variant": variant,
        "isPriority": isPriority,
      },
    )
    didLogItemPriority := true
  }
}

let numItemRemovesLogged = ref(0)
let removeItem = (~itemId, ~variation) => {
  let user = getUser()
  let key = User.getItemKey(~itemId, ~variation)
  if user.items->Js.Dict.get(key)->Option.isSome {
    let updatedUser = {
      ...user,
      items: {
        let clone = Utils.cloneJsDict(user.items)
        Utils.deleteJsDictKey(clone, key)
        clone
      },
    }
    api.dispatch(UpdateUser(updatedUser))
    let item = Item.getItem(~itemId)
    if numItemRemovesLogged.contents < 3 {
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Item Removed",
        ~eventProperties={"itemId": item.id, "variant": variation},
      )
      numItemRemovesLogged := numItemRemovesLogged.contents + 1
    }
    %Repromise({
      let responseResult = API.removeItem(
        ~userId=user.id,
        ~sessionId=sessionId.contents,
        ~itemId,
        ~variant=variation,
      )
      handleServerResponse("/@me/items/remove", responseResult)
      Promise.resolved()
    })->ignore
    Analytics.Amplitude.setItemCount(~itemCount=Js.Dict.keys(updatedUser.items)->Js.Array.length)
  }
}

let removeItems = (~items: array<(int, int)>) => {
  let user = getUser()
  let updatedUser = {
    ...user,
    items: {
      let clone = Utils.cloneJsDict(user.items)
      items->Js.Array.forEach(((itemId, variant)) => {
        let key = User.getItemKey(~itemId, ~variation=variant)
        Utils.deleteJsDictKey(clone, key)
      })
      clone
    },
  }
  api.dispatch(UpdateUser(updatedUser))
  %Repromise({
    let responseResult = API.removeItems(~sessionId=Belt.Option.getExn(sessionId.contents), ~items)
    handleServerResponse("/@me/items/batch/remove", responseResult)
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item Batch Removed",
      ~eventProperties={"numItems": Js.Array.length(items)},
    )
    Promise.resolved()
  })->ignore
  Analytics.Amplitude.setItemCount(~itemCount=Js.Dict.keys(updatedUser.items)->Js.Array.length)
}

let updateProfileText = (~profileText) => {
  let user = getUser()
  let updatedUser = {...user, profileText: profileText}
  api.dispatch(UpdateUser(updatedUser))
  %Repromise({
    let responseResult = API.updateProfileText(
      ~userId=user.id,
      ~sessionId=sessionId.contents,
      ~profileText,
    )
    handleServerResponse("/@me/profileText", responseResult)
    Promise.resolved()
  })->ignore
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Profile Text Updated",
    ~eventProperties={"text": profileText},
  )
}

let patchMe = (~username=?, ~newPassword=?, ~email=?, ~oldPassword=?, ()) => {
  let user = getUser()
  %Repromise({
    let response = API.patchMe(
      ~userId=user.id,
      ~sessionId=sessionId.contents,
      ~username,
      ~newPassword,
      ~email,
      ~oldPassword,
    )
    if Fetch.Response.status(response) < 300 {
      let user = getUser()
      let updatedUser = {
        ...user,
        username: username->Belt.Option.getWithDefault(user.username),
        email: switch email {
        | Some(email) => email != "" ? Some(email) : None
        | None => user.email
        },
      }
      api.dispatch(UpdateUser(updatedUser))
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Account Update Succeeded",
        ~eventProperties={
          "username": username,
          "password": newPassword !== None,
          "email": email,
        },
      )
      Promise.resolved(Ok())
    } else {
      %Repromise.JsExn({
        let error = Fetch.Response.text(response)
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Account Update Failed",
          ~eventProperties={"error": error},
        )
        Promise.resolved(Error(error))
      })
    }
  })
}

let toggleCatalogCheckboxSetting = (~enabled) => {
  let user = getUser()
  let updatedUser = {...user, enableCatalogCheckbox: enabled}
  api.dispatch(UpdateUser(updatedUser))
  %Repromise({
    let responseResult = API.updateSetting(
      ~userId=user.id,
      ~sessionId=sessionId.contents,
      ~settingKey="enableCatalog",
      ~settingValue=Js.Json.boolean(enabled),
    )
    handleServerResponse("/@me/toggleCatalogCheckboxSetting", responseResult)
    Promise.resolved()
  })->ignore
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Catalog Checkbox Setting Toggled",
    ~eventProperties={"enabled": enabled},
  )
}

let errorQuotationMarksRegex = /^"(.*)"$/
let register = (~username, ~email, ~password) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/register2",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(
            Json.Encode.object_(list{
              ("username", Js.Json.string(username)),
              ("email", Js.Json.string(email)),
              ("password", Js.Json.string(password)),
            }),
          ),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    if Fetch.Response.status(response) < 300 {
      %Repromise.JsExn({
        let json = Fetch.Response.json(response)
        updateSessionId(
          json->{
            open Json.Decode
            optional(field("sessionId", string))
          },
        )
        let user = User.fromAPI(json)
        api.dispatch(Login(user))
        Analytics.Amplitude.setUserId(~userId=Some(user.id))
        Analytics.Amplitude.setUsername(~username, ~email)
        Promise.resolved(Ok(user))
      })
    } else {
      %Repromise.JsExn({
        let text = Fetch.Response.text(response)
        let result = text->Js.Re.exec_(errorQuotationMarksRegex)
        let text = switch result {
        | Some(match_) =>
          let captures = Js.Re.captures(match_)
          captures[1]->Option.getExn->Js.Nullable.toOption->Option.getExn
        | None => text
        }
        Promise.resolved(Error(text))
      })
    }
  })

let loginWithDiscord = (~code, ~isRegister) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/login-discord",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(
            Json.Encode.object_(list{
              ("code", Js.Json.string(code)),
              ("isRegister", Js.Json.boolean(isRegister)),
            }),
          ),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    if Fetch.Response.status(response) < 300 {
      %Repromise.JsExn({
        let json = Fetch.Response.json(response)
        updateSessionId(
          json->{
            open Json.Decode
            optional(field("sessionId", string))
          },
        )
        let user = User.fromAPI(json)
        api.dispatch(Login(user))
        Analytics.Amplitude.setUserId(~userId=Some(user.id))
        Analytics.Amplitude.setUsername(
          ~username=user.username,
          ~email=user.email->Belt.Option.getWithDefault(""),
        )
        let isRegister =
          (json->{
            open Json.Decode
            optional(field("isRegister", bool))
          })->Belt.Option.getWithDefault(false)
        if isRegister {
          Analytics.Amplitude.logEventWithProperties(
            ~eventName="Registration Succeeded",
            ~eventProperties={
              "username": user.username,
              "email": user.email,
              "discordId": user.discordId,
            },
          )
        }
        Promise.resolved(Ok((user, isRegister)))
      })
    } else {
      %Repromise.JsExn({
        let text = Fetch.Response.text(response)
        let result = text->Js.Re.exec_(errorQuotationMarksRegex)
        let text = switch result {
        | Some(match_) =>
          let captures = Js.Re.captures(match_)
          captures[1]->Option.getExn->Js.Nullable.toOption->Option.getExn
        | None => text
        }
        Promise.resolved(Error(text))
      })
    }
  })

let followUser = (~userId) =>
  %Repromise({
    let response = API.followUser(~userId, ~sessionId=Belt.Option.getExn(sessionId.contents))
    switch response {
    | Ok() => api.dispatch(FollowUser(userId))
    | Error(_) => ()
    }
    Promise.resolved(response)
  })

let unfollowUser = (~userId) =>
  %Repromise({
    let response = API.unfollowUser(~userId, ~sessionId=Belt.Option.getExn(sessionId.contents))
    switch response {
    | Ok() => api.dispatch(UnfollowUser(userId))
    | Error(_) => ()
    }
    Promise.resolved(response)
  })

let login = (~username, ~password) =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/login",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~body=Fetch.BodyInit.make(
          Js.Json.stringify(
            Js.Json.object_(
              Js.Dict.fromArray([
                ("username", Js.Json.string(username)),
                ("password", Js.Json.string(password)),
              ]),
            ),
          ),
        ),
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Content-Type": "application/json",
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    if Fetch.Response.status(response) < 300 {
      %Repromise.JsExn({
        let json = Fetch.Response.json(response)
        updateSessionId(
          json->{
            open Json.Decode
            optional(field("sessionId", string))
          },
        )
        let user = User.fromAPI(json)
        api.dispatch(Login(user))
        Analytics.Amplitude.setUserId(~userId=Some(user.id))
        Promise.resolved(Ok(user))
      })
    } else {
      Promise.resolved(Error())
    }
  })

let logout = () => {
  api.dispatch(Logout)
  Dom.Storage.localStorage->Dom.Storage.removeItem("sessionId")
  Analytics.Amplitude.setUserId(~userId=None)
  ReasonReactRouter.push("/")
  let sessionId = sessionId.contents
  updateSessionId(None)
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/logout",
      Fetch.RequestInit.make(
        ~method_=Post,
        ~headers=?Option.map(sessionId, sessionId =>
          Fetch.HeadersInit.make({
            "X-Client-Version": Constants.gitCommitRef,
            "Authorization": "Bearer " ++ sessionId,
          })
        ),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    Promise.resolved()
  })
}

let removeAllItems = () => {
  let sessionId = Belt.Option.getExn(sessionId.contents)
  %Repromise({
    let responseResult = API.removeAllItems(~sessionId)
    handleServerResponse("/@me/items/all/delete", responseResult)
    switch responseResult {
    | Ok(response) =>
      if Fetch.Response.status(response) < 300 {
        let user = getUser()
        api.dispatch(UpdateUser({...user, items: Js.Dict.empty()}))
      }
    | Error(_) => ()
    }
    Promise.resolved()
  })
}

let deleteAccount = () => {
  let sessionId = Belt.Option.getExn(sessionId.contents)
  %Repromise({
    let responseResult = API.deleteAccount(~sessionId, ~userId=getUser().id)
    handleServerResponse("/@me/delete", responseResult)
    switch responseResult {
    | Ok(response) =>
      if Fetch.Response.status(response) < 300 {
        api.dispatch(Logout)
        Dom.Storage.localStorage->Dom.Storage.removeItem("sessionId")
        Analytics.Amplitude.setUserId(~userId=None)
        ReasonReactRouter.push("/")
        updateSessionId(None)
      }
    | Error(_) => ()
    }
    Promise.resolved()
  })
}

let connectDiscordAccount = (~code) => {
  let processLoggedIn = user =>
    %Repromise({
      let response = API.connectDiscordAccount(
        ~sessionId=Belt.Option.getExn(sessionId.contents),
        ~code,
      )
      %Repromise.JsExn({
        let json = Fetch.Response.json(response)
        let discordId = json->{
          open Json.Decode
          field("discordId", string)
        }
        api.dispatch(ConnectDiscordId(discordId))
        ReasonReactRouter.push("/settings")
        Promise.resolved()
      })
    })->ignore
  // TODO: error
  let processNotLoggedIn = () => ()
  switch api.getState() {
  | Loading =>
    let unsubscribe = ref(() => ())
    unsubscribe := api.subscribe(state =>
        switch state {
        | Loading => ()
        | LoggedIn(user) =>
          processLoggedIn(user)
          unsubscribe.contents()
        | NotLoggedIn =>
          processNotLoggedIn()
          unsubscribe.contents()
        }
      , ())
  | LoggedIn(user) => processLoggedIn(user)
  | NotLoggedIn => processNotLoggedIn()
  }
}

let init = () =>
  switch sessionId.contents {
  | Some(sessionId) =>
    %Repromise.JsExn({
      let response = Fetch.fetchWithInit(
        Constants.apiUrl ++ "/@me",
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
      if Fetch.Response.status(response) < 400 {
        %Repromise.JsExn({
          let json = Fetch.Response.json(response)
          switch json->{
            open Json.Decode
            optional(field("sessionId", string))
          } {
          | Some(sessionId) => updateSessionId(Some(sessionId))
          | None => ()
          }
          let user = User.fromAPI(json)
          api.dispatch(Login(user))
          Analytics.Amplitude.setUserId(~userId=Some(user.id))
          Promise.resolved()
        })
      } else {
        api.dispatch(FetchMeFailed)
        Promise.resolved()
      }
    })->ignore
  | None => ()
  }
