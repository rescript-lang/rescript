type t = {
  id: option<string>,
  title: option<string>,
  userId: option<string>,
  itemIds: array<(int, int)>,
}

type action =
  | StartList
  | LoadList(string, option<string>, array<(int, int)>)
  | AddItem(int, int)
  | RemoveItem(int, int)
  | UpdateTitle(string, option<string>)
  | RemoveList

open Belt

let api = Restorative.createStore(None, (state, action) =>
  switch action {
  | StartList => Some({id: None, title: None, userId: None, itemIds: []})
  | LoadList(id, title, itemIds) =>
    Some({id: Some(id), title: title, userId: None, itemIds: itemIds})
  | AddItem(itemId, variant) =>
    state->Option.map(state => {
      ...state,
      itemIds: state.itemIds->Array.concat([(itemId, variant)]),
    })
  | RemoveItem(itemId, variant) =>
    state->Option.map(state => {
      ...state,
      itemIds: state.itemIds->Array.keep(((a, b)) => a != itemId || b != variant),
    })
  | UpdateTitle(listId, title) =>
    state->Option.map(state =>
      if state.id == Some(listId) {
        {
          ...state,
          title: title,
        }
      } else {
        state
      }
    )
  | RemoveList => None
  }
)

let useHasQuicklist = () => api.useStoreWithSelector(state => state != None, ())
let useQuicklist = () => api.useStore()

let useItemState = (~itemId, ~variant) => api.useStoreWithSelector(state =>
    switch state {
    | Some(state) => state.itemIds->Array.getBy(((a, b)) => a == itemId && b == variant) !== None
    | None => false
    }
  , ())

let startList = () => {
  api.dispatch(StartList)
  Analytics.Amplitude.logEventWithProperties(
    ~eventName="Item List Started",
    ~eventProperties={
      "path": {
        open Webapi.Dom
        location->Location.pathname
      },
      "isLoggedIn": UserStore.isLoggedIn(),
      "isViewingSelf": switch UserStore.getUserOption() {
      | Some(user) =>
        let url = ReasonReactRouter.dangerouslyGetInitialUrl()
        switch url.path {
        | list{"u", username, ..._} =>
          Js.String.toLowerCase(username) == Js.String.toLowerCase(user.username)
        | _ => false
        }
      | None => false
      },
    },
  )
}

let removeList = () => api.dispatch(RemoveList)

let hasLoggedItemListUpdate = ref(false)
let saveList = () => {
  let list = api.getState()->Belt.Option.getExn
  let itemIds = list.itemIds
  switch list.id {
  | Some(listId) =>
    %Repromise({
      let responseResult = API.updateItemList(
        ~sessionId=Belt.Option.getExn(UserStore.sessionId.contents),
        ~listId,
        ~items=itemIds,
        (),
      )
      UserStore.handleServerResponse("/item-lists/patch", responseResult)
      if !hasLoggedItemListUpdate.contents {
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Item List Updated",
          ~eventProperties={
            "listId": listId,
            "numItems": Js.Array.length(itemIds),
          },
        )
        hasLoggedItemListUpdate := true
      }
      Promise.resolved(listId)
    })
  | None =>
    %Repromise({
      let responseResult = API.createItemList(
        ~sessionId=UserStore.sessionId.contents,
        ~items=itemIds,
      )
      UserStore.handleServerResponse("/item-lists", responseResult)
      let response = Belt.Result.getExn(responseResult)
      %Repromise.JsExn({
        let json = Fetch.Response.json(response)
        let listId = json->{
          open Json.Decode
          field("id", string)
        }
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Item List Created",
          ~eventProperties={
            "listId": listId,
            "numItems": Js.Array.length(itemIds),
          },
        )
        Promise.resolved(listId)
      })
    })
  }
}

let hasLoggedItemAdd = ref(false)
let addItem = (~itemId, ~variant) => {
  api.dispatch(AddItem(itemId, variant))
  switch api.getState() {
  | Some(quicklist) =>
    switch quicklist.id {
    | Some(_listId) => saveList()->ignore
    | None =>
      if !hasLoggedItemAdd.contents {
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Item List Item Added",
          ~eventProperties={"itemId": itemId, "variant": variant},
        )
        hasLoggedItemAdd := true
      }
    }
  | None => ()
  }
}

let hasLoggedItemRemove = ref(false)
let removeItem = (~itemId, ~variant) => {
  api.dispatch(RemoveItem(itemId, variant))
  switch api.getState() {
  | Some(quicklist) =>
    switch quicklist.id {
    | Some(_listId) => saveList()->ignore
    | None =>
      if !hasLoggedItemRemove.contents {
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Item List Item Removed",
          ~eventProperties={"itemId": itemId, "variant": variant},
        )
        hasLoggedItemRemove := true
      }
    }
  | None => ()
  }
}

let updateListTitle = (~listId, ~title: option<string>) => {
  api.dispatch(UpdateTitle(listId, title))
  %Repromise({
    let responseResult = API.updateItemList(
      ~sessionId=Option.getExn(UserStore.sessionId.contents),
      ~listId,
      ~title=title->Option.getWithDefault(""),
      (),
    )
    UserStore.handleServerResponse("/item-lists/patch", responseResult)
    Analytics.Amplitude.logEventWithProperties(
      ~eventName="Item List Title Updated",
      ~eventProperties={"listId": listId, "title": title},
    )
    Promise.resolved()
  })
}

let loadList = (~listId, ~title, ~listItems) => api.dispatch(LoadList(listId, title, listItems))
