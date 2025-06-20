let cardWidth = 176

module Styles = {
  open Css
  let root = style(list{
    display(flexBox),
    flexDirection(column),
    margin2(~v=zero, ~h=auto),
    media("(max-width: 630px)", list{marginLeft(px(16)), marginRight(px(16))}),
  })
  let cloneMessage = style(list{
    backgroundColor(hex("ffffffc0")),
    display(flexBox),
    alignItems(center),
    justifyContent(spaceBetween),
    marginBottom(px(32)),
    padding(px(16)),
    borderRadius(px(8)),
  })
  let cloneMessageDismissLink = style(list{
    textDecoration(none),
    hover(list{textDecoration(underline)}),
  })
  let myListsLinkText = style(list{marginLeft(px(4))})
  let myListsLink = style(list{
    textDecoration(none),
    hover(list{selector("& ." ++ myListsLinkText, list{textDecoration(underline)})}),
  })
  let titleForm = style(list{display(flexBox)})
  let titleInput = style(list{
    boxSizing(borderBox),
    borderRadius(px(4)),
    border(px(1), solid, transparent),
    backgroundColor(transparent),
    padding3(~top=px(6), ~bottom=px(6), ~h=px(7)),
    marginLeft(px(-8)),
    fontSize(px(24)),
    flexGrow(1.),
    transition(~duration=200, "all"),
    focus(list{backgroundColor(Colors.white), borderColor(Colors.lightGreen)}),
    media(
      "(hover: hover)",
      list{hover(list{backgroundColor(Colors.white), borderColor(Colors.lightGreen)})},
    ),
    media("(hover: none)", list{borderColor(Colors.lightGreen)}),
  })
  let titleInputHasChanges = style(list{
    important(backgroundColor(Colors.white)),
    important(borderColor(Colors.lightGreen)),
  })
  let titleSubmitButton = style(list{
    unsafe("alignSelf", "center"),
    marginLeft(px(16)),
    paddingLeft(px(16)),
    paddingRight(px(16)),
  })
  let titleSubmitRow = style(list{display(flexBox), justifyContent(flexStart), marginTop(px(6))})
  let listTitle = style(list{fontSize(px(28))})
  let gridWidth = numCards => numCards * cardWidth + (numCards - 1) * 16
  let rootGrid = style(list{
    width(px(gridWidth(4))),
    media("(max-width: 840px)", list{width(px(gridWidth(3)))}),
    media("(max-width: 630px)", list{width(auto)}),
  })
  let rootList = style(list{maxWidth(px(560))})
  let rootThumbnail = style(list{maxWidth(px(594))})
  let topRow = style(list{
    display(flexBox),
    alignItems(center),
    flexWrap(wrap),
    justifyContent(spaceBetween),
    marginBottom(px(16)),
  })
  let topRowLeft = style(list{flexGrow(1.), marginRight(px(16)), marginTop(px(8))})
  let listUserLink = style(list{
    textDecoration(none),
    media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
  })
  let viewToggles = style(list{display(flexBox), justifyContent(flexEnd), marginTop(px(8))})
  let sortSelector = style(list{
    height(px(30)),
    boxSizing(borderBox),
    marginTop(px(8)),
    marginRight(px(8)),
    media("(max-width: 520px)", list{marginRight(zero)}),
  })
  @module("./assets/grid.png") external gridPng: string = "default"
  @module("./assets/thumbnail.png")
  external thumbnailPng: string = "default"
  @module("./assets/list.png") external listPng: string = "default"
  let viewButton = style(list{
    display(flexBox),
    borderWidth(zero),
    alignItems(center),
    backgroundColor(Colors.green),
    color(Colors.white),
    borderRadius(px(4)),
    padding2(~v=px(6), ~h=px(10)),
    marginLeft(px(8)),
    opacity(0.5),
    transition(~duration=200, "all"),
    firstChild(list{marginLeft(zero)}),
    hover(list{opacity(0.8)}),
  })
  let viewButtonSelected = style(list{important(opacity(1.))})
  let gridIcon = style(list{
    display(inlineBlock),
    width(px(18)),
    height(px(18)),
    backgroundImage(url(gridPng)),
    backgroundSize(cover),
    marginRight(px(8)),
  })
  let thumbnailIcon = style(list{
    display(inlineBlock),
    width(px(18)),
    height(px(18)),
    backgroundImage(url(thumbnailPng)),
    backgroundSize(cover),
    marginRight(px(8)),
  })
  let listIcon = style(list{
    display(inlineBlock),
    width(px(18)),
    height(px(18)),
    backgroundImage(url(listPng)),
    backgroundSize(cover),
    marginRight(px(8)),
  })
  let list = style(list{
    borderRadius(px(8)),
    overflow(hidden),
    border(px(1), solid, Colors.lightGreen),
  })
  let grid = style(list{display(flexBox), flexWrap(wrap), marginRight(px(-16))})
  let thumbnails = style(list{
    display(flexBox),
    flexWrap(wrap),
    justifyContent(center),
    backgroundColor(Colors.white),
    borderRadius(px(8)),
    border(px(1), solid, Colors.lightGreen),
    padding(px(8)),
  })
  let listFooter = style(list{
    marginTop(px(32)),
    display(flexBox),
    flexDirection(column),
    alignItems(flexEnd),
  })
  let numItems = style(list{color(Colors.gray)})
  let deleteListLink = style(list{
    textDecoration(none),
    media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
  })
  let cloneListLink = style(list{
    textDecoration(none),
    media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
  })
}

module ListRow = {
  module Styles = {
    open Css
    let itemName = style(list{flexGrow(1.), padding2(~v=zero, ~h=px(16))})
    let row = style(list{
      backgroundColor(Colors.white),
      padding2(~v=px(8), ~h=px(8)),
      borderTop(px(1), solid, Colors.faintGray),
      firstChild(list{borderTopWidth(zero)}),
      display(flexBox),
      alignItems(center),
      fontSize(px(16)),
      color(Colors.charcoal),
      textDecoration(none),
      hover(list{
        backgroundColor(Colors.faintGreen),
        borderTopColor(Colors.lightGreen),
        // color(Colors.white),
        // selector("& ." ++ itemName, [textDecoration(underline)]),
      }),
    })
    let variantName = style(list{
      color(Colors.gray),
      padding2(~v=zero, ~h=px(8)),
      textAlign(#right),
    })
    let imageWrapper = style(list{position(relative), fontSize(zero)})
    let image = style(list{width(px(48)), height(px(48))})
    let recipeIcon = style(list{
      position(absolute),
      bottom(px(-4)),
      right(px(-4)),
      width(px(24)),
      height(px(24)),
    })
  }

  @react.component
  let make = (~itemId, ~variant) => {
    let item = Item.getItem(~itemId)
    <Link path={Utils.getItemDetailUrl(~itemId, ~variant=Some(variant))} className=Styles.row>
      <div className=Styles.imageWrapper>
        <img src={Item.getImageUrl(~item, ~variant)} className=Styles.image />
        {Item.isRecipe(~item)
          ? <img src={Constants.cdnUrl ++ "/images/DIYRecipe.png"} className=Styles.recipeIcon />
          : React.null}
      </div>
      <div className=Styles.itemName> {React.string(Item.getName(item))} </div>
      {switch Item.getVariantName(~item, ~variant, ~hidePattern=true, ()) {
      | Some(variantName) => <div className=Styles.variantName> {React.string(variantName)} </div>
      | None => React.null
      }}
    </Link>
  }
}

type viewMode =
  | Grid
  | Thumbnail
  | List

@react.component
let make = (~listId, ~url: ReasonReactRouter.url) => {
  let (showCloneMessage, setShowCloneMessage) = React.useState(() => {
    open Webapi.Url.URLSearchParams
    make(url.search)->has("clone")
  })
  let (list, setList) = React.useState(() => None)
  let (editTitle, setEditTitle) = React.useState(() => "")
  React.useEffect0(() => {
    %Repromise({
      let response = API.getItemList(~listId)
      %Repromise.JsExn({
        let json = Fetch.Response.json(Belt.Result.getExn(response))
        open Json.Decode
        let list: QuicklistStore.t = {
          id: Some(json->field("id", string)),
          title: (json->optional(field("title", string)))
            ->Belt.Option.flatMap(title => title != "" ? Some(title) : None),
          userId: json->optional(field("userId", string)),
          itemIds: json->field("itemIds", array(tuple2(int, int))),
        }
        let username = json->optional(field("username", string))
        setList(_ => Some((list, username)))
        setEditTitle(_ => list.title->Belt.Option.getWithDefault(""))
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Item List Page Viewed",
          ~eventProperties={
            "listId": listId,
            "numItems": Js.Array.length(list.itemIds),
          },
        )
        Promise.resolved()
      })
    })->ignore
    None
  })
  React.useEffect0(() => {
    if showCloneMessage {
      ReasonReactRouter.replace("/l/" ++ listId)
    }
    None
  })

  let onTitleSubmit = e => {
    ReactEvent.Form.preventDefault(e)
    let editTitle = editTitle->Js.String.slice(~from=0, ~to_=48)
    QuicklistStore.updateListTitle(~listId, ~title=Some(editTitle))->ignore
    setList(list =>
      list->Belt.Option.map(((list, username)) => (
        {...list, title: editTitle != "" ? Some(editTitle) : None},
        username,
      ))
    )
  }

  let (viewMode, setViewMode) = React.useState(() => List)
  let me = UserStore.useMe()
  let numViewToggleLoggedRef = React.useRef(0)
  let searchParams = Webapi.Url.URLSearchParams.make(url.search)
  let sort = ItemFilters.sortFromUrlSearch(~searchParams, ~defaultSort=ListTimeAdded)

  <div
    className={Cn.make(list{
      Styles.root,
      switch viewMode {
      | Grid => Styles.rootGrid
      | List => Styles.rootList
      | Thumbnail => Styles.rootThumbnail
      },
    })}>
    <div>
      {showCloneMessage
        ? <div className=Styles.cloneMessage>
            <span> {React.string(j`This is your clone. Edit away!️`)} </span>
            <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                setShowCloneMessage(_ => false)
              }}
              className=Styles.cloneMessageDismissLink>
              {React.string("Hide")}
            </a>
          </div>
        : React.null}
      {switch list {
      | Some((list, _)) =>
        list.userId
        ->Belt.Option.flatMap(userId =>
          if me->Belt.Option.map(me => me.id) == Some(userId) {
            let hasChanges =
              list.title != Some(editTitle) && !(editTitle == "" && list.title === None)
            Some(
              <form onSubmit=onTitleSubmit>
                <div>
                  <Link path="/lists" className=Styles.myListsLink>
                    {React.string(j`←`)}
                    <span className=Styles.myListsLinkText> {React.string("My Lists")} </span>
                  </Link>
                </div>
                <div className=Styles.titleForm>
                  <input
                    type_="text"
                    value=editTitle
                    onChange={e => {
                      let value = ReactEvent.Form.target(e)["value"]
                      setEditTitle(_ => value)
                    }}
                    className={Cn.make(list{
                      Styles.titleInput,
                      Cn.ifTrue(Styles.titleInputHasChanges, hasChanges),
                    })}
                    placeholder="Name your list"
                  />
                  {if hasChanges {
                    <Button className=Styles.titleSubmitButton> {React.string("Save")} </Button>
                  } else {
                    React.null
                  }}
                </div>
              </form>,
            )
          } else {
            None
          }
        )
        ->Belt.Option.getWithDefault(
          switch list.title {
          | Some(title) => <div className=Styles.listTitle> {Emoji.parseText(title)} </div>
          | None => React.null
          },
        )
      | None => React.null
      }}
    </div>
    <div className=Styles.topRow>
      <div className=Styles.topRowLeft>
        {switch list {
        | Some((list, username)) =>
          switch list.userId {
          | Some(userId) =>
            if me->Belt.Option.map(me => me.id) == Some(userId) {
              <div>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    QuicklistStore.loadList(~listId, ~title=list.title, ~listItems=list.itemIds)
                    ReasonReactRouter.push("/")
                  }}
                  className=Styles.listUserLink>
                  {React.string("Edit your list")}
                </a>
              </div>
            } else {
              switch username {
              | Some(username) =>
                <Link path={"/u/" ++ username} className=Styles.listUserLink>
                  {React.string("Visit " ++ (username ++ "'s profile"))}
                </Link>
              | None => React.null
              }
            }
          | None => React.null
          }
        | None => React.null
        }}
      </div>
      {switch list {
      | Some((list, _)) =>
        if Js.Array.length(list.itemIds) >= 4 {
          <ItemFilters.SortSelector
            sort
            onChange={sort => {
              let p = []
              switch ItemFilters.serializeSort(~sort, ~defaultSort=ListTimeAdded) {
              | Some(param) => p->Js.Array.push(param)->ignore
              | None => ()
              }
              let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(p)
              ReasonReactRouter.push(ItemBrowser.getUrl(~url, ~urlSearchParams))
            }}
            onListPage=true
            userItemIds=None
            isViewingSelf=false
            className=Styles.sortSelector
          />
        } else {
          React.null
        }
      | None => React.null
      }}
      <div className=Styles.viewToggles>
        <button
          onClick={_ => {
            setViewMode(_ => List)
            if React.Ref.current(numViewToggleLoggedRef) < 5 {
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Item List Page View Toggled",
                ~eventProperties={
                  "view": "list",
                  "listId": listId,
                  "numItems": switch list {
                  | Some((list, _)) => Js.Array.length(list.itemIds)
                  | None => 0
                  },
                },
              )
              React.Ref.setCurrent(
                numViewToggleLoggedRef,
                React.Ref.current(numViewToggleLoggedRef) + 1,
              )
            }
          }}
          className={Cn.make(list{
            Styles.viewButton,
            Cn.ifTrue(Styles.viewButtonSelected, viewMode == List),
          })}>
          <span className=Styles.listIcon /> {React.string("List")}
        </button>
        <button
          onClick={_ => {
            setViewMode(_ => Grid)
            if React.Ref.current(numViewToggleLoggedRef) < 5 {
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Item List Page View Toggled",
                ~eventProperties={
                  "view": "grid",
                  "listId": listId,
                  "numItems": switch list {
                  | Some((list, _)) => Js.Array.length(list.itemIds)
                  | None => 0
                  },
                },
              )
              React.Ref.setCurrent(
                numViewToggleLoggedRef,
                React.Ref.current(numViewToggleLoggedRef) + 1,
              )
            }
          }}
          className={Cn.make(list{
            Styles.viewButton,
            Cn.ifTrue(Styles.viewButtonSelected, viewMode == Grid),
          })}>
          <span className=Styles.gridIcon /> {React.string("Grid")}
        </button>
        {switch list {
        | Some((list, _)) => Js.Array.length(list.itemIds) > 8
        | None => false
        }
          ? <button
              onClick={_ => {
                setViewMode(_ => Thumbnail)
                if React.Ref.current(numViewToggleLoggedRef) < 5 {
                  Analytics.Amplitude.logEventWithProperties(
                    ~eventName="Item List Page View Toggled",
                    ~eventProperties={
                      "view": "thumbnail",
                      "listId": listId,
                      "numItems": switch list {
                      | Some((list, _)) => Js.Array.length(list.itemIds)
                      | None => 0
                      },
                    },
                  )
                  React.Ref.setCurrent(
                    numViewToggleLoggedRef,
                    React.Ref.current(numViewToggleLoggedRef) + 1,
                  )
                }
              }}
              className={Cn.make(list{
                Styles.viewButton,
                Cn.ifTrue(Styles.viewButtonSelected, viewMode == Thumbnail),
              })}>
              <span className=Styles.thumbnailIcon /> {React.string("Thumbnail")}
            </button>
          : React.null}
      </div>
    </div>
    {switch list {
    | Some((list, _)) =>
      <div>
        {if Js.Array.length(list.itemIds) > 0 {
          <div
            className={switch viewMode {
            | Grid => Styles.grid
            | List => Styles.list
            | Thumbnail => Styles.thumbnails
            }}>
            {list.itemIds
            ->(sort !== ListTimeAdded
              ? x => {
                  let sortFn = ItemFilters.getSort(~sort)
                  x->Js.Array.sortInPlaceWith(((aItemId, aVariant), (bItemId, bVariant)) => {
                    let aItem = Item.getItem(~itemId=aItemId)
                    let bItem = Item.getItem(~itemId=bItemId)
                    switch sortFn(aItem, bItem) {
                    | 0 => aVariant - bVariant
                    | y => y
                    }
                  })
                }
              : x => x)
            ->Js.Array.mapi(((itemId, variant), i) =>
              switch viewMode {
              | Grid =>
                <UserItemCard
                  itemId
                  variation=variant
                  editable=false
                  showRecipe=false
                  showMetaIcons=false
                  key={string_of_int(i)}
                />
              | Thumbnail =>
                <UserProfileBrowser.UserItemCardMini
                  itemId variation=variant key={string_of_int(i)}
                />
              | List => <ListRow itemId variant key={string_of_int(i)} />
              }
            )
            ->React.array}
          </div>
        } else {
          React.null
        }}
        <div className=Styles.listFooter>
          {
            let numItems = Js.Array.length(list.itemIds)
            if numItems > 8 {
              <div className=Styles.numItems>
                {React.string(string_of_int(numItems) ++ " items")}
              </div>
            } else {
              React.null
            }
          }
          {switch me {
          | Some(me) => <>
              <div>
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    %Repromise({
                      let response = API.cloneItemList(
                        ~sessionId=Belt.Option.getExn(UserStore.sessionId.contents),
                        ~listId,
                      )
                      UserStore.handleServerResponse("/item-lists/clone", response)
                      %Repromise.JsExn({
                        let json = Fetch.Response.json(Belt.Result.getExn(response))
                        let newListId = {
                          open Json.Decode
                          json->field("id", string)
                        }
                        ReasonReactRouter.push("/l/" ++ (newListId ++ "?clone"))
                        Analytics.Amplitude.logEventWithProperties(
                          ~eventName="Item List Cloned",
                          ~eventProperties={
                            "listId": listId,
                            "newListId": newListId,
                            "numItems": Js.Array.length(list.itemIds),
                          },
                        )
                        Promise.resolved()
                      })
                    })->ignore
                  }}
                  className=Styles.cloneListLink>
                  {React.string("Clone list")}
                </a>
              </div>
              {if list.userId == Some(me.id) {
                <div>
                  <a
                    href="#"
                    onClick={e => {
                      ReactEvent.Mouse.preventDefault(e)
                      ConfirmDialog.confirm(
                        ~bodyText="Are you sure you want to delete this list?",
                        ~confirmLabel="Delete list",
                        ~cancelLabel="Not now",
                        ~onConfirm=() =>
                          %Repromise({
                            let response = API.deleteItemList(
                              ~sessionId=Belt.Option.getExn(UserStore.sessionId.contents),
                              ~listId,
                            )
                            UserStore.handleServerResponse("/item-lists/delete", response)
                            ReasonReactRouter.push("/lists")
                            Promise.resolved()
                          })->ignore,
                        (),
                      )
                    }}
                    className=Styles.deleteListLink>
                    {React.string("Delete list")}
                  </a>
                </div>
              } else {
                React.null
              }}
            </>
          | None => React.null
          }}
        </div>
      </div>
    | None => React.null
    }}
  </div>
}
