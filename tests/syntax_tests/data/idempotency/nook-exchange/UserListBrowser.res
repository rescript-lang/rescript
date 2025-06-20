module Styles = {
  open Css
  let root = style(list{
    borderRadius(px(16)),
    margin2(~v=zero, ~h=auto),
    overflow(hidden),
    media("(min-width: 640px)", list{width(px(624))}),
    media("(min-width: 860px)", list{width(px(816))}),
    media("(min-width: 1040px)", list{width(px(1008))}),
    media("(min-width: 1240px)", list{width(px(1200))}),
    media("(min-width: 1440px)", list{width(px(1392))}),
    media("(max-width: 640px)", list{width(auto), borderRadius(zero)}),
  })
  let body = style(list{
    padding(px(32)),
    backgroundColor(hex("b0dec1c0")),
    position(relative),
    media("(max-width: 640px)", list{padding(px(16))}),
  })
  let listLinks = style(list{
    backgroundColor(hex("ffffff80")),
    media("(max-width: 640px)", list{overflowX(auto)}),
  })
  let listLinksContent = style(list{
    display(flexBox),
    alignItems(center),
    padding3(~top=px(18), ~bottom=px(16), ~h=px(32)),
    media(
      "(max-width: 640px)",
      list{paddingLeft(px(16)), paddingRight(px(16)), whiteSpace(nowrap)},
    ),
  })
  let profileLink = style(list{
    color(Colors.charcoal),
    display(inlineBlock),
    fontSize(px(20)),
    textDecoration(none),
    opacity(0.8),
    hover(list{opacity(1.)}),
    media("(max-width: 860px)", list{display(none)}),
  })
  let profileLinkIcon = style(list{
    flexShrink(0.),
    margin2(~v=zero, ~h=px(8)),
    top(zero),
    media("(max-width: 860px)", list{display(none)}),
  })
  let listLinkEmoji = style(list{
    display(inlineBlock),
    lineHeight(zero),
    marginRight(px(6)),
    position(relative),
    top(px(2)),
    media("(max-width: 460px)", list{display(none)}),
  })
  let listLinkName = "listLinkName"
  let listLinkSelectedName = style(list{borderBottom(px(2), solid, Colors.charcoal)})
  let listLink = style(list{
    color(Colors.charcoal),
    display(inlineBlock),
    fontSize(px(20)),
    textDecoration(none),
    marginRight(px(28)),
    media("(max-width: 640px)", list{fontSize(px(16))}),
    media("(max-width: 370px)", list{marginRight(px(16))}),
    lastChild(list{marginRight(zero)}),
    hover(list{selector("& ." ++ listLinkName, list{borderBottom(px(2), solid, Colors.charcoal)})}),
  })
  let rootMini = style(list{important(backgroundColor(hex("fffffff0")))})
  let catalogNotice = style(list{
    lineHeight(px(18)),
    marginBottom(px(24)),
    media("(max-width: 640px)", list{marginBottom(px(16))}),
  })
  let filterBar = style(list{marginBottom(zero)})
  let topPager = style(list{marginBottom(px(8)), media("(max-width: 640px)", list{display(none)})})
  let sectionToggles = style(list{
    display(flexBox),
    position(static),
    textAlign(center),
    marginTop(px(8)),
    media(
      "(min-width: 1040px)",
      list{position(absolute), right(px(32)), top(px(-40)), marginTop(zero)},
    ),
    media("(max-width: 470px)", list{marginBottom(px(16))}),
  })
  let bottomBar = style(list{display(flexBox), alignItems(center), justifyContent(spaceBetween)})
}

let getUrl = (~url: ReasonReactRouter.url, ~urlSearchParams: Webapi.Url.URLSearchParams.t) =>
  Utils.getPath(~url) ++
  switch Webapi.Url.URLSearchParams.toString(urlSearchParams) {
  | "" => ""
  | search => "?" ++ search
  }

let getNumResultsPerPage = (~viewportWidth) =>
  if viewportWidth >= 1440 {
    49
  } else if viewportWidth >= 1240 {
    42
  } else if viewportWidth >= 1040 {
    35
  } else if viewportWidth >= 860 {
    28
  } else if viewportWidth >= 640 {
    21
  } else {
    20
  }

let userItemsHasOneWithStatus = (~userItems, ~status) =>
  userItems->Js.Dict.values->Belt.Array.someU((. item: User.item) => item.status == status)

module BulkActions = {
  module Styles = {
    open Css
    let bulkActionsLink = style(list{textDecoration(none), hover(list{textDecoration(underline)})})
    let note = style(list{color(Colors.gray), marginBottom(px(8))})
    let bulkActionsButtons = style(list{
      display(flexBox),
      flexDirection(column),
      padding(px(16)),
      backgroundColor(Colors.white),
      borderRadius(px(8)),
      Colors.darkLayerShadow,
      selector(
        "& > a",
        list{
          display(inlineBlock),
          marginBottom(px(4)),
          textDecoration(none),
          lastChild(list{marginBottom(zero)}),
          hover(list{textDecoration(underline)}),
        },
      ),
    })
  }

  @react.component
  let make = (~filteredItems, ~list: ViewingList.t) => {
    let (showPopup, setShowPopup) = React.useState(() => false)
    let reference = React.useRef(Js.Nullable.null)

    <>
      <a
        href="#"
        onClick={e => {
          ReactEvent.Mouse.preventDefault(e)
          setShowPopup(show => !show)
        }}
        className=Styles.bulkActionsLink
        ref={ReactDOMRe.Ref.domRef(reference)}>
        {React.string("Bulk Actions")}
      </a>
      {showPopup
        ? <ReactAtmosphere.PopperLayer
            reference
            onOutsideClick={() => setShowPopup(_ => false)}
            options={
              placement: Some("top-start"),
              modifiers: Some([
                {
                  "name": "offset",
                  "options": {
                    "offset": [-16, 8],
                  },
                },
              ]),
            }
            render={_ => {
              let numItems = Js.Array.length(filteredItems)
              let itemsString = string_of_int(numItems) ++ (" item" ++ (numItems !== 1 ? "s" : ""))
              <div className=Styles.bulkActionsButtons>
                <div className=Styles.note>
                  {React.string("These actions operate on this filtered list.")}
                </div>
                {switch list {
                | Wishlist
                | ForTrade =>
                  <a
                    href="#"
                    onClick={e => {
                      ReactEvent.Mouse.preventDefault(e)
                      ConfirmDialog.confirm(
                        ~bodyText="Are you sure you want to move these " ++
                        (itemsString ++
                        " to your Catalog?"),
                        ~confirmLabel="Move items",
                        ~cancelLabel="Never mind",
                        ~onConfirm=() =>
                          UserStore.setItemStatusBatch(
                            ~items=filteredItems->Belt.Array.map(((item, _)) => item),
                            ~status=CatalogOnly,
                          ),
                        (),
                      )
                      setShowPopup(_ => false)
                    }}>
                    {React.string("Move " ++ (itemsString ++ " to Catalog"))}
                  </a>
                | _ => React.null
                }}
                {switch list {
                | Catalog
                | Wishlist =>
                  <a
                    href="#"
                    onClick={e => {
                      ReactEvent.Mouse.preventDefault(e)
                      ConfirmDialog.confirm(
                        ~bodyText="Are you sure you want to move these " ++
                        (itemsString ++
                        " to your For Trade?"),
                        ~confirmLabel="Move items",
                        ~cancelLabel="Never mind",
                        ~onConfirm=() =>
                          UserStore.setItemStatusBatch(
                            ~items=filteredItems->Belt.Array.map(((item, _)) => item),
                            ~status=ForTrade,
                          ),
                        (),
                      )
                      setShowPopup(_ => false)
                    }}>
                    {React.string("Move " ++ (itemsString ++ " to For Trade"))}
                  </a>
                | _ => React.null
                }}
                <a
                  href="#"
                  onClick={e => {
                    ReactEvent.Mouse.preventDefault(e)
                    ConfirmDialog.confirm(
                      ~bodyText="Are you sure you want to remove these " ++ (itemsString ++ "?"),
                      ~confirmLabel="Remove items",
                      ~cancelLabel="Never mind",
                      ~onConfirm=() =>
                        UserStore.removeItems(
                          ~items=filteredItems->Belt.Array.map(((item, _)) => item),
                        ),
                      (),
                    )
                    setShowPopup(_ => false)
                  }}>
                  {React.string("Remove " ++ itemsString)}
                </a>
              </div>
            }}
          />
        : React.null}
    </>
  }
}

@react.component
let make = (~user: User.t, ~list: ViewingList.t, ~url: ReasonReactRouter.url, ~me=false, ()) => {
  let viewportWidth = Utils.useViewportWidth()
  let numResultsPerPage = getNumResultsPerPage(~viewportWidth)
  let hasForTrade = React.useMemo1(
    () => userItemsHasOneWithStatus(~userItems=user.items, ~status=ForTrade),
    [user],
  )
  let hasCanCraft = React.useMemo1(
    () => userItemsHasOneWithStatus(~userItems=user.items, ~status=CanCraft),
    [user],
  )
  let hasWishlist = React.useMemo1(
    () => userItemsHasOneWithStatus(~userItems=user.items, ~status=Wishlist),
    [user],
  )
  let hasCatalogOnly = React.useMemo1(
    () => userItemsHasOneWithStatus(~userItems=user.items, ~status=CatalogOnly),
    [user],
  )
  let userItems = React.useMemo2(
    () =>
      user.items
      ->Js.Dict.entries
      ->Belt.Array.keepMapU((. (itemKey, item: User.item)) =>
        ViewingList.doesUserItemStatusMatchViewingList(item.status, list)
          ? User.fromItemKey(~key=itemKey)->Belt.Option.map(x => (x, item))
          : None
      ),
    (user, list),
  )
  let userItemIds = userItems->Belt.Array.mapU((. ((itemId, _variant), _)) => itemId)

  let url = ReasonReactRouter.useUrl()
  let showMini = {
    open Webapi.Url.URLSearchParams
    make(url.search)->has("thumbnails")
  }
  let (showRecipes, setShowRecipes) = React.useState(() => false)
  let (filters, pageOffset) = React.useMemo1(
    () => ItemFilters.fromUrlSearch(~urlSearch=url.search, ~defaultSort=UserDefault),
    [url.search],
  )
  let numFiltersChangeLogged = React.useRef(0)
  let setFilters = (filters: ItemFilters.t) => {
    let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(
      ItemFilters.serialize(~filters, ~defaultSort=UserDefault, ~pageOffset=0),
    )
    if showMini {
      urlSearchParams->Webapi.Url.URLSearchParams.append("thumbnails", "")
    }
    let url = ReasonReactRouter.dangerouslyGetInitialUrl()
    let newUrl = getUrl(~url, ~urlSearchParams)
    ReasonReactRouter.push(newUrl)
    if React.Ref.current(numFiltersChangeLogged) < 2 {
      Analytics.Amplitude.logEventWithProperties(
        ~eventName="Filters Changed",
        ~eventProperties={
          "filterText": filters.text,
          "filterMask": filters.mask,
          "filterCategory": filters.category,
          "filterSort": filters.sort,
        },
      )
      React.Ref.setCurrent(numFiltersChangeLogged, React.Ref.current(numFiltersChangeLogged) + 1)
    }
  }
  let setPageOffset = f => {
    let nextPageOffset = f(pageOffset)
    let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(
      ItemFilters.serialize(~filters, ~defaultSort=UserDefault, ~pageOffset=nextPageOffset),
    )
    ReasonReactRouter.push(getUrl(~url, ~urlSearchParams))
  }
  let filteredItems = React.useMemo2(() => {
    let sortFn = ItemFilters.getUserItemSort(
      ~prioritizeViewerStatuses=?!me
        ? Some(
            switch list {
            | Wishlist => [User.ForTrade, User.CanCraft, User.CatalogOnly]
            | ForTrade
            | CanCraft
            | Catalog => [User.Wishlist]
            },
          )
        : None,
      ~sort=filters.sort,
    )
    userItems->Belt.Array.keep((((itemId, _), _)) =>
      ItemFilters.doesItemMatchFilters(~item=Item.getItem(~itemId), ~filters)
    )->Js.Array.sortInPlaceWith((aUserItem, bUserItem) => sortFn(aUserItem, bUserItem))
  }, (userItems, filters))
  let numResults = filteredItems->Belt.Array.length

  let renderListLink = (linkList: ViewingList.t) =>
    if (
      switch linkList {
      | Wishlist => hasWishlist
      | CanCraft => hasCanCraft
      | ForTrade => hasForTrade
      | Catalog => hasCatalogOnly
      }
    ) {
      <Link
        path={"/u/" ++ (user.username ++ ("/" ++ ViewingList.viewingListToUrl(linkList)))}
        className=Styles.listLink>
        <span className=Styles.listLinkEmoji>
          {React.string(ViewingList.viewingListToEmoji(linkList))}
        </span>
        <span
          className={Cn.make(list{
            Styles.listLinkName,
            Cn.ifTrue(Styles.listLinkSelectedName, linkList == list),
          })}>
          {React.string(ViewingList.viewingListToString(linkList))}
        </span>
      </Link>
    } else {
      React.null
    }

  let rootRef = React.useRef(Js.Nullable.null)
  React.useEffect0(() => {
    if TemporaryState.state.contents == Some(FromProfileBrowser) {
      TemporaryState.state := None
      let rootElement = Utils.getElementForDomRef(rootRef)
      open Webapi.Dom
      let boundingRect = Element.getBoundingClientRect(rootElement)
      window->Window.scrollBy(0., DomRect.top(boundingRect) -. 32.)
    } else {
      open Webapi.Dom
      window->Window.scrollTo(0., 0.)
    }
    None
  })

  <div className=Styles.root ref={ReactDOMRe.Ref.domRef(rootRef)}>
    <div className=Styles.listLinks>
      <div className=Styles.listLinksContent>
        <Link path={"/u/" ++ user.username} className=Styles.profileLink>
          {React.string(user.username)}
        </Link>
        <span
          className={Cn.make(list{
            UserProfileBrowser.Styles.sectionTitleLinkIcon,
            Styles.profileLinkIcon,
          })}
        />
        {renderListLink(ForTrade)}
        {renderListLink(CanCraft)}
        {renderListLink(Wishlist)}
        {renderListLink(Catalog)}
      </div>
    </div>
    <div className={Cn.make(list{Styles.body, Cn.ifTrue(Styles.rootMini, showMini)})}>
      {me && list == Catalog
        ? <div className=Styles.catalogNotice>
            {React.string(j`This list includes your For Trade and Can Craft items. You can also add catalog-only items.`)}
          </div>
        : React.null}
      {Js.Array.length(userItems) > 8
        ? <div className={Cn.make(list{ItemBrowser.Styles.filterBar, Styles.filterBar})}>
            <ItemFilters
              userItemIds filters isViewingSelf=me onChange={filters => setFilters(filters)}
            />
            {!showMini
              ? <ItemFilters.Pager
                  numResults pageOffset numResultsPerPage setPageOffset className=Styles.topPager
                />
              : React.null}
          </div>
        : React.null}
      <div className=Styles.sectionToggles>
        <div className=UserProfileBrowser.Styles.showRecipesBox>
          <label htmlFor="show-mini" className=UserProfileBrowser.Styles.showRecipesLabel>
            {React.string("Thumbnails")}
          </label>
          <input
            id="show-mini"
            type_="checkbox"
            checked=showMini
            onChange={e => {
              let checked = ReactEvent.Form.target(e)["checked"]
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Miniature Mode Clicked",
                ~eventProperties={"checked": checked, "status": list},
              )
              let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(
                ItemFilters.serialize(~filters, ~defaultSort=UserDefault, ~pageOffset=0),
              )
              if checked {
                urlSearchParams->Webapi.Url.URLSearchParams.append("thumbnails", "")
              }
              let url = ReasonReactRouter.dangerouslyGetInitialUrl()
              let newUrl = getUrl(~url, ~urlSearchParams)
              ReasonReactRouter.replace(newUrl)
            }}
            className=UserProfileBrowser.Styles.showRecipesCheckbox
          />
        </div>
        <div className=UserProfileBrowser.Styles.showRecipesBox>
          <label
            htmlFor="craftShowRecipe"
            className={Cn.make(list{
              UserProfileBrowser.Styles.showRecipesLabel,
              Cn.ifTrue(UserProfileBrowser.Styles.showRecipesLabelDisabled, showMini),
            })}>
            {React.string("Show Recipes")}
          </label>
          <input
            id="craftShowRecipe"
            type_="checkbox"
            checked={showRecipes && !showMini}
            onChange={e => {
              let checked = ReactEvent.Form.target(e)["checked"]
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Show Recipes Clicked",
                ~eventProperties={"checked": checked},
              )
              setShowRecipes(_ => checked)
            }}
            disabled=showMini
            className=UserProfileBrowser.Styles.showRecipesCheckbox
          />
        </div>
      </div>
      <div
        className={Cn.make(list{
          ItemBrowser.Styles.cards,
          UserProfileBrowser.Styles.cards,
          Cn.ifTrue(UserProfileBrowser.Styles.cardsMini, showMini),
        })}>
        {filteredItems
        ->(showMini
          ? x => x
          : Belt.Array.slice(~offset=pageOffset * numResultsPerPage, ~len=numResultsPerPage))
        ->Belt.Array.mapU((. ((itemId, variation), userItem)) =>
          showMini
            ? <UserProfileBrowser.UserItemCardMini
                itemId variation key={string_of_int(itemId) ++ string_of_int(variation)}
              />
            : <UserItemCard
                itemId
                variation
                userItem
                list
                editable=me
                showRecipe=showRecipes
                key={string_of_int(itemId) ++ string_of_int(variation)}
              />
        )
        ->React.array}
      </div>
      {!showMini
        ? <div className=Styles.bottomBar>
            {me && Js.Array.length(filteredItems) >= 3
              ? <BulkActions filteredItems list />
              : React.null}
            {Js.Array.length(filteredItems) > numResultsPerPage
              ? <div className=ItemBrowser.Styles.bottomFilterBar>
                  <ItemFilters.Pager
                    numResults
                    pageOffset
                    numResultsPerPage
                    setPageOffset={f => {
                      setPageOffset(f)
                      let rootElement = Utils.getElementForDomRef(rootRef)
                      open Webapi.Dom
                      let boundingRect = Element.getBoundingClientRect(rootElement)
                      window->Window.scrollBy(0., DomRect.top(boundingRect))
                    }}
                  />
                </div>
              : React.null}
          </div>
        : React.null}
    </div>
  </div>
}
