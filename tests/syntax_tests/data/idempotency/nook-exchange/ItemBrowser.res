module Styles = {
  open Css
  let root = style(list{
    margin2(~v=zero, ~h=auto),
    padding2(~v=zero, ~h=px(16)),
    media("(min-width: 600px)", list{width(px(544))}),
    media("(min-width: 940px)", list{width(px(832))}),
    media("(min-width: 1200px)", list{width(px(1120))}),
    media("(min-width: 1520px)", list{width(px(1408))}),
  })
  let cards = style(list{
    display(flexBox),
    flexWrap(wrap),
    marginRight(px(-32)),
    justifyContent(flexStart),
    media("(max-width: 600px)", list{marginRight(px(-16))}),
  })
  let filterBarTop = style(list{
    display(flexBox),
    alignItems(center),
    media(
      "(max-width: 600px)",
      list{
        marginLeft(px(-16)),
        marginRight(px(-16)),
        paddingLeft(px(16)),
        overflowX(auto),
        selector(
          "& ." ++ ItemFilters.CategoryButtons.CategoryStyles.button,
          list{marginBottom(zero)},
        ),
        selector(
          "& ." ++ ItemFilters.CategoryButtons.CategoryStyles.select,
          list{marginBottom(zero)},
        ),
      },
    ),
  })
  let filterBar = style(list{
    display(flexBox),
    justifyContent(spaceBetween),
    marginBottom(px(16)),
    flexWrap(wrap),
    alignItems(center),
  })
  let filters = style(list{media("(max-width: 939px)", list{width(pct(100.))})})
  let topPager = style(list{marginBottom(px(8))})
  let bottomFilterBar = style(list{display(flexBox), justifyContent(flexEnd)})
  let noResults = style(list{fontSize(px(20)), paddingTop(px(32))})
}

let getNumResultsPerPage = () => {
  open Webapi.Dom
  let windowWidth = window->Window.innerWidth
  if windowWidth >= 1520 {
    50
  } else if windowWidth >= 1200 {
    40
  } else {
    24
  }
}

let getUrl = (~url: ReasonReactRouter.url, ~urlSearchParams: Webapi.Url.URLSearchParams.t) =>
  "/" ++
  (Js.Array.joinWith("/", Belt.List.toArray(url.path)) ++
  switch Webapi.Url.URLSearchParams.toString(urlSearchParams) {
  | "" => ""
  | search => "?" ++ search
  })

@react.component
let make = (~showLogin, ~url: ReasonReactRouter.url) => {
  let isLoggedIn = UserStore.useIsLoggedIn()
  let viewportWidth = Utils.useViewportWidth()
  let (numResultsPerPage, _setNumResultsPerPage) = React.useState(() => getNumResultsPerPage())
  let (filters, pageOffset) = React.useMemo1(
    () => ItemFilters.fromUrlSearch(~urlSearch=url.search, ~defaultSort=SellPriceDesc),
    [url.search],
  )
  let numFiltersChangeLogged = React.useRef(0)
  let setFilters = (filters: ItemFilters.t) => {
    let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(
      ItemFilters.serialize(~filters, ~defaultSort=SellPriceDesc, ~pageOffset=0),
    )
    ReasonReactRouter.push(getUrl(~url, ~urlSearchParams))
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
  let rootRef = React.useRef(Js.Nullable.null)
  let setPageOffset = f => {
    let nextPageOffset = f(pageOffset)
    let urlSearchParams = Webapi.Url.URLSearchParams.makeWithArray(
      ItemFilters.serialize(~filters, ~defaultSort=SellPriceDesc, ~pageOffset=nextPageOffset),
    )
    ReasonReactRouter.push(getUrl(~url, ~urlSearchParams))
  }
  let excludeString = filters.exclude->Js.Array.joinWith(",")
  let excludeUserItemIds = React.useMemo2(() =>
    if isLoggedIn && Js.Array.length(filters.exclude) > 0 {
      let userItems = UserStore.getUser().items
      let userItemMap = Js.Dict.empty()
      userItems
      ->Js.Dict.entries
      ->Belt.Array.forEach(((itemKey, userItem)) =>
        if (
          switch userItem.status {
          | Wishlist => filters.exclude->Js.Array.includes(ItemFilters.Wishlist)
          | CanCraft =>
            filters.exclude->Js.Array.includes(ItemFilters.Catalog) ||
              filters.exclude->Js.Array.includes(ItemFilters.CanCraft)
          | CatalogOnly
          | ForTrade =>
            filters.exclude->Js.Array.includes(ItemFilters.Catalog)
          }
        ) {
          let (itemId, variant) = User.fromItemKey(~key=itemKey)->Belt.Option.getExn
          let itemVariantList = switch Js.Dict.get(userItemMap, string_of_int(itemId)) {
          | Some(list) => list
          | None =>
            let list = []
            userItemMap->Js.Dict.set(string_of_int(itemId), list)
            list
          }
          itemVariantList->Js.Array.push(variant)->ignore
        }
      )
      userItemMap
      ->Js.Dict.entries
      ->Belt.Array.keepMap(((itemId, variantList)) => {
        let itemId = int_of_string(itemId)
        let numVariations = Item.getCollapsedVariants(~item=Item.getItem(~itemId))
        // Exclude item only if user has all variants
        if Js.Array.length(variantList) >= Js.Array.length(numVariations) {
          Some(itemId)
        } else {
          None
        }
      })
    } else {
      []
    }
  , (excludeString, isLoggedIn))
  let filteredItems = React.useMemo2(
    () =>
      Item.all->Belt.Array.keep(item =>
        !(
          Js.Array.includes(item.id, excludeUserItemIds) ||
          (Item.isRecipe(~item) &&
          Js.Array.includes(Item.getItemIdForRecipe(~recipe=item), excludeUserItemIds))
        ) &&
        ItemFilters.doesItemMatchFilters(~item, ~filters)
      )->Js.Array.sortInPlaceWith(ItemFilters.getSort(~sort=filters.sort)),
    (filters, excludeUserItemIds),
  )
  let numResults = filteredItems->Belt.Array.length

  let isInitialLoadRef = React.useRef(true)
  React.useEffect1(() => {
    open Webapi.Dom
    if React.Ref.current(isInitialLoadRef) {
      window->Window.scrollTo(0., 0.)
      React.Ref.setCurrent(isInitialLoadRef, false)
    } else {
      let rootElement = Utils.getElementForDomRef(rootRef)
      let boundingRect = Element.getBoundingClientRect(rootElement)
      window->Window.scrollBy(
        0.,
        DomRect.top(boundingRect) -. float_of_int(Constants.headerHeight),
      )
    }
    None
  }, [url.search])

  <div className=Styles.root ref={ReactDOMRe.Ref.domRef(rootRef)}>
    <div className=Styles.filterBarTop>
      <ItemFilters.CategoryButtons filters onChange={filters => setFilters(filters)} />
      {isLoggedIn && viewportWidth >= 940
        ? <ItemFilters.AdvancedFilter filters onChange=setFilters />
        : React.null}
      <div className=ItemFilters.Styles.rootScrollerSpacer />
    </div>
    <div className=Styles.filterBar>
      <ItemFilters filters onChange={filters => setFilters(filters)} className=Styles.filters />
      {isLoggedIn && viewportWidth < 940
        ? <ItemFilters.AdvancedFilter filters onChange=setFilters />
        : React.null}
      <ItemFilters.Pager
        numResults pageOffset numResultsPerPage setPageOffset className=Styles.topPager
      />
    </div>
    <div className=Styles.cards>
      {filteredItems
      ->Belt.Array.slice(~offset=pageOffset * numResultsPerPage, ~len=numResultsPerPage)
      ->Belt.Array.mapWithIndexU((. i, item) =>
        <ItemCard item isLoggedIn showLogin key={string_of_int(item.id) ++ string_of_int(i)} />
      )
      ->React.array}
    </div>
    <div className=Styles.bottomFilterBar>
      <ItemFilters.Pager
        numResults pageOffset numResultsPerPage setPageOffset={f => setPageOffset(f)}
      />
    </div>
    {if Js.Array.length(filteredItems) == 0 {
      <div className=Styles.noResults>
        {React.string("There are no results. Try changing or ")}
        <a
          href="#"
          onClick={e => {
            setFilters(
              (
                {
                  text: "",
                  mask: None,
                  category: None,
                  exclude: [],
                  sort: SellPriceDesc,
                }: ItemFilters.t
              ),
            )
            ReactEvent.Mouse.preventDefault(e)
          }}>
          {React.string("clearing the filters!")}
        </a>
      </div>
    } else {
      React.null
    }}
  </div>
}
