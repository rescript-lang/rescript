module Styles = {
  open Css
  let root = style(list{
    padding2(~v=zero, ~h=px(16)),
    margin3(~top=zero, ~bottom=px(48), ~h=auto),
    media("(min-width: 660px)", list{width(px(560))}),
    media("(min-width: 860px)", list{width(px(752))}),
    media("(min-width: 1040px)", list{width(px(944))}),
    media("(min-width: 1240px)", list{width(px(1136))}),
    media("(min-width: 1440px)", list{width(px(1328))}),
    padding(px(32)),
    borderRadius(px(16)),
    backgroundColor(hex("b0dec1c0")),
    position(relative),
    media("(max-width: 640px)", list{width(auto), padding(px(16)), borderRadius(zero)}),
  })
  let rootMini = style(list{backgroundColor(hex("fffffff0"))})
  let sectionTitle = style(list{marginBottom(px(16))})
  @module("./assets/link.png") external linkIcon: string = "default"
  let sectionTitleLinkIcon = style(list{
    backgroundImage(url(linkIcon)),
    display(inlineBlock),
    backgroundSize(cover),
    width(px(24)),
    height(px(24)),
    opacity(0.5),
    position(relative),
    top(px(4)),
    left(px(-2)),
    transition(~duration=200, "all"),
  })
  let sectionTitleLink = style(list{
    color(Colors.charcoal),
    fontSize(px(24)),
    textDecoration(none),
    hover(list{
      selector("& ." ++ sectionTitleLinkIcon, list{opacity(1.), transform(translateX(px(4)))}),
    }),
  })
  let filterBar = style(list{marginTop(px(32)), marginBottom(zero)})
  let cards = style(list{
    paddingTop(px(16)),
    marginRight(px(-16)),
    media("(max-width: 470px)", list{paddingTop(zero)}),
  })
  let cardsMini = style(list{
    justifyContent(flexStart),
    marginLeft(px(-8)),
    marginRight(px(-8)),
    media("(max-width: 640px)", list{justifyContent(center)}),
  })
  let cardSeeAllLinkIcon = style(list{opacity(0.8), top(px(-1))})
  let cardSeeAll = style(list{
    flexDirection(row),
    justifyContent(center),
    fontSize(px(20)),
    color(Colors.charcoal),
    paddingTop(px(36)),
    paddingBottom(px(36)),
    textDecoration(none),
    hover(list{
      Colors.darkLayerShadow,
      selector("& ." ++ cardSeeAllLinkIcon, list{transform(translateX(px(2)))}),
    }),
  })
  let cardMini = style(list{position(relative), cursor(pointer)})
  let cardMiniHasQuicklist = style(list{
    cursor(pointer),
    transition(~duration=200, "all"),
    media("(hover: hover)", list{opacity(0.5), hover(list{opacity(0.8)})}),
  })
  let cardMiniQuicklistSelected = style(list{
    important(opacity(1.)),
    backgroundColor(hex("3aa56340")),
  })
  let cardMiniImage = style(list{display(block), width(px(64)), height(px(64))})
  let cardMiniRecipe = style(list{
    display(block),
    width(px(32)),
    height(px(32)),
    position(absolute),
    bottom(px(-4)),
    right(px(-4)),
  })
  let sectionToggles = style(list{
    position(absolute),
    right(px(32)),
    top(px(48)),
    display(flexBox),
    flexDirection(column),
    alignItems(flexEnd),
    transform(translateY(pct(-50.))),
    media("(max-width: 640px)", list{top(px(32)), right(px(16))}),
  })
  let showRecipesBox = style(list{
    marginLeft(px(16)),
    display(flexBox),
    alignItems(center),
    firstChild(list{marginLeft(zero)}),
  })
  let showRecipesLabel = style(list{fontSize(px(16)), marginRight(px(8))})
  let showRecipesLabelDisabled = style(list{opacity(0.5)})
  let showRecipesCheckbox = style(list{
    fontSize(px(24)),
    margin(zero),
    position(relative),
    top(px(-2)),
  })
  let recipe = style(list{marginTop(px(6)), textAlign(center), fontSize(px(12))})
}

open Belt

module UserItemCardMini = {
  @react.component
  let make = (~itemId: int, ~variation) => {
    let hasQuicklist = QuicklistStore.useHasQuicklist()
    let isInQuicklist = QuicklistStore.useItemState(~itemId, ~variant=variation)

    let item = Item.getItem(~itemId)
    <div
      className={Cn.make(list{
        Styles.cardMini,
        Cn.ifTrue(Styles.cardMiniHasQuicklist, hasQuicklist),
        Cn.ifTrue(Styles.cardMiniQuicklistSelected, isInQuicklist),
      })}>
      {
        let image =
          <img
            onClick={_ =>
              if hasQuicklist {
                if isInQuicklist {
                  QuicklistStore.removeItem(~itemId, ~variant=variation)
                } else {
                  QuicklistStore.addItem(~itemId, ~variant=variation)
                }
              } else {
                ReasonReactRouter.push(
                  Utils.getItemDetailUrl(~itemId=item.id, ~variant=Some(variation)),
                )
              }}
            src={Item.getImageUrl(~item, ~variant=variation)}
            className=Styles.cardMiniImage
          />
        if Utils.browserSupportsHover {
          <ReactAtmosphere.Tooltip text={React.string(Item.getName(item))}>
            {({onMouseEnter, onMouseLeave, onFocus, onBlur, ref}) =>
              <div onMouseEnter onMouseLeave onFocus onBlur ref={ReactDOMRe.Ref.domRef(ref)}>
                image
              </div>}
          </ReactAtmosphere.Tooltip>
        } else {
          image
        }
      }
      {Item.isRecipe(~item)
        ? <img src={Constants.cdnUrl ++ "/images/DIYRecipe.png"} className=Styles.cardMiniRecipe />
        : React.null}
    </div>
  }
}

module Section = {
  let randomString = () => Js.Math.random()->Js.Float.toString

  let getMaxResults = (~viewportWidth) =>
    if viewportWidth >= 1440 {
      14
    } else if viewportWidth >= 1240 {
      12
    } else if viewportWidth >= 1040 {
      10
    } else if viewportWidth >= 860 {
      8
    } else if viewportWidth >= 640 {
      6
    } else {
      6
    }

  @react.component
  let make = (
    ~username: string,
    ~list: ViewingList.t,
    ~userItems: array<((int, int), User.item)>,
    ~editable,
  ) => {
    let id = React.useMemo0(() => randomString())
    let viewportWidth = Utils.useViewportWidth()
    let maxResults = getMaxResults(~viewportWidth)
    let (showRecipes, setShowRecipes) = React.useState(() => false)
    let url = ReasonReactRouter.useUrl()
    let showMini = {
      open Webapi.Url.URLSearchParams
      make(url.search)->has("thumbnails")
    }
    let url = ReasonReactRouter.useUrl()
    let sort = React.useMemo1(() => {
      let (filters, _) = ItemFilters.fromUrlSearch(~urlSearch=url.search, ~defaultSort=UserDefault)
      filters.sort
    }, [url.search])
    let filteredItems = React.useMemo1(() => {
      let sortFn = ItemFilters.getUserItemSort(
        ~prioritizeViewerStatuses=?!editable
          ? Some(
              switch list {
              | Wishlist => [User.ForTrade, User.CanCraft, User.CatalogOnly]
              | ForTrade
              | Catalog
              | CanCraft => [User.Wishlist]
              },
            )
          : None,
        ~sort,
      )
      userItems->Js.Array.sortInPlaceWith((aUserItem, bUserItem) => sortFn(aUserItem, bUserItem))
    }, [userItems])
    let viewingListUrl =
      "/u/" ++
      (username ++
      ("/" ++
      (ViewingList.viewingListToUrl(list) ++
      switch url.search {
      | "" => ""
      | search => "?" ++ search
      })))
    let numResults = userItems->Js.Array.length

    <div className={Cn.make(list{Styles.root, Cn.ifTrue(Styles.rootMini, showMini)})}>
      <div className=Styles.sectionTitle>
        <Link path=viewingListUrl className=Styles.sectionTitleLink>
          {React.string(ViewingList.viewingListToEmoji(list))}
          {React.string(" " ++ ViewingList.viewingListToString(list))}
          <span className=Styles.sectionTitleLinkIcon />
        </Link>
      </div>
      <div className=Styles.sectionToggles>
        <div className=Styles.showRecipesBox>
          <label htmlFor=id className=Styles.showRecipesLabel> {React.string("Thumbnails")} </label>
          <input
            id
            type_="checkbox"
            checked=showMini
            onChange={e => {
              let checked = ReactEvent.Form.target(e)["checked"]
              Analytics.Amplitude.logEventWithProperties(
                ~eventName="Miniature Mode Clicked",
                ~eventProperties={"checked": checked, "list": list},
              )
              let url = ReasonReactRouter.dangerouslyGetInitialUrl()
              ReasonReactRouter.replace(Utils.getPath(~url) ++ (checked ? "?thumbnails" : ""))
            }}
            className=Styles.showRecipesCheckbox
          />
        </div>
        {if list == CanCraft {
          <div className=Styles.showRecipesBox>
            <label
              htmlFor="craftShowRecipe"
              className={Cn.make(list{
                Styles.showRecipesLabel,
                Cn.ifTrue(Styles.showRecipesLabelDisabled, showMini),
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
              className=Styles.showRecipesCheckbox
            />
          </div>
        } else {
          React.null
        }}
      </div>
      <div
        className={Cn.make(list{
          ItemBrowser.Styles.cards,
          Styles.cards,
          Cn.ifTrue(Styles.cardsMini, showMini),
        })}>
        {filteredItems
        ->(showMini
          ? x => x
          : Belt.Array.slice(~offset=0, ~len=numResults > maxResults ? maxResults - 1 : maxResults))
        ->Belt.Array.mapU((. ((itemId, variation), userItem)) =>
          showMini
            ? <UserItemCardMini
                itemId variation key={string_of_int(itemId) ++ string_of_int(variation)}
              />
            : <UserItemCard
                itemId
                variation
                userItem
                editable
                list
                showRecipe=showRecipes
                key={string_of_int(itemId) ++ string_of_int(variation)}
              />
        )
        ->(!showMini && numResults > maxResults
          ? Belt.Array.concat(
              _,
              [
                <Link
                  path=viewingListUrl
                  className={Cn.make(list{UserItemCard.Styles.card, Styles.cardSeeAll})}
                  key="link">
                  {React.string("See all " ++ string_of_int(Js.Array.length(userItems)))}
                  <span
                    className={Cn.make(list{
                      Styles.sectionTitleLinkIcon,
                      Styles.cardSeeAllLinkIcon,
                    })}
                  />
                </Link>,
              ],
            )
          : x => x)
        ->React.array}
      </div>
    </div>
  }
}

@react.component
let make = (~username, ~userItems: array<((int, int), User.item)>, ~editable) => {
  let wishlist = userItems->Array.keepU((. (_, item: User.item)) => item.status == Wishlist)
  let forTradeList = userItems->Array.keepU((. (_, item: User.item)) => item.status == ForTrade)
  let canCraftList = userItems->Array.keepU((. (_, item: User.item)) => item.status == CanCraft)
  let hasForTrade = forTradeList->Array.length > 0
  let hasCanCraft = canCraftList->Array.length > 0
  let hasWishlist = wishlist->Array.length > 0
  let hasCatalogOnly =
    userItems->Array.getBy(((_, item: User.item)) => item.status == CatalogOnly) != None

  React.useEffect0(() => Some(() => TemporaryState.state := Some(FromProfileBrowser)))

  <div>
    {if hasForTrade {
      <Section username list=ForTrade userItems=forTradeList editable />
    } else {
      React.null
    }}
    {if hasCanCraft {
      <Section username list=CanCraft userItems=canCraftList editable />
    } else {
      React.null
    }}
    {if hasWishlist {
      <Section username list=Wishlist userItems=wishlist editable />
    } else {
      React.null
    }}
    {if hasCatalogOnly {
      <Section
        username
        list=Catalog
        userItems={userItems->Array.keepU((. (_, item: User.item)) =>
          switch item.status {
          | CanCraft
          | ForTrade
          | CatalogOnly => true
          | Wishlist => false
          }
        )}
        editable
      />
    } else {
      React.null
    }}
  </div>
}
