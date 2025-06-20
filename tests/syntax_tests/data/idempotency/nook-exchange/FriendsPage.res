let cardWidth = 176

type followee = {
  id: string,
  username: string,
  lastUpdate: option<float>,
  items: array<(int, int, User.item)>,
}

let compareOptionTimestamps = (aTime, bTime) =>
  switch (aTime, bTime) {
  | (None, None) => 0
  | (Some(_), None) => -1
  | (None, Some(_)) => 1
  | (Some(aTime), Some(bTime)) => int_of_float(bTime -. aTime)
  }

let fetchFeed = () =>
  %Repromise.JsExn({
    let response = Fetch.fetchWithInit(
      Constants.apiUrl ++ "/@me/follow-feed",
      Fetch.RequestInit.make(
        ~method_=Get,
        ~headers=Fetch.HeadersInit.make({
          "X-Client-Version": Constants.gitCommitRef,
          "Authorization": "Bearer " ++ Belt.Option.getExn(UserStore.sessionId.contents),
        }),
        ~credentials=Include,
        ~mode=CORS,
        (),
      ),
    )
    %Repromise.JsExn({
      let json = Fetch.Response.json(response)
      open Json.Decode
      let feed =
        json
        ->array(json => {
          let items =
            (json->field("items", dict(User.itemFromJson)))
            ->Js.Dict.entries
            ->Belt.Array.keepMap(((itemKey, item)) =>
              item->Belt.Option.flatMap(item =>
                User.fromItemKey(~key=itemKey)->Belt.Option.map(((itemId, variant)) => (
                  itemId,
                  variant,
                  item,
                ))
              )
            )
              ->Js.Array.sortInPlaceWith(((_, _, aItem: User.item), (_, _, bItem: User.item)) =>
                compareOptionTimestamps(aItem.timeUpdated, bItem.timeUpdated)
              )
          {
            id: json->field("id", string),
            username: json->field("username", string),
            lastUpdate: Belt.Array.get(items, 0)->Belt.Option.flatMap(((_, _, item)) =>
              item.timeUpdated
            ),
            items: items,
          }
        })
        ->Js.Array.sortInPlaceWith((a, b) => compareOptionTimestamps(a.lastUpdate, b.lastUpdate))
      Promise.resolved(feed)
    })
  })

module Followee = {
  let getRootWidth = numCards => numCards * cardWidth + (numCards - 1) * 16
  module Styles = {
    open Css
    let root = style(list{
      width(px(getRootWidth(5))),
      backgroundColor(hex("b0dec1c0")),
      borderRadius(px(16)),
      padding3(~top=px(32), ~bottom=px(16), ~h=px(32)),
      margin3(~top=zero, ~h=auto, ~bottom=px(48)),
      width(auto),
      media("(min-width: 660px)", list{width(px(getRootWidth(3)))}),
      media("(min-width: 860px)", list{width(px(getRootWidth(4)))}),
      media("(min-width: 1040px)", list{width(px(getRootWidth(5)))}),
      media("(max-width: 640px)", list{width(auto), padding(px(16)), borderRadius(zero)}),
    })
    let usernameRow = style(list{
      display(flexBox),
      alignItems(center),
      justifyContent(spaceBetween),
      marginBottom(px(24)),
    })
    let usernameRowUnfollowed = style(list{
      important(marginBottom(px(16))),
      media("(max-width: 640px)", list{important(marginBottom(zero))}),
    })
    let usernameLink = style(list{
      fontSize(px(24)),
      color(Colors.charcoal),
      textDecoration(none),
      media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
    })
    let unfollowLink = style(list{
      textDecoration(none),
      media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
    })
    let items = style(list{display(flexBox), flexWrap(wrap), marginRight(px(-16))})
    let listLink = style(list{
      position(absolute),
      top(px(8)),
      left(px(8)),
      display(flexBox),
      alignItems(center),
      color(Colors.gray),
      textDecoration(none),
      media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
    })
    let itemMatch = style(list{boxShadow(Shadow.box(~spread=px(2), Colors.green))})
  }

  let getNumCards = viewportWidth =>
    if viewportWidth >= 1040 {
      10
    } else if viewportWidth >= 860 {
      8
    } else {
      6
    }

  module ItemCard = {
    @react.component
    let make = (~itemId, ~variant, ~userItem: User.item, ~username) => {
      let viewerItem = UserStore.useItem(~itemId, ~variation=variant)
      let isMatch = switch viewerItem {
      | Some(viewerItem) =>
        switch (userItem.status, viewerItem.status) {
        | (ForTrade, Wishlist)
        | (CanCraft, Wishlist)
        | (CatalogOnly, Wishlist)
        | (Wishlist, ForTrade)
        | (Wishlist, CanCraft)
        | (Wishlist, CatalogOnly) => true
        | _ => false
        }
      | None => false
      }
      <UserItemCard
        itemId
        variation=variant
        userItem
        list={switch userItem.status {
        | CatalogOnly
        | ForTrade =>
          ForTrade
        | CanCraft => CanCraft
        | Wishlist => Wishlist
        }}
        editable=false
        showRecipe=false
        customTopLeft={<Link
          path={"/u/" ++
          (username ++
          ("/" ++
          (ViewingList.viewingListToUrl(
            switch userItem.status {
            | Wishlist => Wishlist
            | ForTrade => ForTrade
            | CanCraft => CanCraft
            | CatalogOnly => Catalog
            },
          ) ++
          "?s=tu")))}
          className=Styles.listLink>
          {React.string(User.itemStatusToString(userItem.status))}
        </Link>}
        className={Cn.ifTrue(Styles.itemMatch, isMatch)}
      />
    }
  }

  @react.component
  let make = (~followee) => {
    let viewportWidth = Utils.useViewportWidth()
    let numCards = getNumCards(viewportWidth)
    let (unfollowed, setUnfollowed) = React.useState(() => false)

    <div className=Styles.root>
      <div
        className={Cn.make(list{
          Styles.usernameRow,
          Cn.ifTrue(Styles.usernameRowUnfollowed, unfollowed),
        })}>
        <Link path={"/u/" ++ followee.username} className=Styles.usernameLink>
          {React.string(followee.username)}
        </Link>
        {!unfollowed
          ? <a
              href="#"
              onClick={e => {
                ReactEvent.Mouse.preventDefault(e)
                %Repromise({
                  let response = UserStore.unfollowUser(~userId=followee.id)
                  switch response {
                  | Ok() =>
                    setUnfollowed(_ => true)
                    Analytics.Amplitude.logEventWithProperties(
                      ~eventName="Friend Unfollowed",
                      ~eventProperties={"followeeId": followee.id},
                    )
                  | Error(_) => ()
                  }
                  Promise.resolved()
                })->ignore
              }}
              className=Styles.unfollowLink>
              {React.string("Unfollow")}
            </a>
          : <span> {React.string("Unfollowed")} </span>}
      </div>
      {!unfollowed
        ? <div className=Styles.items>
            {followee.items
            ->Js.Array.slice(~start=0, ~end_=numCards - 1)
            ->Js.Array.map(((itemId, variant, userItem)) =>
              <ItemCard
                itemId
                variant
                userItem
                username=followee.username
                key={string_of_int(itemId) ++ string_of_int(variant)}
              />
            )
            ->React.array}
            {Js.Array.length(followee.items) > 0
              ? <Link
                  path={"/u/" ++ (followee.username ++ "?s=tu")}
                  className={Cn.make(list{
                    UserItemCard.Styles.card,
                    UserProfileBrowser.Styles.cardSeeAll,
                  })}>
                  {React.string("Go to profile")}
                  <span
                    className={Cn.make(list{
                      UserProfileBrowser.Styles.sectionTitleLinkIcon,
                      UserProfileBrowser.Styles.cardSeeAllLinkIcon,
                    })}
                  />
                </Link>
              : <div> {React.string(followee.username ++ (" has no items! " ++ `😞`))} </div>}
          </div>
        : React.null}
    </div>
  }
}

module WithViewer = {
  module Styles = {
    open Css
    let root = style(list{})
    let emptyFeed = style(list{
      backgroundColor(hex("ffffffc0")),
      boxSizing(borderBox),
      lineHeight(px(20)),
      margin3(~top=zero, ~bottom=px(48), ~h=auto),
      maxWidth(px(512)),
      padding2(~v=px(32), ~h=px(24)),
      borderRadius(px(8)),
      textAlign(center),
      media(
        "(max-width: 512px)",
        list{
          borderRadius(zero),
          padding2(~v=px(24), ~h=px(16)),
          marginBottom(zero),
          borderBottom(px(1), solid, Colors.faintGray),
        },
      ),
    })
  }

  @react.component
  let make = (~me: User.t) => {
    let (feed: option<array<followee>>, setFeed) = React.useState(() => None)

    React.useEffect0(() => {
      %Repromise({
        let feed = fetchFeed()
        setFeed(_ => Some(feed))
        Analytics.Amplitude.logEventWithProperties(
          ~eventName="Friend Page Viewed",
          ~eventProperties={"numFollowees": Js.Array.length(feed)},
        )
        Promise.resolved()
      })->ignore
      None
    })

    <div className=Styles.root>
      <PageTitle title="My Friends" />
      {switch feed {
      | Some(feed) =>
        Js.Array.length(feed) > 0
          ? <div>
              {feed
              ->Js.Array.map(followee => <Followee followee key=followee.id />)
              ->React.array}
            </div>
          : <div className=Styles.emptyFeed>
              {React.string("Visit people's profiles to add them to your friends page!")}
            </div>
      | None => React.null
      }}
    </div>
  }
}

@react.component
let make = () => {
  let me = UserStore.useMe()

  switch me {
  | Some(me) => <WithViewer me />
  | None => React.null
  }
}
