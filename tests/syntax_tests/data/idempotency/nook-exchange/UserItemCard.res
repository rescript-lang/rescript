module Styles = {
  open Css
  let metaIcons = style(list{position(absolute), top(px(5)), left(px(7)), display(flexBox)})
  let metaIcon = style(list{opacity(0.), transition(~duration=200, "all")})
  let topRightIcon = style(list{
    position(absolute),
    top(px(5)),
    right(px(8)),
    fontSize(px(13)),
    boxSizing(borderBox),
    cursor(#default),
    padding2(~v=zero, ~h=px(2)),
    height(px(24)),
    display(flexBox),
    alignItems(center),
    opacity(0.8),
    transition(~duration=200, "all"),
    hover(list{opacity(1.)}),
  })
  let ellipsisButton = style(list{position(absolute), top(px(8)), right(px(8))})
  let catalogStatusButton = style(list{
    fontSize(px(13)),
    opacity(0.5),
    cursor(#default),
    padding2(~v=zero, ~h=px(1)),
    transition(~duration=200, "all"),
  })
  let nameLink = style(list{
    color(Colors.charcoal),
    textDecoration(none),
    media("(hover: hover)", list{hover(list{textDecoration(underline)})}),
  })
  let card = style(list{
    backgroundColor(hex("fffffff0")),
    borderRadius(px(8)),
    display(flexBox),
    flexDirection(column),
    alignItems(center),
    marginRight(px(16)),
    marginBottom(px(16)),
    padding3(~top=px(24), ~bottom=px(8), ~h=px(8)),
    position(relative),
    boxSizing(borderBox),
    width({
      open Calc
      pct(50.) - px(16)
    }),
    transition(~duration=200, "all"),
    media("(min-width: 660px)", list{width(px(176))}),
    media(
      "(hover: hover)",
      list{
        children(list{transition(~duration=200, "opacity")}),
        hover(list{
          selector("& ." ++ metaIcon, list{opacity(1.)}),
          selector("& ." ++ topRightIcon, list{opacity(1.)}),
          selector("& ." ++ catalogStatusButton, list{opacity(1.)}),
          selector("& ." ++ ItemImage.Styles.variantButton, list{opacity(0.5)}),
        }),
      },
    ),
  })
  let cardOnCatalogPage = style(list{paddingBottom(px(16))})
  let cardWithoutTopRow = style(list{paddingTop(px(8))})
  let itemImage = style(list{marginLeft(px(-8)), marginRight(px(-8))})
  let name = style(list{fontSize(px(16)), marginBottom(px(4)), textAlign(center)})
  let userItemNote = style(list{marginTop(px(8))})
  let userNote = style(list{
    borderTop(px(1), solid, hex("f0f0f0")),
    unsafe("alignSelf", "stretch"),
    lineHeight(px(18)),
    marginTop(px(4)),
    padding3(~top=px(8), ~bottom=zero, ~h=px(4)),
  })
  let removeButton = style(list{
    position(absolute),
    top(px(10)),
    right(px(10)),
    selector("." ++ (card ++ ":hover &"), list{opacity(0.8)}),
  })
  let recipe = style(list{marginTop(px(6)), textAlign(center), fontSize(px(12))})
  let cardViewerMatch = style(list{boxShadow(Shadow.box(~spread=px(2), Colors.green))})
  let cardHasQuicklist = style(list{
    boxShadow(none),
    selector("& ." ++ metaIcons, list{display(none)}),
    media(
      "(hover: hover)",
      list{children(list{opacity(0.5)}), hover(list{children(list{opacity(1.)})})},
    ),
  })
  let cardQuicklistSelected = style(list{
    important(boxShadow(Shadow.box(~spread=px(2), Colors.green))),
    children(list{opacity(1.)}),
  })
  let quicklistButton = style(list{position(absolute), top(px(6)), left(px(6))})
}

module StarIcon = {
  module Styles = {
    open Css
    let icon = style(list{
      padding2(~v=zero, ~h=px(2)),
      display(flexBox),
      alignItems(center),
      height(px(24)),
      fontSize(px(12)),
      cursor(#default),
    })
  }

  @react.component
  let make = () => <div className=Styles.icon> {React.string(j`⭐️`)} </div>
}

@react.component
let make = (
  ~itemId,
  ~variation,
  ~userItem: option<User.item>=?,
  ~list: option<ViewingList.t>=?,
  ~editable,
  ~showRecipe,
  ~showMetaIcons=true,
  ~onCatalogPage=false,
  ~customTopLeft=?,
  ~className=?,
  (),
) => {
  let item = Item.getItem(~itemId)
  let viewerItem = UserStore.useItem(~itemId, ~variation)
  let hasQuicklist = QuicklistStore.useHasQuicklist()
  let isInQuicklist = QuicklistStore.useItemState(~itemId, ~variant=variation)

  let isViewerMatch = switch viewerItem {
  | Some(viewerItem) =>
    switch (list, viewerItem.status) {
    | (Some(ForTrade), Wishlist)
    | (Some(CanCraft), Wishlist)
    | (Some(Wishlist), ForTrade)
    | (Some(Wishlist), CanCraft) => true
    | (Some(Catalog), Wishlist)
    | (Some(Wishlist), CatalogOnly) =>
      item.orderable
    | _ => false
    }
  | None => false
  }

  <div
    className={Cn.make(list{
      Styles.card,
      Cn.ifTrue(Styles.cardWithoutTopRow, !showMetaIcons),
      Cn.ifTrue(Styles.cardOnCatalogPage, onCatalogPage),
      Cn.ifTrue(Styles.cardHasQuicklist, hasQuicklist),
      Cn.ifTrue(Styles.cardQuicklistSelected, isInQuicklist),
      Cn.ifTrue(Styles.cardViewerMatch, isViewerMatch),
      Cn.unpack(className),
    })}>
    <div className=ItemCard.Styles.body>
      <ItemImage
        item
        variant=variation
        forceTooltip=true
        narrow=true
        className={Cn.make(list{ItemCard.Styles.itemImage, Styles.itemImage})}
      />
      <div className=Styles.name>
        <Link
          path={Utils.getItemDetailUrl(
            ~itemId=item.id,
            ~variant=variation != 0 ? Some(variation) : None,
          )}
          className=Styles.nameLink>
          {React.string(Item.getName(item))}
        </Link>
      </div>
      {switch (showRecipe, item.recipe) {
      | (true, Some(recipe)) =>
        <div className=Styles.recipe>
          {recipe
          ->Belt.Array.map(((itemId, quantity)) =>
            <div key=itemId> {React.string(itemId ++ (" x " ++ string_of_int(quantity)))} </div>
          )
          ->React.array}
        </div>
      | _ => React.null
      }}
    </div>
    {showMetaIcons
      ? switch customTopLeft {
        | None =>
          <div className=Styles.metaIcons>
            {if Belt.Option.flatMap(userItem, userItem => userItem.priorityTimestamp) !== None {
              <StarIcon />
            } else {
              React.null
            }}
            {switch (onCatalogPage, list, Belt.Option.map(userItem, userItem => userItem.status)) {
            | (true, _, Some(CanCraft) as userItemStatus)
            | (true, _, Some(ForTrade) as userItemStatus)
            | (false, Some(Catalog), Some(CanCraft) as userItemStatus)
            | (false, Some(Catalog), Some(ForTrade) as userItemStatus) =>
              <ReactAtmosphere.Tooltip
                text={React.string(userItemStatus == Some(ForTrade) ? "For Trade" : "Can Craft")}
                options={Obj.magic({"modifiers": None})}>
                {({onMouseEnter, onMouseLeave, onFocus, onBlur, ref}) =>
                  <div
                    onMouseEnter
                    onMouseLeave
                    onFocus
                    onBlur
                    className=Styles.catalogStatusButton
                    ref={ReactDOMRe.Ref.domRef(ref)}>
                    {React.string(userItemStatus == Some(ForTrade) ? `🤝` : `🔨`)}
                  </div>}
              </ReactAtmosphere.Tooltip>
            | _ => React.null
            }}
            {switch (onCatalogPage, item.recipe) {
            | (false, Some(recipe)) => <ItemCard.RecipeIcon recipe className=Styles.metaIcon />
            | _ => React.null
            }}
            {if !onCatalogPage && item.orderable {
              <ItemCard.OrderableIcon className=Styles.metaIcon />
            } else {
              React.null
            }}
          </div>
        | Some(customTopLeft) => customTopLeft
        }
      : React.null}
    {editable
      ? <>
          {switch userItem {
          | Some(userItem) =>
            <UserItemNote
              itemId=item.id
              variation
              userItem
              className=Styles.userItemNote
              key={string_of_int(variation)}
            />
          | _ => React.null
          }}
          <UserItemEllipsisButton
            item userItem={Belt.Option.getExn(userItem)} variation className=Styles.ellipsisButton
          />
        </>
      : <>
          {switch userItem {
          | Some(userItem) =>
            if userItem.note->Js.String.length > 0 {
              <div className=Styles.userNote> {Emoji.parseText(userItem.note)} </div>
            } else {
              React.null
            }
          | None => React.null
          }}
          {switch viewerItem {
          | Some(viewerItem) =>
            switch (list, viewerItem.status, item.orderable) {
            | (Some(Catalog), Wishlist, true)
            | (Some(ForTrade), Wishlist, _)
            | (Some(CanCraft), Wishlist, _)
            | (Some(Wishlist), ForTrade, _)
            | (Some(Wishlist), CanCraft, _)
            | (Some(Wishlist), CatalogOnly, true) =>
              <ReactAtmosphere.Tooltip
                text={React.string("In your " ++ User.itemStatusToString(viewerItem.status))}>
                {({onMouseEnter, onMouseLeave, ref}) =>
                  <div
                    onMouseEnter
                    onMouseLeave
                    className={Cn.make(list{Styles.topRightIcon})}
                    ref={ReactDOMRe.Ref.domRef(ref)}>
                    {React.string(User.itemStatusToEmoji(viewerItem.status))}
                  </div>}
              </ReactAtmosphere.Tooltip>
            | _ => React.null
            }
          | None => React.null
          }}
        </>}
    {hasQuicklist
      ? <QuicklistButton
          itemId variant=variation selected=isInQuicklist className=Styles.quicklistButton
        />
      : React.null}
  </div>
}
