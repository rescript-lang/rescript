open Belt

module Styles = {
  open Css
  let statusButton = style(list{
    backgroundColor(transparent),
    color(hex("3aa563c0")),
    borderWidth(zero),
    borderRadius(px(4)),
    fontSize(px(12)),
    marginRight(px(8)),
    outlineStyle(none),
    padding3(~top=px(5), ~bottom=px(3), ~h=px(4)),
    transition(~duration=200, "all"),
    cursor(pointer),
    whiteSpace(nowrap),
    lastChild(list{marginRight(zero)}),
    hover(list{important(backgroundColor(Colors.green)), important(color(Colors.white))}),
    media("(max-width: 430px)", list{fontSize(px(14))}),
  })
  let catalogCheckbox = style(list{
    backgroundColor(Colors.white),
    height(px(20)),
    width(px(20)),
    borderRadius(px(3)),
    border(px(2), solid, hex("c0c0c0")),
    padding(zero),
    transition(~duration=200, "all"),
    margin(zero),
    cursor(pointer),
    outlineStyle(none),
    opacity(0.),
    transition(~duration=200, "all"),
    media("(hover: none)", list{opacity(0.5)}),
    hover(list{borderColor(hex("808080"))}),
  })
  @module("./assets/check.png") external checkImage: string = "default"
  let catalogCheckboxChecked = style(list{
    borderColor(Colors.white),
    backgroundImage(url(checkImage)),
    backgroundSize(size(px(16), px(16))),
    backgroundRepeat(noRepeat),
    backgroundPosition(center),
    opacity(0.5),
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
    marginRight(px(32)),
    marginBottom(px(32)),
    padding3(~top=px(24), ~bottom=px(8), ~h=px(8)),
    position(relative),
    boxSizing(borderBox),
    width(px(256)),
    transition(~duration=200, "all"),
    hover(list{
      selector("& ." ++ statusButton, list{backgroundColor(hex("3aa56320")), color(Colors.green)}),
      selector(
        "& ." ++ catalogCheckbox,
        list{opacity(1.), media("(hover: none)", list{borderColor(hex("c0c0c0"))})},
      ),
      selector("& ." ++ ItemImage.Styles.variantButton, list{opacity(0.5)}),
    }),
    media(
      "(max-width: 600px)",
      list{
        marginRight(px(16)),
        marginBottom(px(16)),
        width({
          open Calc
          pct(50.) - px(16)
        }),
      },
    ),
    media("(max-width: 430px)", list{width(pct(100.)), paddingTop(px(16))}),
  })
  let cardSelected = style(list{backgroundColor(hex("ffffff"))})
  let body = style(list{
    flexGrow(1.),
    display(flexBox),
    flexDirection(column),
    alignItems(center),
    width(pct(100.)),
  })
  let name = style(list{
    fontSize(px(20)),
    marginBottom(px(8)),
    padding2(~v=zero, ~h=px(16)),
    textAlign(center),
  })
  let itemImage = style(list{unsafe("alignSelf", "stretch"), marginBottom(px(8))})
  let variation = style(list{
    display(flexBox),
    flexWrap(wrap),
    justifyContent(center),
    marginBottom(px(8)),
  })
  let variationImageWrapper = style(list{position(relative)})
  let variationImage = style(list{
    display(block),
    cursor(pointer),
    width(px(32)),
    height(px(32)),
    borderRadius(px(4)),
    hover(list{backgroundColor(hex("00000010"))}),
  })
  let variationImageSelected = style(list{backgroundColor(hex("3aa56320"))})
  @module("./assets/check_small.png")
  external checkSmall: string = "default"
  let variationImageCheck = style(list{
    position(absolute),
    backgroundImage(url(checkSmall)),
    width(px(12)),
    height(px(12)),
    backgroundSize(cover),
    top(px(-2)),
    right(px(-2)),
  })
  let metaIcons = style(list{position(absolute), top(px(8)), left(px(6))})
  let topRightIcons = style(list{position(absolute), top(px(10)), right(px(10))})
  let bottomBar = style(list{fontSize(px(12))})
  let bottomBarStatus = style(list{alignSelf(flexStart), paddingTop(px(4))})
  let bottomBarEmoji = style(list{display(inlineBlock), marginRight(px(8))})
  let statusButtons = style(list{display(flexBox), alignItems(center)})
  let removeButton = style(list{
    position(absolute),
    bottom(px(10)),
    right(px(10)),
    selector("." ++ (card ++ ":hover &"), list{opacity(0.8)}),
  })
  let ellipsisButton = style(list{
    position(absolute),
    bottom(px(8)),
    right(px(8)),
    selector("." ++ (card ++ ":hover &"), list{opacity(0.8)}),
  })

  let cardHasQuicklist = style(list{
    selector("& ." ++ metaIcons, list{display(none)}),
    selector("& ." ++ bottomBar, list{display(none)}),
    selector("& ." ++ catalogCheckbox, list{display(none)}),
    media(
      "(hover: hover)",
      list{children(list{opacity(0.5)}), hover(list{children(list{opacity(1.)})})},
    ),
  })
  let cardQuicklistSelected = style(list{
    boxShadow(Shadow.box(~spread=px(2), Colors.green)),
    children(list{opacity(1.)}),
  })
  let quicklistButton = style(list{position(absolute), top(px(6)), left(px(6))})
}

module MetaIconStyles = {
  open Css
  let icon = style(list{display(block), width(px(24)), height(px(24))})
  let iconClickable = style(list{cursor(pointer)})
  let layer = style(list{padding2(~v=px(4), ~h=px(4))})
  let clickNote = style(list{
    borderTop(px(1), solid, hex("ffffff40")),
    marginTop(px(6)),
    marginLeft(px(-8)),
    marginRight(px(-8)),
    marginBottom(px(-4)),
    paddingTop(px(6)),
    paddingLeft(px(8)),
    paddingRight(px(8)),
    paddingBottom(px(4)),
  })
}

module RecipeIcon = {
  module RecipeIconStyles = {
    open Css
    let tooltip = style(list{
      backgroundColor(Colors.darkLayerBackground),
      borderRadius(px(4)),
      color(Colors.white),
      fontSize(px(14)),
      padding3(~top=px(9), ~bottom=px(7), ~h=px(14)),
      position(relative),
      whiteSpace(#preLine),
      Colors.darkLayerShadow,
    })
  }

  module RecipeLayer = {
    @react.component
    let make = (~recipe, ~isRecipe, ~onClickAlternate) =>
      <div className={Cn.make(list{MetaIconStyles.layer, RecipeIconStyles.tooltip})}>
        {recipe
        ->Array.map(((itemId, quantity)) =>
          <div key=itemId>
            {React.string(Item.getMaterialName(itemId) ++ (j` × ` ++ string_of_int(quantity)))}
          </div>
        )
        ->React.array}
        {switch (isRecipe, onClickAlternate) {
        | (Some(isRecipe), Some(onClickAlternate)) =>
          <div
            onClick={_ => onClickAlternate()}
            onTouchStart={_ => onClickAlternate()}
            className=MetaIconStyles.clickNote>
            {React.string("Click to see " ++ (isRecipe ? "item" : "DIY"))}
          </div>
        | _ => React.null
        }}
      </div>
  }

  @module("./assets/recipe_icon.png")
  external recipeIcon: string = "default"

  @react.component
  let make = (~recipe: Item.recipe, ~isRecipe=?, ~onClick=?, ~className=?, ()) => {
    let (showLayer, setShowLayer) = React.useState(() => false)
    let iconRef = React.useRef(Js.Nullable.null)
    let isMountedRef = React.useRef(true)
    React.useEffect0(() => Some(() => React.Ref.setCurrent(isMountedRef, false)))
    <>
      <img
        src=recipeIcon
        className={Cn.make(list{
          MetaIconStyles.icon,
          Cn.ifTrue(MetaIconStyles.iconClickable, onClick !== None),
          Cn.unpack(className),
        })}
        onMouseEnter={_ => Js.Global.setTimeout(() => setShowLayer(_ => true), 10)->ignore}
        onMouseLeave={_ => Js.Global.setTimeout(() =>
            if React.Ref.current(isMountedRef) {
              setShowLayer(_ => false)
            }
          , 10)->ignore}
        onClick=?{Belt.Option.map(onClick, (onClick, e) =>
          if Utils.browserSupportsHover {
            onClick()
          }
        )}
        ref={ReactDOMRe.Ref.domRef(iconRef)}
      />
      {showLayer
        ? <ReactAtmosphere.PopperLayer
            reference=iconRef
            render={_ => <RecipeLayer recipe isRecipe onClickAlternate=onClick />}
            options={Obj.magic({
              "placement": "bottom-start",
              "modifiers": Some([
                {
                  "name": "offset",
                  "options": {
                    "offset": [0, 2],
                  },
                },
              ]),
            })}
          />
        : React.null}
    </>
  }
}

module OrderableIcon = {
  module OrderableLayer = {
    @react.component
    let make = () =>
      <div className=MetaIconStyles.layer>
        <div> {React.string("Orderable from catalog")} </div>
        <div> {React.string("Touch tradeable")} </div>
      </div>
  }

  @module("./assets/shop_icon.png")
  external orderableIcon: string = "default"

  @react.component
  let make = (~className=?, ()) =>
    <ReactAtmosphere.Tooltip
      text={<OrderableLayer />} options={Obj.magic({"placement": "bottom-start"})}>
      {({onMouseEnter, onMouseLeave, ref}) =>
        <img
          src=orderableIcon
          className={Cn.make(list{MetaIconStyles.icon, Cn.unpack(className)})}
          onMouseEnter
          onMouseLeave
          ref={ReactDOMRe.Ref.domRef(ref)}
        />}
    </ReactAtmosphere.Tooltip>
}

module StatusButtons = {
  let renderStatusButton = (~itemId, ~variation, ~status: User.itemStatus, ~showLogin=?, ()) =>
    <button
      onClick={_ =>
        if UserStore.isLoggedIn() {
          UserStore.setItemStatus(~itemId, ~variation, ~status)
        } else {
          Option.map(showLogin, showLogin => showLogin())->ignore
        }}
      className=Styles.statusButton
      title={switch status {
      | Wishlist => "Add to Wishlist"
      | ForTrade => "Add to For Trade list"
      | CanCraft => "Add to Can Craft list"
      | CatalogOnly => throw(Constants.Uhoh)
      }}>
      {React.string(
        switch status {
        | Wishlist => "+ Wishlist"
        | ForTrade => "+ For Trade"
        | CanCraft => "+ Can Craft"
        | CatalogOnly => throw(Constants.Uhoh)
        },
      )}
    </button>

  @react.component
  let make = (~item: Item.t, ~variant, ~showLogin=?, ~className=?, ()) =>
    <div className={Cn.make(list{Styles.statusButtons, Cn.unpack(className)})}>
      {renderStatusButton(~itemId=item.id, ~variation=variant, ~status=Wishlist, ~showLogin?, ())}
      {renderStatusButton(~itemId=item.id, ~variation=variant, ~status=ForTrade, ~showLogin?, ())}
      {switch item.type_ {
      | Item(Some(_)) => true
      | _ => false
      }
        ? renderStatusButton(~itemId=item.id, ~variation=variant, ~status=CanCraft, ~showLogin?, ())
        : React.null}
    </div>
}

module CatalogCheckbox = {
  @react.component
  let make = (~item: Item.t, ~variant, ~userItem: option<User.item>, ~className=?, ()) =>
    <ReactAtmosphere.Tooltip
      options={
        placement: Some("top"),
        modifiers: Some([
          {
            "name": "offset",
            "options": {
              "offset": [0, 4],
            },
          },
        ]),
      }
      text={React.string(
        switch userItem->Option.map(userItem => userItem.status) {
        | None => "Not in catalog"
        | Some(Wishlist) => "Move to catalog from Wishlist"
        | Some(ForTrade)
        | Some(CanCraft)
        | Some(CatalogOnly) => "In your catalog"
        },
      )}>
      {({onMouseEnter, onMouseLeave, ref}) =>
        <button
          className={Cn.make(list{
            Styles.catalogCheckbox,
            Cn.ifTrue(
              Styles.catalogCheckboxChecked,
              switch userItem {
              | Some(userItem) => userItem.status !== Wishlist
              | None => false
              },
            ),
            Cn.unpack(className),
          })}
          onClick={e => {
            let status = switch Option.map(userItem, userItem => userItem.status) {
            | None
            | Some(Wishlist) =>
              Some(User.CatalogOnly)
            | Some(CanCraft)
            | Some(ForTrade)
            | Some(CatalogOnly) =>
              None
            }
            switch status {
            | Some(status) =>
              let updateItem = () =>
                UserStore.setItemStatus(~itemId=item.id, ~variation=variant, ~status)
              if Option.map(userItem, userItem => userItem.status) == Some(Wishlist) {
                WishlistToCatalog.confirm(~onConfirm=updateItem)
              } else {
                updateItem()
              }
            | None =>
              if Belt.Option.getExn(userItem).status == CatalogOnly {
                UserStore.removeItem(~itemId=item.id, ~variation=variant)
              } else {
                DeleteFromCatalog.confirm(~onConfirm=() =>
                  UserStore.removeItem(~itemId=item.id, ~variation=variant)
                )
              }
            }
          }}
          onMouseEnter
          onMouseLeave
          ref={ReactDOMRe.Ref.domRef(ref)}
        />}
    </ReactAtmosphere.Tooltip>
}

module VariantImage = {
  @react.component
  let make = (~item: Item.t, ~variant, ~selected) => {
    let userItem = UserStore.useItem(~itemId=item.id, ~variation=variant)
    <div className=Styles.variationImageWrapper>
      <img
        src={Item.getImageUrl(~item, ~variant)}
        className={Cn.make(list{
          Styles.variationImage,
          Cn.ifTrue(Styles.variationImageSelected, selected),
        })}
      />
      {switch userItem->Belt.Option.map(userItem => userItem.status) {
      | Some(ForTrade)
      | Some(CanCraft)
      | Some(CatalogOnly) =>
        <span className=Styles.variationImageCheck />
      | _ => React.null
      }}
    </div>
  }
}

@react.component
let make = (~item: Item.t, ~isLoggedIn, ~showLogin) => {
  let (showRecipeAlternate, setShowRecipeAlternate) = React.useState(() => false)
  let item = if showRecipeAlternate {
    switch item.type_ {
    | Recipe(itemId) => Item.getItem(~itemId)
    | Item(Some(recipeId)) => Item.getItem(~itemId=recipeId)
    | Item(None) => throw(Constants.Uhoh)
    }
  } else {
    item
  }

  let (variation, setVariation) = React.useState(() => 0)
  let numVariations = Item.getNumVariations(~item)
  if variation > numVariations {
    setVariation(_ => 0)
  }
  let variation = Js.Math.min_int(variation, numVariations - 1)
  let userItem = UserStore.useItem(~itemId=item.id, ~variation)

  let hasQuicklist = QuicklistStore.useHasQuicklist()
  let isInQuicklist = QuicklistStore.useItemState(~itemId=item.id, ~variant=variation)

  <div
    className={Cn.make(list{
      Styles.card,
      Cn.ifSome(Styles.cardSelected, userItem),
      Cn.ifTrue(Styles.cardHasQuicklist, hasQuicklist),
      Cn.ifTrue(Styles.cardQuicklistSelected, isInQuicklist),
    })}>
    <div className=Styles.body>
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
      <ItemImage item variant=variation className=Styles.itemImage key={string_of_int(item.id)} />
      {
        let collapsedVariants = Item.getCollapsedVariants(~item)
        if Js.Array.length(collapsedVariants) === 1 {
          React.null
        } else {
          <div className={Cn.make(list{Styles.variation})}>
            {collapsedVariants
            ->Belt.Array.map(v => {
              let image = <VariantImage item variant=v selected={v == variation} />
              switch Item.getVariantName(~item, ~variant=v, ~hidePattern=true, ()) {
              | Some(variantName) =>
                <ReactAtmosphere.Tooltip text={React.string(variantName)} key={string_of_int(v)}>
                  {({onMouseEnter, onMouseLeave, onFocus, onBlur, ref}) =>
                    <div
                      onClick={_ => setVariation(_ => v)}
                      onMouseEnter
                      onMouseLeave
                      onFocus
                      onBlur
                      ref={ReactDOMRe.Ref.domRef(ref)}>
                      image
                    </div>}
                </ReactAtmosphere.Tooltip>
              | None =>
                <div onClick={_ => setVariation(_ => v)} key={string_of_int(v)}> image </div>
              }
            })
            ->React.array}
          </div>
        }
      }
      <div className=Styles.metaIcons>
        {switch item.recipe {
        | Some(recipe) =>
          <RecipeIcon
            recipe
            isRecipe={Item.isRecipe(~item)}
            onClick={_ => setShowRecipeAlternate(show => !show)}
            key={string_of_int(item.id)}
          />
        | None => React.null
        }}
        {if item.orderable {
          <OrderableIcon />
        } else {
          React.null
        }}
      </div>
      {isLoggedIn
        ? <div className=Styles.topRightIcons>
            <CatalogCheckbox item variant=variation userItem />
          </div>
        : React.null}
    </div>
    {
      let userItemStatus = userItem->Option.map(userItem => userItem.status)
      <>
        {switch userItem {
        | Some(userItem) => <>
            <UserItemNote itemId=item.id variation userItem key={string_of_int(variation)} />
            {switch userItemStatus {
            | Some(Wishlist)
            | Some(ForTrade)
            | Some(CanCraft) => <>
                <div className={Cn.make(list{Styles.bottomBar, Styles.bottomBarStatus})}>
                  <span className=Styles.bottomBarEmoji>
                    {React.string(User.itemStatusToEmoji(userItem.status))}
                  </span>
                  {React.string(
                    switch userItem.status {
                    | Wishlist => `In your Wishlist`
                    | ForTrade => `In your For Trade list`
                    | CanCraft => `In your Can Craft list`
                    | _ => throw(Constants.Uhoh)
                    },
                  )}
                </div>
                <UserItemEllipsisButton item userItem variation className=Styles.ellipsisButton />
              </>
            | _ => React.null
            }}
          </>
        | None => React.null
        }}
        {switch userItemStatus {
        | Some(CatalogOnly)
        | None =>
          <StatusButtons item variant=variation showLogin className=Styles.bottomBar />
        | _ => React.null
        }}
      </>
    }
    {hasQuicklist
      ? <QuicklistButton
          itemId=item.id variant=variation selected=isInQuicklist className=Styles.quicklistButton
        />
      : React.null}
  </div>
}
